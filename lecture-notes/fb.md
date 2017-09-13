% FP @ FB

# Bad actors on the internet

<img src="anakin.jpg"></img>

No, not that kind.


# Bad actors on the internet

<img src="hackers.jpg"></img>

Fighting spam is an arms race.


# Bad actors on the internet

<img src="0akley.jpg"></img>

Fighting spam is an arms race.


# Bad actors on the internet

The attack landscape is constantly evolving:

* Fake accounts, account cloning

* Social engineering

* Hacked accounts posting spam

* Malicious browser extensions

...and many more


# Why does spam appear to be "solved"?

* Massive automation

* Fast engineering triage of breaking attacks

* Continuous deployment of code to respond


# Fighting spam at Facebook

**Sigma** is a rule execution engine

Every interaction on FB has associated rules

* Posting a status update

* Liking a post

* Sending a message

Sigma evaluates each interaction to **identify** and **block**
malicious acts


# An example policy

> "A user who is less than a week old posts a photo tagging >= 5
> non-friends"


# History

> "A user who is less than a week old posts a photo tagging >= 5
> non-friends"

The old Sigma rule language, FXL:

	If (AgeInHours(Account) < 168 &&
		Length(Difference(Tagged(Post),
		                  Friends(Account))) >= 5)
	Then [BlockAction, LogRequest]
	Else []


# History: FXL

Pluses:

* Pure functions

* Batched data fetches

Minuses became an increasing drag:

* Home-grown language

* Simple, non-extensible type system

* Interpreted runtime
  * Too much C++ written


# Purely functional and strongly typed

* Policies can't inadvertently interact with each other

* Rule code can't crash Sigma

* Policies become easy to test in isolation


# Automatically batch and overlap data fetches

Policies fetch data from **many** other systems

* Must use concurrency wherever possible

Concurrency must be **implicit**

* Engineers writing policies can concentrate on **fighting spam**
* Avoids clutter from efficiency-related details
* Code is easier to understand and modify

# Fast turnaround

Push code to production in **minutes**

* Allows new or updated policies to go live quickly

Support for interactive development

* Spam fighters need to experiment, test code, and **get feedback
  quickly**


# Our desired endpoint

* `x` and `y` are Facebook users

* We want to compute the number of friends that `x` and `y` have in
  common

Our ideal for expressiveness:

~~~~ {.haskell}
length (intersect (friendsOf x) (friendsOf y))
~~~~


# Some requirements

We have many varied data sources

* Synchronous *vs* async
* Batch *vs* one-shot requests
* Pooled *vs* per-request connections

Must minimise network roundtrips

* Overlap accesses to different services

* One "multi-get" access when a single service is used

* Cache repeated requests for same/similar data

**Need to abstract all this away**



# A terrible hack

~~~~ {.haskell}
{-# LANGUAGE NoImplicitPrelude #-}
module HackyPrelude where

length :: IO [a] -> IO Int
intersect :: Eq a => IO [a] -> IO [a] -> IO [a]
friendsOf :: UserID -> IO [UserID]
~~~~

This approach "works", but it's awful.


# What's wrong?

Everything gets lifted into `IO`:

~~~~ {.haskell}
length :: IO [a] -> IO Int
~~~~

No more pure, safe code :-(

No concurrency or batching when we execute two `friendsOf` actions:

~~~~ {.haskell}
length (intersect (friendsOf x) (friendsOf y))
~~~~


# Explicit concurrency

What if we use `MVar` to express this?

~~~~ {.haskell}
length (intersect (friendsOf x) (friendsOf y))
~~~~

Can't see what's happening for all the `MVar` machinery!

~~~~ {.haskell}
do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  forkIO (friendsOf x >>= putMVar m1)
  forkIO (friendsOf y >>= putMVar m2)
  fx <- takeMVar m1
  fy <- takeMVar m2
  return (length (intersect fx fy))
~~~~


# The central problem

We need something where:

* We can reorder an expression to optimize data fetching

* No side effects (other than fetching)

* No clutter from concurrency machinery


# What do we want?

1. **Inspect** a rule to find all data fetches, *without* executing them

2. **Re-organize** fetches to use batching, concurrency, and caching

3. **Execute** fetches, wait for results

4. **Resume** execution once results come in,<br>inspecting and
   re-organizing the **next round** of fetches


# Static analysis

We want to be able to inspect the *structure* of code **without**
executing it


# Monads and static analysis

Let's think briefly of a monad `m` as the "structure" of a
computation.

Look at the flipped version of bind:

~~~~ {.haskell}
(=<<) :: Monad m => (a -> m b) -> m a -> m b
~~~~

It transforms an `m a` into an `m b`.

To do so, it takes its instructions from `a -> m b`.

That function can use `a` to **decide** what `m b` it returns.

Thus we can't analyse this statically (at least not easily):

* Won't know what structure (`m b`) will be returned until the code is
  run.


# Functors, applicatives, and static analysis

Compare this:

~~~~ {.haskell}
(=<<) :: Monad m       => (a -> m b) -> m a -> m b
~~~~

With these rather similar type signatures:

~~~~ {.haskell}
(<$>) :: Functor f     =>   (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
~~~~

What's the crucial difference?

Neither `Functor` nor `Applicative` can affect the "structure" `f`
that gets returned.

They can *only* change `b` *inside* `f`.

Therefore they're **friendly** to static analysis!


# Enter Haxl

~~~~ {.haskell}
newtype Haxl a = Haxl { unHaxl :: IO (Result a) }

data Result a = Done a
              | Blocked (Haxl a)

instance Monad Haxl where
  return a = Haxl (return (Done a))

  m >>= k = Haxl $ do
	a <- unHaxl m
	case a of
	  Done a    -> unHaxl (k a)
	  Blocked r -> return (Blocked (r >>= k))
~~~~


# What's going on?

~~~~ {.haskell}
data Result a = Done a
              | Blocked (Haxl a)
~~~~

A computation has either

* Completed successfully

* Blocked on a pending data fetch


# What is the `Haxl` monad?

Alternate names for this construction:

* Resumption monad, concurrency monad

* Modern name: free monad (ours has a few bells and whistles)

What?

* We are separating the *representation* of the computation from the
  way it will be *run*

* `Haxl` and `Result` give us an abstract syntax tree (AST) that we
  can manipulate before we execute the code


# Problem?

~~~~ {.haskell}
countCommonFriends :: UserID -> UserID -> Haxl Int

countCommonFriends x y = do
  fx <- friendsOf x
  fy <- friendsOf y
  return (length (intersect fx fy))
~~~~

We run out of AST to explore as soon as we hit the first `friendsOf`,
thanks to use of `>>=` (via `do` desugaring).

Can't see the second one, so no concurrent execution.


# Applicatives to the rescue!

Can we rewrite our function to make more of the fetches statically
visible?

Yes!

~~~~ {.haskell}
countCommonFriends x y =
  length <$> (intersect <$> friendsOf x <*> friendsOf y)
~~~~


# A little bit of sequencing

~~~~ {.haskell}
instance Applicative Haxl where
  pure = return
  Haxl f <*> Haxl a = Haxl $ do
	r <- f
	case r of
	Done f' -> do
	  ra <- a
	  case ra of
		Done a'    -> pure (Done    (f' a'))
		Blocked a' -> pure (Blocked (f' <$> a'))
	Blocked f' -> do
	  ra <- a
	  case ra of
		Done a'    -> pure (Blocked (f' <*> pure a'))
		Blocked a' -> pure (Blocked (f' <*> a'))
~~~~


# Simplifying coding

Two complementary approaches:

* Alternate Haxl-friendly prelude

* Fancy language support (`ApplicativeDo`)


# Fancy language support

~~~~ {.haskell}
{-# LANGUAGE ApplicativeDo #-}
~~~~

Facebook-developed language extension

When `ApplicativeDo` is turned on:

* GHC will use a different method for desugaring `do`-notation

* Attempts to use the `Applicative` operator `<*>` as far as possible,
  along with `fmap` and `join`

* Makes it possible to use `do`-notation for types that are
  `Applicative` but not `Monad`

Example:

~~~~ {.haskell}
do
  x <- a
  y <- b
  return (f x y)
~~~~

Translates to:

~~~~ {.haskell}
f <$> a <*> b
~~~~


# Why does caching matter?

Consider our example fragment of code:

~~~~ {.haskell}
length (intersect (friendsOf x) (friendsOf y))
~~~~

What if this was executed as two fetches?

* And what if `x` == `y`?

* If the two queries returned different results, we'd get bogus
  behaviour

* For this application, caching matters for performance and
  **correctness**


# Data sources

Several "core data" systems at Facebook

* Memcache

* TAO

* MySQL

Other data sources also involved in Sigma

* Machine learning systems


# Architecture

* Haxl core doesn't know about data sources

* Data sources are hot-pluggable (!)

A data source interacts with Haxl core in 3 ways

* Persistent state (e.g. threads, connection pools)
* Issuing data fetch requests
* Fetching the data


# Implementation

~~~~ {.haskell}
-- Core abstraction
class DataSource req where
  {- ... -}

-- An example data source
data ExampleReq a where
  CountAardvarks :: String -> ExampleReq Int
  ListWombats :: Id -> ExampleReq [Id]
  deriving Typeable

-- Fetch data generically
dataFetch :: DataSource req => req a -> Haxl a
~~~~


# Refinement: blocked computations

~~~~ {.haskell}
dataFetch :: DataSource req => req a -> Haxl a
~~~~

Haxl core has to manage requests submitted with `dataFetch`

Once an entire round of fetching has stopped making progress, we
retrieve the pending requests

~~~~ {.haskell}
class DataSource req where
  fetch :: [BlockedFetch req] -> IO ()

data BlockedFetch req =
  forall a . BlockedFetch (req a) (MVar a)
~~~~


# Refinement: acquiring pending requests

Remember: Haxl core is agnostic to data sources

We use dynamic typing (`Typeable`) to manage them

~~~~ {.haskell}
-- hack to support parameterised types
class Eq1 req where
  eq1 :: req a -> req a -> Bool

class (Typeable1 req, Hashable1 req, Eq1 req) =>
      DataSource req where
  data DataState req

  fetch :: DataState req
      -> [BlockedFetch req]
      -> IO ()
~~~~

`DataState` is an *associated type*


# Is that it?

A lot of other demanding and intricate work went into making Haxl run
effectively at scale.

As of June 2015, Haxl-powered Sigma was handling over a million RPS.

For many more juicy details, see the article on
[code.facebook.com](https://code.facebook.com/posts/745068642270222/fighting-spam-with-haskell/).

Code is open sourced on Hackage as
[haxl](http://hackage.haskell.org/package/haxl).

Haxl inspired Twitter's
[Stitch project](https://engineering.twitter.com/university/videos/introducing-stitch).


# Dynamic types at scale

Facebook is famous for having been built in PHP.

As our code base grew to millions of lines in size:

* PHP's dynamic typing became increasingly troublesome

* It became harder to understand and change code

* "What type is this variable?  I have no idea!"

We observed the same growing pains in Javascript.


# Gradual typing

In between static and dynamic type systems lies the interesting middle
ground of *gradual types*.

* Some variables are given explicit types, which can be checked
  statically

* Others have types checked at runtime

Languages that start all-dynamic can acquire gradual typing

* Racket, Clojure

...as can languages that start all-static!

* C#


# Gradual typing of PHP

At Facebook, we developed Hack: [hacklang.org](http://hacklang.org/)

* Addresses the productivity and reliability problems of PHP<br>(*lots
  of* PHP!)

* A gradually typed language

* Add type annotations to existing code *incrementally*

* IDE support: type-sensitive autocomplete, live typechecking and
  errors


# Hackification

Well over 90% of Facebook PHP code is now statically typed via Hack.

* Much of our code was always statically typeable!

Some code is still too tricky to statically type.

* Tends to be isolated.


# Type system ergonomics

PHP programmers rightly value rapid feedback.

* Fast iteration: "Save my file, hit reload in the browser, see what
  went wrong, fix it."

With Hack, this short cycle time *had to be* preserved.

Implications are huge:

* Type checking algorithms must be fast and mostly incremental

* A persistent daemon maintains type system state

* Filesystem change notifications lead to "instant" type checking

* Typical typechecking time is 200ms (outliers up to 1000ms) for
  **millions of lines of code**


# Implementing Hack

The Hack type checker is built in OCaml.

OCaml had, until recently, zero support for multiple CPUs.

But multiple CPUs can speed up type checking, and we care greatly
about speed ...

* Workaround: multi-process architecture using shared memory and
  lock-free concurrent data structures


# Gradually typed Javascript: Flow

Following the success of Hack at Facebook, we started work on the same
problem space of types in Javascript.

The result is the Flow type checker:
[flowtype.org](http://flowtype.org/)

Similar ergonomics:

* Heavily based on automatic inference

* Fast incremental checks


# Philosophical differences

TypeScript

* Pessimistically assumes most JS code is statically untypeable

* Only typechecks code that is explicitly annotated

* Designed mainly for IDE tooling; unsound whenever convenient

Flow

* Optimistically assumes that most JS is statically typable

* Uses type inference to fill in missing annotations

* Designed to find errors; takes soundness far more seriously


# Flow vs TypeScript: pragmatics

Somewhat similar ideas, different tradeoffs

Pros for TS:

* Really slick Visual Studio integration

* Support for typing of third-party js (but all those `.ts` files are a
  pain)

* One-stop type checking and transformation

Pros for Flow:

* Path-sensitive type narrowing

* Better type checking of idiomatic js

* Really fast

* Gets non-nullability right (new TS support is unsound)


# Summary

<img src="http://cdn.makeagif.com/media/7-31-2015/1BQ3s8.gif"></img>


# React

Highly influential FP-based framework:

Focus:

* Building large, understandable UI-heavy applications

FP heritage:

* Immutable data, mostly pure functions

* DOM updates abstracted and made efficient

* Declarative "here's how my UI should look at an instant in time" feel

* Focus on composable components

* Flow for type safety


# Mobile development

iOS and Android development are complex and expensive

* Huge quirky APIs take ages to master

* Investment in one platform is not transferable to the other

* Client-side "business logic" locked up in ObjC or Java (or worse,
  C++)


# React Native

Similar playbook to React:

* Declarative, self-contained UI components

* Rapid feedback during development

But:

* Integrates with platform native components

* Uses native gesture recognition

* Allows proper "look and feel", not typical "write once, run
  anywhere" clunkiness


# Fetching network data for modern apps

REST isn't a great model in practice.

Consider the GitHub API.

Here's a response to a "fetch me all comments on an issue" request.

~~~~ {.javascript}
[
  {
    "id": 1,
    "url": "https://api.github.com/repos/octocat/Hello-World/issues/comments/1",
    "html_url": "https://github.com/octocat/Hello-World/issues/1347#issuecomment-1",
    "body": "Me too",
    "user": {
      "login": "octocat",
      "id": 1,
      "avatar_url": "https://github.com/images/error/octocat_happy.gif",
      "gravatar_id": "",
      "url": "https://api.github.com/users/octocat",
      "html_url": "https://github.com/octocat",
      "followers_url": "https://api.github.com/users/octocat/followers",
      "following_url": "https://api.github.com/users/octocat/following{/other_user}",
      "gists_url": "https://api.github.com/users/octocat/gists{/gist_id}",
      "starred_url": "https://api.github.com/users/octocat/starred{/owner}{/repo}",
      "subscriptions_url": "https://api.github.com/users/octocat/subscriptions",
      "organizations_url": "https://api.github.com/users/octocat/orgs",
      "repos_url": "https://api.github.com/users/octocat/repos",
      "events_url": "https://api.github.com/users/octocat/events{/privacy}",
      "received_events_url": "https://api.github.com/users/octocat/received_events",
      "type": "User",
      "site_admin": false
    },
    "created_at": "2011-04-14T16:00:49Z",
    "updated_at": "2011-04-14T16:00:49Z"
  }
]
~~~~


# Problems with REST

* Kitchen-sink responses with a bunch of information you probably don't
  need, because maybe someone sometimes will need some of them

* Multiple fetches: data included by reference in a response require
  costly additional roundtrips, concurrency management, and error
  handling

* Custom endpoint proliferation (and duplication) as new requirements
  evolve

* Version bloat: as client versions proliferate, legacy server
  endpoints must be maintained

* Client-side cache management and UI updates are painful when data
  changes


# GraphQL

A query language and protocol for graph-based data.

Puts the client in control of requesting only the data it needs in a
single round-trip.

Unlike SQL, where there's a storage engine with tables and indices
behind the query engine, a GraphQL server is powered by arbitrary
code (which you write).

Developer friendliness: self-documenting schema, strong type system,
interactive exploration.


# GraphQL and Haxl

Chad Austin wrote a great overview of a GraphQL server that he built
in Haskell using Haxl:
[chadaustin.me](https://chadaustin.me/2016/02/dropbox-hack-week-graphql-server-in-haskell/)

In response to a single GraphQL request, this uses Haxl to efficiently
query multiple endpoints in a minimal number of roundtrips.


# Convergent evolution

Sharing some design features with Haxl is the Relay project:
[facebook.github.io/relay](https://facebook.github.io/relay/)

* Declarative "here's the data this view needs" specifications

* Abstracts away details of how and when to fetch data

* When modifications are made to data, hides details of updates and
  cache consistency


# Recap

Haxl demonstrates how to roll your own unobtrusive concurrency

There's a ton of other FP-influenced work at Facebook:

* Languages: Hack and Flow (built in OCaml)

* User interfaces: React and React Native

* Data: GraphQL and Relay

Key takeaway:

* Ergonomics really matter!

* All of these systems have had a ton of attention put into making
  them efficient and easy to work with.
