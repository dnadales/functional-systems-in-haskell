% Parsing

# Intro

Hi!

Office hours:

* Wednesdays, 14:30-15:00

* The room full of grad students next to Gates 290

* **Will not be present** Jan 20

# Let's talk about parsing

A whole lot of programming involves interacting with external sources
of data:

* Files containing junk

* Network peers sending us junk

Our sad duty is to try to make sense of this stuff.


# O hai, HTTP 1.1

The very first lines of BNF from RFC 2616:

~~~~ {.commonlisp}
Request = Request-Line              ; Section 5.1
          *(( general-header        ; Section 4.5
           | request-header         ; Section 5.3
           | entity-header ) CRLF)  ; Section 7.1
          CRLF
          [ message-body ]          ; Section 4.3
~~~~

(Never read an RFC?  See
[RFC 2616 section 2](http://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2)
for a quick tour of IETF BNF syntax.)


# Parsing by hand: C++ parser fragment

Long ago, the ancients wrote all of their parsers by hand, because
that was all they could do.

~~~~ {.cpp}
void Response::ProcessStatusLine( std::string const& line )
{
    const char* p = line.c_str();

    while( *p && *p == ' ' )
        ++p;

    while( *p && *p != ' ' )
        m_VersionString += *p++;
    while( *p && *p == ' ' )
        ++p;

    std::string status;
    while( *p && *p != ' ' )
        status += *p++;
    while( *p && *p == ' ' )
        ++p;

    while( *p )
        m_Reason += *p++;

    m_Status = atoi( status.c_str() );
    if( m_Status < 100 || m_Status > 999 )
        throw Wobbly( "BadStatusLine (%s)", line.c_str() );

    if( m_VersionString == "HTTP:/1.0" )
        m_Version = 10;
    else if( 0==m_VersionString.compare( 0,7,"HTTP/1." ) )
        m_Version = 11;
    else
        throw Wobbly( "UnknownProtocol (%s)", m_VersionString.c_str() );

    m_State = HEADERS;
    m_HeaderAccum.clear();
}
~~~~


# Parsing by hand: Java parser fragment, page 1

~~~~ {.java}
public class HttpResponseParser extends AbstractMessageParser {

    private final HttpResponseFactory responseFactory;
    private final CharArrayBuffer lineBuf;

    public HttpResponseParser(
            final SessionInputBuffer buffer,
            final LineParser parser,
            final HttpResponseFactory responseFactory,
            final HttpParams params) {
        super(buffer, parser, params);
        if (responseFactory == null) {
            throw new IllegalArgumentException("Response factory may not be null");
        }
        this.responseFactory = responseFactory;
        this.lineBuf = new CharArrayBuffer(128);
    }
~~~~


# Parsing by hand: Java parser fragment, page 2

~~~~ {.java}
    protected HttpMessage parseHead(
            final SessionInputBuffer sessionBuffer)
        throws IOException, HttpException, ParseException {

        this.lineBuf.clear();
        int i = sessionBuffer.readLine(this.lineBuf);
        if (i == -1) {
            throw new NoHttpResponseException("The target server failed to respond");
        }
        //create the status line from the status string
        ParserCursor cursor = new ParserCursor(0, this.lineBuf.length());
        StatusLine statusline = lineParser.parseStatusLine(this.lineBuf, cursor);
        return this.responseFactory.newHttpResponse(statusline, null);
    }
}
~~~~


# Commentary

Hand-written parsers can be quite efficient.

But...


# Commentary

Hand-written parsers can be quite efficient.

But...

<img src="jbwtf.gif"></img>


# Domain specific languages

~~~~
%%{
  machine http_parser_common;

  scheme = ( alpha | digit | "+" | "-" | "." )* ;
  absolute_uri = (scheme ":" (uchar | reserved )*);

  path = ( pchar+ ( "/" pchar* )* ) ;
  query = ( uchar | reserved )* %query_string ;
  param = ( pchar | "/" )* ;
  params = ( param ( ";" param )* ) ;
  rel_path = ( path? %request_path (";" params)? ) ("?" %start_query query)?;
  absolute_path = ( "/"+ rel_path );

  Request_URI = ( "*" | absolute_uri | absolute_path ) >mark %request_uri;
  Fragment = ( uchar | reserved )* >mark %fragment;
  Method = ( upper | digit | safe ){1,20} >mark %request_method;

  http_number = ( digit+ "." digit+ ) ;
  HTTP_Version = ( "HTTP/" http_number ) >mark %http_version ;
  Request_Line = ( Method " " Request_URI ("#" Fragment){0,1} " " HTTP_Version CRLF ) ;

  field_name = ( token -- ":" )+ >start_field %write_field;

  field_value = any* >start_value %write_value;

  message_header = field_name ":" " "* field_value :> CRLF;

  Request = Request_Line ( message_header )* ( CRLF @done );

  main := Request;
}%%
~~~~


# What was that?

That last slide used a DSL named Ragel.

We squeezed most of a parser into one slide!

Problems:

* A completely new language.

* Generated code is hard to follow.

* Error reporting and recovery can be nasty.

Advantages:

* Concise.

* DSL-powered parsers (bison, antlr) can optimize parsers and find
  certain classes of grammar bugs.


# Parsing in Haskell

Let's build a parser of our own.

The dumbest parser we can think of:

* Accept a string to match against,

* and another string as input,

* then tell us if the input string matches our first string.

What should this parser's type signature be?


# Parsing in Haskell

Let's build a parser of our own.

The dumbest parser we can think of:

* Accept a string to match against,

* and another string as input,

* then tell us if the input string matches our first string.

What should this parser's type signature be?

~~~~ {.haskell}
string :: String -> String -> Bool
~~~~

(Is this a *good* type signature??)


# Parsing a number

Suppose we want to parse a decimal number.

What should our type signature be?


# Parsing a number

Suppose we want to parse a decimal number.

What should our type signature be?

~~~~ {.haskell}
int :: String -> Int
~~~~

(Is *this* a good type signature?)


# A concrete example

~~~~ {.haskell}
import Data.List

string :: String -> String -> Bool
string = (==)

number :: String -> Int
number = read
~~~~


# This is a dead end

We can't do anything useful with these functions.

What are we missing?


# Not is a dead end

We can't do anything useful with these functions.

What are we missing?

A new perspective:

* A parser consumes as much input as it can.

* It converts the input as necessary.

* It returns the result of the conversion.

But most importantly:

* It *also* returns *the leftover input* that it could not consume.


# Abstractly

Given an input `s` and a desired result `a`, we could model a parser
via this type:

~~~~ {.haskell}
type Parser s a = s -> (a, s)
~~~~

What are we missing?


# Abstractly

Given an input `s` and a desired result `a`, we could model a parser
via this type:

~~~~ {.haskell}
type Parser s a = s -> (a, s)
~~~~

What are we missing?

* Parsers can fail.

~~~~ {.haskell}
type Parser s a = s -> Maybe (a, s)
~~~~


# Less crappy

~~~~ {.haskell}
import Data.Char
import Data.List
import Data.Maybe

type Parser s a = s -> Maybe (a,s)

string :: String -> Parser String String
string pat input =
  case stripPrefix pat input of
    Nothing   -> Nothing
    Just rest -> Just (pat, rest)
~~~~

Please supply a type signature and body for `number`. You have 2 minutes.

~~~~ {.haskell}
number = undefined
~~~~

Skeleton: [`http://cs240h.scs.stanford.edu/DumbP.hs`](http://cs240h.scs.stanford.edu/DumbP.hs)



# Your turn!

Build a small parser from `string` and `number`.

If you call:

~~~~ {.haskell}
version "HTTP/1.1\r\n"
~~~~

You should get back a result containing:

~~~~ {.haskell}
(1,1)
~~~~

You have 5 minutes.


# Horrible first version

Look at this staircase of `case` expressions!

~~~~ {.haskell}
versionDumb i0 =
  case string "HTTP/" i0 of
    Nothing -> Nothing
    Just (_,i1) ->
      case number i1 of
        Nothing -> Nothing
        Just (maj,i2) ->
          case string "." i2 of
            Nothing -> Nothing
            Just (n,i3) ->
              case number i3 of
                Nothing -> Nothing
                Just (min,i4) -> Just ((maj,min),i4)
~~~~

What's the pattern here?


# Noticing things

~~~~ {.haskell}
versionDumb i0 =
  case string "HTTP/" i0 of
    Nothing -> Nothing
    Just (_,i1) ->
      case number i1 of
        {- ... -}
~~~~

We have:

* A parser is fed some input

* We do `case` analysis of the result

* On success, we pass the leftover input to the next parser


# Turn the boilerplate into a function

~~~~ {.haskell}
andThen :: Parser s a -> (a -> Parser s b) -> Parser s b
andThen parse next = \input ->
  case parse input of
    Nothing          -> Nothing
    Just (a, input') -> next a input'
~~~~


# Now what?

~~~~ {.haskell}
version2 =
  string "HTTP/" `andThen` \_ ->
  number `andThen` \maj ->
  string "." `andThen` \_ ->
  number `andThen` \min ->
  {- ... give back (maj,min) -}
~~~~

From 12 lines to 4!

All that's missing:

* How do we construct the `(maj,min)` tuple?


# Handing back a result

We need to stuff our result into a `Parser` box:

~~~~ {.haskell}
stuff :: a -> Parser s a
stuff a = \input -> Just (a, input)
~~~~

Thus:

~~~~ {.haskell}
version2 =
  string "HTTP/" `andThen` \_ ->
  number `andThen` \maj ->
  string "." `andThen` \_ ->
  number `andThen` \min ->
  stuff (maj,min)
~~~~


# Types

Look at this:

~~~~ {.haskell}
andThen :: Parser s a -> (a -> Parser s b) -> Parser s b
(>>=)   :: Monad m =>
           m        a -> (a -> m        b) -> m        b
~~~~

And this:

~~~~ {.haskell}
stuff  ::            a -> Parser s a
return :: Monad m => a -> m        a
~~~~


# So close!

<img src="wow.jpg"></img>


# Making a "proper" monad

We **don't** want to write a `Monad` instance for this type:

~~~~ {.haskell}
type Parser s a = s -> (a, s)
~~~~

If we did, *every* function from `s` to `(a,s)` would be an instance
of our weird monad.

Instead of `type`, let's use `newtype`.

~~~~ {.haskell}
-- type Parser s a =             s -> Maybe (a, s)
newtype Parser s a = P { runP :: s -> Maybe (a, s) }
~~~~

Our `Monad` instance:

~~~~ {.haskell}
instance Monad (Parser s) where
  (>>=)  = bind
  return = shove
~~~~


# return

Let's look at our old parser (now named `OldParser`) and our new
`Parser` side by side.

~~~~ {.haskell}
stuff :: a -> OldParser s a
shove :: a ->    Parser s a

stuff a =     \input -> Just (a, input)

shove a = P $ \input -> Just (a, input)
~~~~

It should be clear that the *only* difference is some `newtype`
machinery.


# bind

~~~~ {.haskell}
andThen :: OldParser s a -> (a -> OldParser s b) -> OldParser s b
bind    ::    Parser s a -> (a ->    Parser s b) ->    Parser s b

andThen parse next =     \input ->
  case      parse input of
    Nothing          -> Nothing
    Just (a, input') ->      (next a) input'

bind parse next    = P $ \input ->
  case runP parse input of
    Nothing          -> Nothing
    Just (a, input') -> runP (next a) input'
~~~~

What should the `Functor` instance look like?

~~~~ {.haskell}
class Functor where
  fmap :: (a -> b) -> f a -> f b
~~~~


# Functor

~~~~ {.haskell}
instance Functor (Parser s) where
  fmap f parser = P $ \input ->
    case runP parser input of
      Nothing          -> Nothing
      Just (a, input') -> Just (f a, input')
~~~~


# Revisiting our smaller parser

Before:

~~~~ {.haskell}
version2 =
  string "HTTP/" `andThen` \_ ->
  number `andThen` \maj ->
  string "." `andThen` \_ ->
  number `andThen` \min ->
  stuff (maj,min)
~~~~

Suppose we plumb `P` and `runP` into the right places in `string` and
`number`.

After:

~~~~ {.haskell}
version3 = do
  string "HTTP/"
  maj <- number
  string "."
  min <- number
  return (maj,min)
~~~~


# Can we do better?

We needed our `Functor` instance so we can write this:

~~~~ {.haskell}
import Control.Applicative
import Control.Monad (ap)

instance Applicative (Parser s) where
  pure  = return
  (<*>) = ap
~~~~

Our parser now becomes:

~~~~ {.haskell}
version4 = (,) <$>
           (string "HTTP/" *> number <* string ".") <*>
           number
~~~~

Nice!


# What next? A small trick!

Let's write less-polymorphic combinators to aid our notation:

~~~~ {.haskell}
(.*>) :: String -> Parser String b -> Parser String b
a .*> b = string a *> b
infixl 4 .*>

(<*.) :: Parser String a -> String -> Parser String a
a <*. b = a <* string b
infixl 4 <*.
~~~~

And finally:

~~~~ {.haskell}
version5 = (,) <$> ("HTTP/" .*> number <*. ".")
               <*> number
~~~~

From 12 lines to two!


# Your turn: Alternative

How hard is it to build a parser that can choose another parse if the
first one fails?

~~~~ {.haskell}
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
~~~~

Take 5 minutes to write your own.

~~~~ {.haskell}
instance Alternative (Parser s) where
    {- ... -}
~~~~

Skeleton: [`http://cs240h.scs.stanford.edu/P.hs`](http://cs240h.scs.stanford.edu/P.hs)

Desired behaviour:

~~~~ {.haskell}
>>> let p = string "foo" <|> string "bar"
>>> runP p "foowhee"
Just ("foo","whee")
>>> runP p "quuxly"
Nothing
>>> runP p "barely"
Just ("bar","ely")
~~~~


# Alternative

My implementation took about one minute:

~~~~ {.haskell}
instance Alternative (Parser s) where
    empty = P $ \_ -> Nothing

    f <|> g = P $ \input ->
      case runP f input of
        Nothing -> runP g input
        result  -> result
~~~~


# Parsec

There's a long history of functional programmers writing
parsing libraries.

Parsec
([Leijen 2001](http://legacy.cs.uu.nl/daan/download/papers/parsec-paper.pdf))
was the first to be practical for real world use.


# Parsec and attoparsec

Inspired by Parsec, I wrote a specialized derivative named
[attoparsec](http://hackage.haskell.org/package/attoparsec).

How do the two differ?

* attoparsec is optimized for *fast* stream and file parsing.

* attoparsec focuses on the `ByteString` and `Text` types.

* Parsec is more general: it can parse `String` too (in fact
  arbitrary token types).

* attoparsec does *not* attempt to give friendly error messages (no
  file names or line numbers).  It's aimed at data generated by machines.

* Parsec might be a better choice for e.g. parsing source files, where
  the friendliness/performance tradeoff is a little different.


# Mise en scene

Typical network protocol problem:

* You receive a TCP segment off the network

* You need to decode a variable-length message that lacks message
  length information (e.g. a JSON blob)

* How do you tell whether your packet/segment contains enough data to
  parse the whole message?

More importantly:

* How messed-up does your parser become to accommodate this need?


# One approach

What Parsec does:

* Built as a *monad transformer* (stackable on top of another monad)

* Combines parsing with ability to perform IO (e.g. read more TCP
  segments)

<img src="jackie.jpg"></img>


# Monad transformers

In general:

* The composition of two monads is *not* itself a monad.

* Monad transfomers allow us to "stack" a series of "transformers" on
  top of a base monad, so that the stacked combination *is* a monad.

For our Parsec case, the base monad is `IO`, and the transformer
stacked on top is `ParsecT`.

Read more on Haskellwiki if you need to know.


# Monad transformers

In general:

* The composition of two monads is *not* itself a monad.

* Monad transfomers allow us to "stack" a series of "transformers" on
  top of a base monad, so that the stacked combination *is* a monad.

For our Parsec case, the base monad is `IO`, and the transformer
stacked on top is `ParsecT`.

Read more on Haskellwiki **if you dare**.

<img src="wibble.gif">


# A simple attoparsec parser

Parse the `Request-Line` of an HTTP 1.1 request:

~~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.ByteString.Char8 as A
import Control.Applicative

request = (,,) <$>
          (verb <* skipSpace) <*>
          (url <* skipSpace) <*>
          (version <* endOfLine)

verb = "GET" <|> "POST"
url = takeTill A.isSpace
version = (,) <$> ("HTTP/" *> decimal) <*> ("." *> decimal)
~~~~


# Let's try this in ghci

~~~~ {.haskell}
>>> :load AttoHttp.hs
>>> parse request "GET /foo HTTP/1.1\r\n"
Done "" ("GET","/foo",(1,1))
~~~~

What happens on incomplete input?

~~~~ {.haskell}
>>> parse request "GET /foo HTTP/"
Partial _
~~~~


# Feeding more input

The `Partial` constructor has a parameter that is a function. We can
use `feed` to supply it with more input.

~~~~ {.haskell}
>>> let r = parse request "GET /foo HTTP/"
>>> r
Partial _
>>> r `feed` "1.1\r\n"
Done "" ("GET","/foo",(1,1))
~~~~


# How does this work?

<img src="cont.png"></img>


# Basic continuation based parsing

Instead of returning a value:

* If a parse succeeds or fails, we *call a function*.

~~~~ {.haskell}
{-# LANGUAGE RankNTypes #-}

type ErrMsg = String

newtype ContP a = ContP {
    runP :: forall r.                        -- wat
	        String                           -- input
         -> (ErrMsg -> Either ErrMsg r)      -- failure
         -> (String -> a -> Either ErrMsg r) -- success
         -> Either ErrMsg r                  -- result
  }
~~~~


# Rank-1 types

For polymorphic Haskell functions, there is a type quantifier that is
not written.

~~~~ {.haskell}
id :: a -> a
~~~~

Really means:

~~~~ {.haskell}
id :: forall a. a -> a
~~~~

And `forall` is a type-level lambda, corresponding to the value-level
lambda "`\`".

So in the above signature, we say:

* "Caller, provide me a type and I'll bind it to the type variable `a`."

We call this a *rank-1 type* because the `forall` is present at the
outermost level, or rank.


# Rank-2 types

Our continuation-based parser has a rank-2 type.

~~~~ {.haskell}
newtype ContP a = ContP {
    runP :: forall r.                        -- wat
	        String                           -- input
         -> (ErrMsg -> Either ErrMsg r)      -- failure
         -> (String -> a -> Either ErrMsg r) -- success
         -> Either ErrMsg r                  -- result
  }
~~~~

The *caller* of `runP` cannot pick the type of `r`.

* Instead, the *callee* controls it.


# Continuations are tricky

Working with continuations is tough.

Here's a simple example:

~~~~ {.haskell}
instance Functor ContP where
  fmap f p = ContP $ \bs0 fail0 succ0 ->
    runP p bs0 fail0 $ \bs1 a -> succ0 bs1 (f a)
~~~~

Help me fumble through a `Monad` instance!


# Attoparsec internals: the types

~~~~ {.haskell}
newtype Parser t a = Parser {
      runParser :: forall r. Input t -> Added t -> More
                -> Failure t   r
                -> Success t a r
                -> IResult t r
    }

type Failure t   r = Input t -> Added t -> More
                   -> [String] -> String -> IResult t r
type Success t a r = Input t -> Added t -> More
                   -> a -> IResult t r

newtype Input t = I {unI :: t} deriving (Monoid)
newtype Added t = A {unA :: t} deriving (Monoid)

-- | Have we reached EOF?
data More = Complete | Incomplete
            deriving (Eq, Show)

data IResult t r = Fail t [String] String
                 | Partial (t -> IResult t r)
                 | Done t r
~~~~


# Types I

We have to somehow make `feed` a function that is possible to write.

If we are given more input, we use these types to track it.

~~~~ {.haskell}
newtype Input t = I {unI :: t} deriving (Monoid)
newtype Added t = A {unA :: t} deriving (Monoid)
~~~~

`Input` is all the input we've ever seen.

`Added` is the *extra* input we've been given via `feed`.

We keep track of the two separately because parsing can fail.

* When parsing fails and we try another branch, we *backtrack*.

When we backtrack, we throw away the `Input` state.

We tack all the data our caller `Added` back on again, so we won't
forget it.


# Types I.5

Suppose our caller runs out of input to give us on one branch.

But our parse fails.

We need to remember this so that after we backtrack, we won't ask for
more input on the other side of the branch.

~~~~ {.haskell}
data More = Complete | Incomplete
            deriving (Eq, Show)
~~~~


# Types II

If a parse fails, we report an error message, along with a stack of
context information that might help with debugging.

~~~~ {.haskell}
type Failure t   r = Input t -> Added t -> More
                   -> [String] -> String -> IResult t r
~~~~

If the parse succeeds, we call our successor continuation with our
result.

~~~~ {.haskell}
type Success t a r = Input t -> Added t -> More
                   -> a -> IResult t r
~~~~

The result is just a fancier `Either` with the possibility of saying
"feed me more before I answer you".

~~~~ {.haskell}
data IResult t r = Fail t [String] String
                 | Partial (t -> IResult t r)
                 | Done t r
~~~~


# Running and ending a parse

As is usually the case with monads, the user-visible "run this monad"
function is quite simple.

~~~~ {.haskell}
parse :: Monoid t => Parser t a -> t -> IResult t a
parse m s = runParser m (I s) (A mempty) Incomplete failK successK
~~~~

The only slight complication:

* We need to create "terminal continuations"

* i.e. continuations that *do not* chain up yet another continuation,
  but instead return us to our usual mode of computation

~~~~ {.haskell}
failK :: Failure t a
failK i0 _a0 _m0 stack msg = Fail (unI i0) stack msg

successK :: Success t a a
successK i0 _a0 _m0 a = Done (unI i0) a
~~~~


# Lab 1

http://www.scs.stanford.edu/16wi-cs240h/labs/lab1.html

Write a simple Haskell version of the UNIX `tr` program.
