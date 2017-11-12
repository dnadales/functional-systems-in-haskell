{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}

newtype Data tag contents = Data contents
newtype Cons tag contents = Cons contents

class Tag tag where showTag :: m tag c -> String

instance (Tag tag, Show c) => Show (Data tag c) where
    showsPrec d m@(Data c) = showParen (d > 10) $ \rest ->
      "Data[" ++ showTag m ++ "] " ++ showsPrec 11 c rest

instance (Tag tag, Show c) => Show (Cons tag c) where
    showsPrec d m@(Cons c) = showParen (d > 10) $ \rest ->
      "Cons[" ++ showTag m ++ "] " ++ showsPrec 11 c rest

-- Representing types like () with no constructor arguments
data Unit = Unit deriving Show

-- Representing constructor arguments
newtype Arg t = Arg t deriving Show
data (:*:) a b = a :*: b deriving Show
infixr 6 :*:

class MetaData d m | d -> m, m -> d where
  fromData :: d -> m
  toData :: m -> d

data T1 = C1 deriving Show

data T1_tag
instance Tag T1_tag where showTag _ = "T1"

data C1_tag
instance Tag C1_tag where showTag _ = "C1"

type T1_meta = Data T1_tag (Cons C1_tag Unit)

instance MetaData T1 T1_meta where
  fromData ~C1 = Data (Cons Unit)
  toData _ = C1


data T2 = C2 String Bool deriving Show

data T2_tag = T2_tag
instance Tag T2_tag where showTag _ = "T2"

data C2_tag = C2_tag deriving (Show, Bounded)
instance Tag C2_tag where showTag _ = "C2"

type T2_meta = Data T2_tag (Cons C2_tag
                            (Arg String :*: Arg Bool))
instance MetaData T2 T2_meta where
  fromData ~(C2 s b) = Data (Cons (Arg s :*: Arg b))
  toData ~(Data (Cons (Arg s :*: Arg b))) = C2 s b

class MyShow a where
  myShow :: a -> String
  default myShow :: (MetaData a m, MetaMyShow m) =>
                    a -> String
  myShow = genericMyShow

instance MyShow String where myShow = show
instance MyShow Int where myShow = show
instance MyShow Bool where myShow = show

class MetaMyShow a where
  metaMyShow :: a -> String
instance (MetaMyShow c) => MetaMyShow (Data tag c) where
  metaMyShow (Data c) = metaMyShow c
instance (Tag tag, MetaMyShow c) => MetaMyShow (Cons tag c) where
  metaMyShow m@(Cons c) = "(" ++ showTag m ++ metaMyShow c ++ ")"

instance MetaMyShow Unit where metaMyShow _ = ""

instance (MetaMyShow a, MetaMyShow b) =>
  MetaMyShow (a :*: b) where
    metaMyShow (a :*: b) = metaMyShow a ++ metaMyShow b
instance (MyShow a) => MetaMyShow (Arg a) where
  metaMyShow (Arg a) = ' ' : myShow a

genericMyShow :: (MetaData d m, MetaMyShow m) => d -> String
genericMyShow = metaMyShow . fromData

instance MyShow T1
instance MyShow T2
