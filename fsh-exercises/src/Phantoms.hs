module Phantoms () where

type Param = (String, String)

type ContentType = String

data Part = Part
  { name :: String
  , fileName :: Maybe FilePath
  , contentType :: Maybe ContentType
  , body :: String
  } deriving (Show)

data Payload a = NoPayload
               | Raw ContentType String
               | Params [Param]
               | FormData [Part]
               deriving (Show)

-- Can you write a Monoid instance for `Payload`?
instance Monoid (Payload [Param]) where
