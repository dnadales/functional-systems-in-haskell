-- | 

module FPatFB where

-- * Enter Haxl

newtype Haxl a = Haxl { unHaxl :: IO (Result a)}

data Result a = Done a
              | Blocked (Haxl a)

instance Monad Haxl where
    return = Haxl . return . Done 

    m >>= k = Haxl $ do
        res <- unHaxl m
        case res of
            Done a -> unHaxl (k a)
            Blocked h -> return (Blocked (h >>= k))


-- TODO: next, write an applicative instance

instance Applicative Haxl where
    pure = return

    -- (<*>) :: Haxl (a -> b) -> Haxl a -> Haxl b
    hf <*> ha = Haxl $ do
        rf <- unHaxl hf
        case rf of
            Done f -> do
                ra <- unHaxl ha
                case ra of
                    Done a      -> return $ Done $ f a
                    Blocked ha' -> return $ Blocked $ pure f <*> ha'
            Blocked hf' -> do
                ra <- unHaxl ha
                case ra of
                    Done a       -> return $ Blocked (hf <*> pure a)
                    Blocked ha'  -> return $ Blocked $ hf' <*> ha'

instance Functor Haxl where
    fmap f ha = pure f <*> ha
