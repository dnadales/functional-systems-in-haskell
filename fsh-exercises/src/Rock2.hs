-- | Exercise taken from:
--
--     http://www.scs.stanford.edu/16wi-cs240h/slides/basics-slides.html#(65)

module Rock2 where

import           Control.Exception
import           Network
import           System.IO

data Move = Rock | Paper | Scissors deriving (Eq, Read, Show, Enum, Bounded)

data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

-- | @outcome our_move their_move@
outcome :: Move -> Move -> Outcome
outcome Rock Scissors        = Win
outcome Paper Rock           = Win
outcome Scissors Paper       = Win
outcome us them | us == them = Tie
                | otherwise  = Lose

parseMove :: String -> Maybe Move
parseMove str = case reads str of
  [(m, rest)] | ok rest -> Just m
  _                     -> Nothing
  where ok = all (`elem` " \r\n")

getMove :: Handle -> IO Move
getMove h = do
  hPutStrLn h $ "Please enter one of " ++ show ([minBound..] :: [Move])
  input <- hGetLine h
  case parseMove input of Just move -> return move
                          Nothing -> getMove h

computerVsUser :: Move -> Handle -> IO ()
computerVsUser computerMove h = do
  userMove <- getMove h
  let o = outcome userMove computerMove
  hPutStrLn h $ "You " ++ show o

withTty :: (Handle -> IO a) -> IO a
withTty = withFile "/dev/tty" ReadWriteMode

-- connectTo :: HostName -> PortID -> IO Handle
-- listenOn :: PortID -> IO Socket
-- accept :: Socket -> (Handle, HostName, PortNumber)
-- sClose :: Socket -> IO ()

withClient :: PortID -> (Handle -> IO a) -> IO a
withClient listenPort fn = do
  -- XXX here is where you must accept a TCP connection and call fn on it
  socket <- listenOn listenPort
  (hndl, _, _) <- accept  socket
  finally (fn hndl) (sClose socket >> hClose hndl)

run :: IO ()
run = withClient (PortNumber 1617) (computerVsUser Rock)
