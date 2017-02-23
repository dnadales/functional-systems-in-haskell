-- | Exercises on testing, taken from:
--
--    http://www.scs.stanford.edu/16wi-cs240h/slides/testing-slides.html#(9)
--

module Testing where

import Data.Word (Word16)
import Data.Char (ord, chr)
import Data.Bits ((.&.), shiftR, shiftL)

-- UTF-16 encoding.
--
-- - code points below 0x10000 (what fits in 16 bits) are encoded as a single
--   code unit.
--
-- - at and above 0x10000, two code units.
--
encodeChar :: Char -> [Word16]
encodeChar x
  | w < 0x10000 = [fromIntegral w]
  | otherwise = [fromIntegral a, fromIntegral b]
  where w = ord x
        a = ((w - 0x10000) `shiftR` 10) + 0xD800
        b = (w .&. 0x3FF) + 0xDC00

-- | Decoding:
--
-- Example:
--
-- >  decodeUtf16 (encodeChar '\150370')
--
-- Taken from http://unicode.org/faq/utf_bom.html
decodeUtf16 :: [Word16] -> [Char]
decodeUtf16 [x] = [chr (fromIntegral x)]
decodeUtf16 [lead, trail] =
  [chr $ ((leadI `shiftL` 10) + trailI + surrogateOffset)]
  where
    leadI = fromIntegral lead
    trailI = fromIntegral trail
    surrogateOffset = 0x10000 - (0xD800 `shiftL` 10) - 0xDC00
