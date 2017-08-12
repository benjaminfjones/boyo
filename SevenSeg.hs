{-# LANGUAGE BinaryLiterals #-}

module SevenSeg where

import qualified Data.List as L

import CLaSH.Prelude


encodeChar :: Unsigned 4 -> Unsigned 3
encodeChar 0 = 0b0111111
encodeChar 1 = 0b0000110
encodeChar 2 = 0b1011011
encodeChar 3 = 0b1001111
encodeChar 4 = 0b1100110
encodeChar 5 = 0b1101101
encodeChar 6 = 0b1111101
encodeChar 7 = 0b0000111
encodeChar 8 = 0b1111111
encodeChar 9 = 0b1100111
encodeChar _ = 0b1111111

multiSevenSeg
  :: Signal (Unsigned 4,  -- digit 1
             Unsigned 4,  -- digit 2
             Unsigned 4,  -- digit 3
             Unsigned 4)  -- digit 4
  -> Signal (Unsigned 3 {- seg -}, Unsigned 4 {- sel -})
multiSevenSeg = mealy m7 0
  where m7 c (d1,d2,d3,d4) = (c+1, (seg, snd sel))
          where
            _ = c :: Unsigned 18
            topC = resize (shiftR c 16) :: Unsigned 2

            seg = encodeChar (fst sel)

            sel :: (Unsigned 4, Unsigned 4)
            sel = case topC of
                    0 -> (d1, 0b1110)
                    1 -> (d2, 0b1101)
                    2 -> (d3, 0b1011)
                    3 -> (d4, 0b0111)
                    _ -> (d1, 0b1111)
