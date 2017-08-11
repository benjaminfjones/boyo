{-# LANGUAGE DataKinds #-}

module OneHot where

import CLaSH.Prelude
import CLaSH.Promoted.Nat
import CLaSH.Promoted.Nat.Literals

import Data.Proxy


-- | One hot encoder for 3-bit values
--
-- > L.take 4 $ simulate onehot3 [0..]
-- [<1,0,0,0,0,0,0,0>,<0,1,0,0,0,0,0,0>,<0,0,1,0,0,0,0,0>,<0,0,0,1,0,0,0,0>]
--
onehot3
  :: Signal (Unsigned 3)
  -> Signal (Vec 8 Bit)
onehot3 = mealy onehotT ()
  where onehotT :: () -> Unsigned 3 -> ((), Vec 8 Bit)
        onehotT () n = ((), oh n)

        oh n = fmap (\u -> if u == n then high else low)
                    (iterate d8 (+1) 0)


type N = 3
type TwoN = 2 ^ 3

-- | One hot encoder for N-bit values where N is a compile time constant.
--
-- > L.take 4 $ simulate onehotN [0..]
-- [<1,0,0,0,0,0,0,0>,<0,1,0,0,0,0,0,0>,<0,0,1,0,0,0,0,0>,<0,0,0,1,0,0,0,0>]
--
onehotN
  :: Signal (Unsigned N)
  -> Signal (BitVector TwoN)
onehotN = mealy onehotT ()
  where onehotT :: () -> Unsigned N -> ((), BitVector TwoN)
        onehotT () n = ((), oh n)

        oh n = shiftL 1 (fromIntegral n)


-- More complicated example ----------------------------------------------------
--
-- Drive 3 banks of LEDs using the one-hot encoding composed with a 0-7
-- counter. Each bank gets a slightly permuted version of the encoding.
--


perm1
  :: Signal (BitVector TwoN)
  -> Signal (BitVector TwoN)
perm1 = mealy permT ()
  where permT () v = ((), v2bv (reverse (rotateLeft (bv2v v) 3)))

perm2
  :: Signal (BitVector TwoN)
  -> Signal (BitVector TwoN)
perm2 = mealy permT ()
  where permT () v = ((), v2bv (rotateLeft (bv2v v) 4))

type BigN = 26

-- | Take the top 3 bits off of a 26-bit counter.
counterSlow
  :: Signal (Unsigned N)
counterSlow = mealy counterT 0 (signal ())
  where counterT :: Unsigned BigN -> () -> (Unsigned BigN, Unsigned N)
        counterT x () = (x+1, resize (shiftR x r))

        r = fromIntegral (natVal (Proxy :: Proxy (BigN-N)))

-- | Drive three banks of LEDs
driver
  :: (Signal (BitVector TwoN), Signal (BitVector TwoN), Signal (BitVector TwoN))
driver = (b1, b2, b3)
  where oh = onehotN counterSlow
        b1 = oh
        b2 = perm1 oh
        b3 = perm2 oh

topEntity
  :: Signal ()
  -> (Signal (BitVector TwoN), Signal (BitVector TwoN), Signal (BitVector TwoN))
topEntity _i = driver


