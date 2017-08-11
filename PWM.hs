{-# LANGUAGE DataKinds #-}

module PWM where

import CLaSH.Prelude


-- Counter ---------------------------------------------------------------------

-- mealy :: (s -> i -> (s,o))
--       -> s
--       -> (Signal i -> Signal o)

-- | Count up from zero.
--
-- > L.take 4 $ simulate counter (L.replicate 4 ())
-- [0,1,2,3]
--
counter = mealy counterT 0
  where
    counterT
      :: Num a
      => a      -- ^ current state
      -> ()     -- ^ input
      -> (a,a)  -- ^ new state, output
    counterT x () = (x+1, x)


type N = 3

-- | Specialize to 3-bit unsigned numbers
--
-- > L.take 10 $ simulate counterUnsigned3 (L.replicate 10 ())
-- [0,1,2,3,4,5,6,7,0,1]
--
counterUnsigned3 :: Signal (Unsigned N)
counterUnsigned3 = counter (signal ())

-- | PWM based on the 'counterUnsigned3' counter above.
--
-- > sampleN 10 (pwm 3 counterUnsigned3)
-- [True,True,True,False,False,False,False,False,True,True]
pwm
  :: (Ord a, Num a)
  => a            -- ^ cutoff threshold
  -> Signal a     -- ^ underlying counter
  -> Signal Bool
pwm t c = c .<. tsig
  where
    tsig = signal t
