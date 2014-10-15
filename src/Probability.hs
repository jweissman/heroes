{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables, OverlappingInstances #-}
module Probability where
    import Control.Monad
    import System.Random
    --
    -- probabilities
    -- 
    rollDie :: Integer -> IO Integer
    rollDie faces = getStdRandom (randomR (1,faces)) 

    rollDice :: Int -> Integer -> IO [Integer]
    rollDice n faces = replicateM n (rollDie faces)

    -- some sweet 'tax..
    d = rollDice

    --select random elems from bounded enums like r <- randomIO :: Klass
    instance (Bounded a, Enum a) => Random a where
       random = randomR (minBound :: a, maxBound :: a)
       randomR (f, t) gen =
         (toEnum r :: a, nextGen)
         where
           (rnd, nextGen) = next gen
           r = fromEnum f + (rnd `mod` length [f..t])
       
    randomIndex l = getStdRandom (randomR (0, length l - 1))

    pickFrom :: [a] -> IO a
    pickFrom l = do
      index <- randomIndex l
      return (l !! index)