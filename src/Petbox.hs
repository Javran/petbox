{-# LANGUAGE
    FlexibleContexts
  #-}
module Petbox
  ( (^!)
  , fInt
  , sq
  , halve
  , digitLen
  , digitsToInt
  , firstSuchThat
  , lastSuchThat
  , keepInput
  , add2DCoords
    -- from Data.List
  , permutations
  , unfoldr
  , module Math.NumberTheory.Primes
  ) where

import Data.List
import Math.NumberTheory.Primes

-- | same as 'fromIntegral'
fInt :: (Integral a, Num b) => a -> b
fInt = fromIntegral
{-# INLINABLE fInt #-}

-- | @sq x = x * x@
sq :: Integral a => a -> a
sq x = x * x
{-# INLINABLE sq #-}
{-# SPECIALIZE sq :: Int -> Int #-}
{-# SPECIALIZE sq :: Integer -> Integer #-}

-- | same as '(^)' but requires its
--   second argument to be an "Int"
(^!) :: Num a => a -> Int -> a
(^!) = (^)
{-# INLINABLE (^!) #-}

-- | @halve x = x `div` 2@
halve :: Integral a => a -> a
halve = (`div` 2)
{-# INLINABLE halve #-}

-- | the length of the number in base 10
digitLen :: Integral a => a -> Int
digitLen x = 1 + floor (log (fromIntegral x) / (log 10 :: Double))
{-# INLINABLE digitLen #-}
{-# SPECIALIZE digitLen :: Int -> Int #-}
{-# SPECIALIZE digitLen :: Integer -> Int #-}

-- | concatenates a list of digits to form an integer
digitsToInt :: Integral a => [Int] -> a
digitsToInt = foldl' (\acc i -> acc*10 + fInt i) 0
{-# INLINABLE digitsToInt #-}
{-# SPECIALIZE digitsToInt :: [Int] -> Int #-}
{-# SPECIALIZE digitsToInt :: [Int] -> Integer #-}

-- | takes the first element that meet the requirement
firstSuchThat :: (a -> Bool) -> [a] -> a
firstSuchThat f = head . dropWhile (not . f)
{-# INLINABLE firstSuchThat #-}

-- | takes the last one in a consecutive sequence
--   whose elements satisfy the requirement
lastSuchThat :: (a -> Bool) -> [a] -> a
lastSuchThat f = last . takeWhile f
{-# INLINABLE lastSuchThat #-}

-- | modifies a function to return not just its output
--   but also its input
keepInput :: (a -> b) -> a -> (a,b)
keepInput f x = (x, f x)
{-# INLINABLE keepInput #-}

-- | add coordinates to a 2D list
add2DCoords :: (Enum e1, Enum e2)
            => e1 -> e2 -> [[a]] -> [[((e1,e2),a)]]
add2DCoords d1 d2 =
  zipWith
    (\rowN -> zipWith (\colN x -> ((rowN,colN),x)) [d2..])
    [d1..]
{-# INLINABLE add2DCoords #-}
