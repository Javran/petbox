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

-- | @sq x = x * x@
sq :: Integral a => a -> a
sq x = x * x

-- | same as '(^)' but requires its
--   second argument to be an "Int"
(^!) :: Num a => a -> Int -> a
(^!) = (^)

-- | @halve x = x `div` 2@
halve :: Integral a => a -> a
halve = (`div` 2)

-- | the length of the number in base 10
digitLen :: (Show a, Integral a) => a -> Int
digitLen = length . map (:[]) . show

-- | concatenates a list of digits to form an integer
digitsToInt :: Integral a => [Int] -> a
digitsToInt = foldl' (\acc i -> acc*10 + fInt i) 0

-- | takes the first element that meet the requirement
firstSuchThat :: (a -> Bool) -> [a] -> a
firstSuchThat f = head . dropWhile (not . f)

-- | takes the last one in a consecutive sequence
--   whose elements satisfy the requirement
lastSuchThat :: (a -> Bool) -> [a] -> a
lastSuchThat f = last . takeWhile f

-- | modifies a function to return not just its output
--   but also its input
keepInput :: (a -> b) -> a -> (a,b)
keepInput f x = (x, f x)

-- | add coordinates to a 2D list
add2DCoords :: (Enum e1, Enum e2)
            => e1 -> e2 -> [[a]] -> [[((e1,e2),a)]]
add2DCoords d1 d2 =
  zipWith
    (\rowN -> zipWith (\colN x -> ((rowN,colN),x)) [d2..])
    [d1..]
