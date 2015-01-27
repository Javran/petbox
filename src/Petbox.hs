{-# LANGUAGE TupleSections #-}
module Petbox
  ( (^!)
  , fInt
  , sq
  , halve
  , digitLen
  , factorial
  , isolateNthElement
  , digitsToInt
  , firstSuchThat
  , lastSuchThat
  , eUnfoldr
  , keepInput
  , divisible
  , factorization
  , allDigits
  , toDigits
    -- from Data.List
  , permutations
  , unfoldr
  ) where

import Control.Applicative
import Control.Arrow
import Data.List
import Data.Numbers.Primes

-- | short for "fromIntegral"
fInt :: (Integral a, Num b) => a -> b
fInt = fromIntegral

-- | @sq x = x * x@
sq :: Integral a => a -> a
sq x = x * x

-- | same as "(^)" but requires its
--   second argument to be an "Int"
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

-- | calculates factorial
factorial :: Integral a => Int -> a
factorial x = product [1..fInt x]

-- | @halve x = x `div` 2@
halve :: Integral a => a -> a
halve = (`div` 2)

-- | the length of the number in base 10
digitLen :: (Show a, Integral a) => a -> Int
digitLen = length . map (:[]) . show

-- | concatenates a list of digits to form an integer
digitsToInt :: Integral a => [Int] -> a
digitsToInt = foldl' (\acc i -> acc*10 + fInt i) 0

-- | fetches the n-th element from a list,
--   the rest of the elements are kept as well.
isolateNthElement :: Int -> [a] -> (a,[a])
isolateNthElement _ [] = error "list cannot be empty"
isolateNthElement 0 (x:xs) = (x,xs)
isolateNthElement n (x:xs) = (x1,x:xs1)
  where
    (x1,xs1) = isolateNthElement (n-1) xs

-- | takes the first element that meet the requirement
firstSuchThat :: (a -> Bool) -> [a] -> a
firstSuchThat f = head . dropWhile (not . f)

-- | takes the last one in a consecutive sequence
--   whose elements satisfy the requirement
lastSuchThat :: (a -> Bool) -> [a] -> a
lastSuchThat f = last . takeWhile f

-- | like "unfoldr" but keeps seeds
eUnfoldr :: (b -> Maybe (a,b)) -> b -> [(a,b)]
eUnfoldr f = unfoldr f'
  where
    f' x = keepInput snd <$> f x

-- | modifies a function to return not just its output
--   but also its input
keepInput :: (a -> b) -> a -> (a,b)
keepInput f x = (x, f x)

-- | @divisible x y@ checks if "x" is divisible by "y"
divisible :: Integral a => a -> a -> Maybe a
divisible x y = if k*y == x then Just y else Nothing
  where
    k = x `div` y

-- | naive factorization
factorization :: Integral a => a -> [(a, Int)]
factorization n = factorization' n primes
  where
    factorization' _ [] = undefined
    factorization' 1 _  = []
    factorization' m (p:ps) =
        if pTimes == 0
          then factorization' m ps
          else (p,pTimes) : factorization' (m `div` prod) ps
      where
        (pTimes,prod) = lastSuchThat ((== 0) . (m `mod`) . snd)
                      . zip [0..]
                      $ iterate (* p) 1

-- | convert an integer to a list of digits
toDigits :: Integral a => a -> [Int]
toDigits = reverse . allDigits

-- | convert an integer to a list of digits
--   the order is not guaranteed to be preserved.
allDigits :: Integral a => a -> [Int]
allDigits = map snd
          . takeWhile notAllZero
          . tail
          . iterate splitInt
          . (,0)
  where
    splitInt :: Integral a => (a,Int) -> (a,Int)
    splitInt = second fromIntegral . (`quotRem` 10) . fst
    notAllZero (a,b) = a /= 0 || b /= 0
