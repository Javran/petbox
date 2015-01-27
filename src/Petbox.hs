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

fInt :: (Integral a, Num b) => a -> b
fInt = fromIntegral

sq :: Integral a => a -> a
sq x = x * x

(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

factorial :: Integral a => Int -> a
factorial x = product [1..fInt x]

halve :: Integral a => a -> a
halve = (`div` 2)

digitLen :: (Show a, Integral a) => a -> Int
digitLen = length . map (:[]) . show

digitsToInt :: Integral a => [Int] -> a
digitsToInt = foldl' (\acc i -> acc*10 + fInt i) 0

isolateNthElement :: Int -> [a] -> (a,[a])
isolateNthElement _ [] = error "list cannot be empty"
isolateNthElement 0 (x:xs) = (x,xs)
isolateNthElement n (x:xs) = (x1,x:xs1)
  where
    (x1,xs1) = isolateNthElement (n-1) xs

firstSuchThat :: (a -> Bool) -> [a] -> a
firstSuchThat f = head . dropWhile (not . f)

lastSuchThat :: (a -> Bool) -> [a] -> a
lastSuchThat f = last . takeWhile f

eUnfoldr :: (b -> Maybe (a,b)) -> b -> [(a,b)]
eUnfoldr f = unfoldr f'
  where
    f' x = (\(a,b) -> ((a,b),b)) <$> f x

keepInput :: (a -> b) -> a -> (a,b)
keepInput f x = (x, f x)

divisible :: Integral a => a -> a -> Maybe a
divisible x y = if k*y == x then Just y else Nothing
  where
    k = x `div` y

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

toDigits :: Integral a => a -> [Int]
toDigits = reverse . allDigits

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
