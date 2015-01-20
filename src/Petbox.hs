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
  , eUnfoldr
  , keepInput
  , divisible
    -- from Data.List
  , permutations
  , unfoldr
  ) where

import Control.Applicative
import Data.List

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
