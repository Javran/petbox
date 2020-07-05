{-# LANGUAGE FlexibleContexts #-}

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
  , -- from Data.List
    permutations
  , unfoldr
  , -- moved from ProjectEuler.SolCommon
    factorials
  , factorial
  , intToDigits
  , intToDigitsRev
  , pick
  , pickInOrder
  , numReverseInBase
  , primes
  , pickInOrder'
  , slidingWindows
  , choose
  )
where

import Data.List
import qualified Math.NumberTheory.Primes as Primes

primes :: Integral i => [i]
primes = Primes.unPrime <$> Primes.primes
{-# SPECIALIZE primes :: [Int] #-}
{-# SPECIALIZE primes :: [Integer] #-}

-- | same as 'fromIntegral'
fInt :: (Integral a, Num b) => a -> b
fInt = fromIntegral
{-# INLINEABLE fInt #-}

-- | @sq x = x * x@
sq :: Integral a => a -> a
sq x = x * x
{-# INLINEABLE sq #-}
{-# SPECIALIZE sq :: Int -> Int #-}
{-# SPECIALIZE sq :: Integer -> Integer #-}

-- | same as '(^)' but requires its
--   second argument to be an "Int"
(^!) :: Num a => a -> Int -> a
(^!) = (^)
{-# INLINEABLE (^!) #-}

-- | @halve x = x `div` 2@
halve :: Integral a => a -> a
halve = (`div` 2)
{-# INLINEABLE halve #-}

-- | the length of the number in base 10
digitLen :: Integral a => a -> Int
digitLen x = 1 + floor (log (fromIntegral x) / (log 10 :: Double))
{-# INLINEABLE digitLen #-}
{-# SPECIALIZE digitLen :: Int -> Int #-}
{-# SPECIALIZE digitLen :: Integer -> Int #-}

-- | concatenates a list of digits to form an integer
digitsToInt :: Integral a => [Int] -> a
digitsToInt = foldl' (\acc i -> acc * 10 + fInt i) 0
{-# INLINEABLE digitsToInt #-}
{-# SPECIALIZE digitsToInt :: [Int] -> Int #-}
{-# SPECIALIZE digitsToInt :: [Int] -> Integer #-}

-- | takes the first element that meet the requirement
firstSuchThat :: (a -> Bool) -> [a] -> a
firstSuchThat f = head . dropWhile (not . f)
{-# INLINEABLE firstSuchThat #-}

-- | takes the last one in a consecutive sequence
--   whose elements satisfy the requirement
lastSuchThat :: (a -> Bool) -> [a] -> a
lastSuchThat f = last . takeWhile f
{-# INLINEABLE lastSuchThat #-}

-- | modifies a function to return not just its output
--   but also its input
keepInput :: (a -> b) -> a -> (a, b)
keepInput f x = (x, f x)
{-# INLINEABLE keepInput #-}

-- | add coordinates to a 2D list
add2DCoords
  :: (Enum e1, Enum e2)
  => e1
  -> e2
  -> [[a]]
  -> [[((e1, e2), a)]]
add2DCoords d1 d2 =
  zipWith
    (\rowN -> zipWith (\colN x -> ((rowN, colN), x)) [d2 ..])
    [d1 ..]
{-# INLINEABLE add2DCoords #-}

factorials :: [Int]
factorials = scanl (*) 1 [1 ..]
{-# INLINEABLE factorials #-}

factorial :: Int -> Int
factorial = (factorials !!)
{-# INLINEABLE factorial #-}

intToDigits :: Integral i => i -> [Int]
intToDigits x = ($ []) . foldr (.) id $ unfoldr f x
  where
    f 0 = Nothing
    f n = let (q, r) = n `quotRem` 10 in Just ((++ [fromIntegral r]), q)
{-# INLINEABLE intToDigits #-}
{-# SPECIALIZE intToDigits :: Int -> [Int] #-}

intToDigitsRev :: Integral i => i -> [Int]
intToDigitsRev = unfoldr f
  where
    f 0 = Nothing
    f n = let (q, r) = n `quotRem` 10 in Just (fromIntegral r, q)
{-# INLINEABLE intToDigitsRev #-}
{-# SPECIALIZE intToDigitsRev :: Int -> [Int] #-}
{-# SPECIALIZE intToDigitsRev :: Integer -> [Int] #-}

-- | non-deterministically picking an element from the given list,
--   separating the selected element and all other remaining elements
--   the list order is preserved
--   e.g. pick [1,2,3] == [(1,[2,3]),(2,[1,3]),(3,[1,2])]
pick :: [a] -> [(a, [a])]
pick xs = map split (init $ zip (inits xs) (tails xs))
  where
    split (ls, v : rs) = (v, ls ++ rs)
    split _ = error "cannot split empty list"
{-# INLINEABLE pick #-}

-- convert to reversed list of digits and put digits together,
-- this allows digit-wise reversal of numbers
-- and gives a compact representation (i.e. the number itself) to work with
-- rather than comparing on list of stuff.
numReverseInBase :: Integral i => Int -> i -> i
numReverseInBase base = foldl (\a b -> a * base' + b) 0 . unfoldr f
  where
    base' = fromIntegral base
    f 0 = Nothing
    f n = let (q, r) = n `quotRem` base' in Just (r, q)
{-# INLINEABLE numReverseInBase #-}
{-# SPECIALIZE numReverseInBase :: Int -> Int -> Int #-}
{-# SPECIALIZE numReverseInBase :: Int -> Integer -> Integer #-}

{-
  like "pick", but whenever an element picked,
  all elements before it will be dropped. This has the effect of only picking
  elements in order.
 -}
pickInOrder :: [a] -> [] (a, [a])
pickInOrder xs = do
  (y : ys) <- tails xs
  pure (y, ys)
{-# INLINEABLE pickInOrder #-}

{-
  like "pickInOrder" but the element being picked is not removed from the list,
  therefore has the effect of allowing a previously picked element to be picked again.
 -}
pickInOrder' :: [a] -> [] (a, [a])
pickInOrder' = fmap (\(x : xs) -> (x, x : xs)) . init . tails
{-# INLINEABLE pickInOrder' #-}

{-
  sliding a window of n elements against a list xs,
  note that xs has to be a finite list, as
  this function computes the position to truncate
  so that each window is exactly the expected length.
 -}
slidingWindows :: Int -> [a] -> [[a]]
slidingWindows n xs =
  take (l - n + 1)
    . map (take n)
    . iterate tail
    $ xs
  where
    l = length xs
{-# INLINEABLE slidingWindows #-}

choose :: Integral i => i -> i -> i
choose n k =
  foldl' (\acc (x, y) -> (acc * x) `quot` y) 1 $ zip [n, n -1 ..] [1 .. k]
{-# INLINEABLE choose #-}
{-# SPECIALIZE choose :: Int -> Int -> Int #-}
{-# SPECIALIZE choose :: Integer -> Integer -> Integer #-}
