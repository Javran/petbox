{-# LANGUAGE
    TupleSections
  , FlexibleContexts
  #-}
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
  , sqrtI
  , fixData
  , fixMemo
  , add2DCoords
    -- from Data.List
  , permutations
  , unfoldr
  , module Data.Numbers.Primes
  ) where

import Control.Arrow
import Control.Monad.State
import Data.List
import Data.Numbers.Primes

import qualified Data.IntMap as IM

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

-- | @factorial n@ requires @n >= 0@
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

-- | @factorization n@, requires @n >= 2@
factorization :: Integral a => a -> [(a, Int)]
factorization = map (head &&& length) . group . primeFactors

-- | breaks an integer to a list of digits
--
--   prop> digitsToInt . toDigits = id = toDigits . digitsToInt
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

-- | square root for integers
sqrtI :: Integral a => a -> a
sqrtI = floor . (sqrt :: Double -> Double) . fromIntegral

-- | @fixData good next seed@ trys to recursively refine "seed" using "next"
--   until "good x" returns true
fixData :: (a -> Bool) -> (a -> a) -> a -> a
fixData good next = gen
  where
    gen x = if good x then x else gen (next x)

-- | use IntMap to memoize a function
fixMemo :: (Monad m, MonadState (IM.IntMap a) m, Enum e)
        => ((e -> m a) -> e -> m a) -> e -> m a
fixMemo f x = do
    let xe = fromEnum x
    val <- gets $ IM.lookup xe
    case val of
        Just v -> return v
        Nothing -> do
            v <- f (fixMemo f) x
            modify $ IM.insert xe v
            return v

-- | add coordinates to a 2D list
add2DCoords :: (Enum e1, Enum e2)
            => e1 -> e2 -> [[a]] -> [[((e1,e2),a)]]
add2DCoords d1 d2 =
  zipWith
    (\rowN -> zipWith (\colN x -> ((rowN,colN),x)) [d2..])
    [d1..]
