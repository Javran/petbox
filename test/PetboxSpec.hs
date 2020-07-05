{-# LANGUAGE
    TypeApplications
  , BinaryLiterals
  , ScopedTypeVariables
  #-}
module PetboxSpec where

import Control.Monad
import Data.List
import Test.Hspec
import Test.QuickCheck hiding (choose)
import Test.QuickCheck.Poly

import Petbox

spec :: Spec
spec = do
  describe "digitLen" $
    specify "correctness" $
      property $
        \(Positive x) ->
          digitLen @Int x === length (show x)

  describe "factorials" $
    specify "first few elements" $
      take 10 factorials
        `shouldBe` [1,1,2,6,24,120,720,5040,40320,362880]

  describe "intToDigits" $
    specify "examples" $ do
      intToDigits @Integer 1234
        `shouldBe` [1,2,3,4]
      intToDigits @Int 8
        `shouldBe` [8]
      intToDigits @Integer 43212345566
        `shouldBe` [4,3,2,1,2,3,4,5,5,6,6]

  describe "digitsToInt" $
    specify "identity" $
      property $
        \(Positive x) ->
          (digitsToInt . intToDigits @Int) x === x

  describe "intToDigitsRev" $
    specify "reversed intToDigits" $
      property $
        \(Positive x) ->
          intToDigits @Int x === reverse (intToDigitsRev x)

  describe "pick" $ do
    specify "empty" $
      pick @Bool [] `shouldBe` []
    specify "example" $
      pick @Int [4,1,2,3]
        `shouldBe` [(4,[1,2,3]),(1,[4,2,3]),(2,[4,1,3]),(3,[4,1,2])]
    specify "poly" $
      property $
        \(xs :: [OrdA]) ->
          let ys = pick xs
              sortedXs = sort xs
          in fmap fst ys === xs
             .&&. conjoin (fmap (\(h,t) -> sort (h:t) === sortedXs) ys)

  describe "numReverseInBase" $
    specify "examples" $ do
      numReverseInBase @Integer 16 0xAABBCCDD1234
        `shouldBe` 0x4321DDCCBBAA
      numReverseInBase @Int 2 0b11001011
        `shouldBe` 0b11010011
      numReverseInBase @Integer 10 987654321000
        `shouldBe` 123456789

  describe "pickInOrder" $
    specify "examples" $ do
      pickInOrder @() [] `shouldBe` []
      pickInOrder "ABCD" `shouldBe`
        [ ('A', "BCD")
        , ('B', "CD")
        , ('C', "D")
        , ('D', "")
        ]

  describe "pickInOrder'" $ do
    specify "example" $ do
      pickInOrder @() [] `shouldBe` []
      pickInOrder' "abcd1"
        `shouldBe`
          [ ('a', "abcd1")
          , ('b', "bcd1")
          , ('c', "cd1")
          , ('d', "d1")
          , ('1', "1")
          ]
    specify "correctness" $
      property $
        \(xs :: [A]) ->
          pickInOrder' xs === fmap (\(u,v) -> (u,u:v)) (pickInOrder xs)

  describe "slidingWindows" $
    specify "examples" $ do
      slidingWindows 3 "abcde"
        `shouldBe` ["abc", "bcd", "cde"]
      slidingWindows 4 "abcd"
        `shouldBe` ["abcd"]
      slidingWindows 10 "a"
        `shouldBe` []

  describe "choose" $ do
    specify "examples" $
      choose (37 :: Integer) 11 `shouldBe`
        (product (take 11 [37,36..]) `div` product [1..11])
    specify "small" $
      forM_ [2..10] $ \n -> do
        let xs = (n `choose`) <$> [0..n]
        take 3 xs `shouldBe` [1, n, n*(n-1) `div` 2]
        xs `shouldBe` reverse xs
        sum xs `shouldBe` 2 ^! n
