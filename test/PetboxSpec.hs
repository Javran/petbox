{-# LANGUAGE
    TypeApplications
  #-}
module PetboxSpec where

import Test.Hspec
import Test.QuickCheck

import Petbox

spec :: Spec
spec =
  describe "digitLen" $
    specify "correctness" $
      property $
        \(Positive x) ->
          digitLen @Int x === length (show x)

