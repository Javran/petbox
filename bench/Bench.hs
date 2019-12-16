module Bench
  ( main
  ) where

import Criterion.Main

import Petbox

main :: IO ()
main = defaultMain
  [ bgroup "digitLen"
      [ bench "maxBound Int" $
          nf digitLen (maxBound :: Int)
      , bench "small Integer" $
          nf digitLen (2 ^! 1000 :: Integer)
      ]
  , bgroup "pickInOrder"
      [ bench "short list" $
          nf pickInOrder "abcdefg"
      , bench "long list" $
          nf pickInOrder $
            take 2000 $ let xs = 0 : 1 : zipWith (+) xs (tail xs) in (xs :: [Integer])
      ]
  ]
