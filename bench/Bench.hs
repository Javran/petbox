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
  ]
