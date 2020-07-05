{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bench
  ( main
  )
where

import Control.DeepSeq
import Criterion.Main
import Petbox

main :: IO ()
main =
  defaultMain
    [ bgroup
        "digitLen"
        [ bench "maxBound Int" $
            nf digitLen (maxBound :: Int)
        , bench "small Integer" $
            nf digitLen (2 ^! 1000 :: Integer)
        ]
    , bg "pickInOrder" pickInOrder
    , bg "pickInOrder'" pickInOrder'
    ]
  where
    bg tag (myPickInOrder :: forall a. NFData a => [a] -> [] (a, [a])) =
      bgroup
        tag
        [ bench "short list" $
            nf (myPickInOrder :: [Char] -> [(Char, [Char])]) "abcdefg"
        , bench "long list" $
            nf (myPickInOrder :: [Integer] -> [(Integer, [Integer])]) $
              take 2000 $ let xs = 0 : 1 : zipWith (+) xs (tail xs) in (xs :: [Integer])
        ]
