module Bench
  ( main
  ) where

import Criterion.Main

import Petbox

setupEnv :: IO ([Int], [Integer])
setupEnv = do
  let small0 = [1..100000]
      small1 = [100000..200000]
  pure (small0, small1)

main :: IO ()
main = defaultMain
  [ env setupEnv $ \ ~(small0,small1) ->
    bgroup "digitLen"
      [ bench "small Int" $
          nf (fmap digitLen) small0
      , bench "small Integer" $
          nf (fmap digitLen) small1
      ]
  ]
