{-# LANGUAGE TemplateHaskell #-}

module Main where

import Criterion.Main
import Example3
import Plugin.CurryPlugin.Encapsulation

main :: IO ()
main =
  defaultMain
    [ bench "NotTwice" $
        nf
          $(evalGeneric DFS 'notTwice)
          True,
      bench "Fib" $
        nf
          $(evalGeneric DFS 'fib)
          25,
      bench "RSA" $
        nf
          $(evalGeneric DFS 'roundTripRSA)
          "Hello World!"
    ]

{-main = print res
  where
    res = $(evalGeneric DFS 'fib) 32-}