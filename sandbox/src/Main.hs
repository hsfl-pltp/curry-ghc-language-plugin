{-# LANGUAGE TemplateHaskell #-}

{- OPTIONS_GHC -fno-full-laziness #-}

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
      bench
        "NotALot"
        $ nf
          $(evalGeneric DFS 'notALot)
          True,
      bench "RSA" $
        nf
          $(evalGeneric DFS 'roundTripRSA)
          "Hello World! This is an example Text.  ",
      bench
        "Sort"
        $ nf
          $(evalGeneric DFS 'sort)
          [7 :: Int, 6, 5, 4, 3, 2, 1],
      bench "Fib" $
        nf
          $(evalGeneric DFS 'fib)
          19,
      bench
        "Queens"
        $ nf
          $(evalGeneric DFS 'nqueens)
          7
    ]

{-
main = print res
  where
    res = $(evalGeneric DFS 'fib) 32
-}