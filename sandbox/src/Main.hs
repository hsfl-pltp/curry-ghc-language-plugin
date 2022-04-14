{-# LANGUAGE TemplateHaskell #-}

module Main where

import Example
import Plugin.CurryPlugin.Encapsulation

main :: IO ()
main = print res
  where
    res = $(evalGeneric DFS 'notTwice) False