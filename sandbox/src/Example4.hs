{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin Plugin.CurryPlugin #-}

module Example4 (fib) where

import Plugin.CurryPlugin.Prelude hiding (not)

----------- Fib -----------

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
