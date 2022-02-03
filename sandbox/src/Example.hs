{-# OPTIONS_GHC -fplugin Plugin.CurryPlugin #-}
{-# LANGUAGE NoImplicitPrelude              #-}
module Example where

import Plugin.CurryPlugin.Prelude hiding (not)

not :: Bool -> Bool
not True  = False
not False = True

notTwice :: Bool -> Bool
notTwice x = not (not x)