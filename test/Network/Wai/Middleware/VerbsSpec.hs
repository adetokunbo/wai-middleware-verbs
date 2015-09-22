module Network.Wai.Middleware.VerbsSpec (spec) where

import Network.Wai.Middleware.Verbs

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances


spec :: TestTree
spec = testGroup "Network.Wai.Middleware.Verbs"
  [ QC.testProperty "`someFunction` should pass"
      someFunction
  ]

someFunction :: Bool -> Property
someFunction x = not (not $ x) === x
