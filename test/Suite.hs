module Main where

import Test.Framework (defaultMain)

import qualified Teatros.Tests

main :: IO ()
main = defaultMain [ Teatros.Tests.tests ]
