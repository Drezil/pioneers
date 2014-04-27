module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
 
import Map.Map

main :: IO ()
main = defaultMain tests
 
tests :: [Test]
tests =
  [
    testGroup "Map.Map"
    [
       testProperty "remdups idempotency" prop_rd_idempot
    ]
  ]


