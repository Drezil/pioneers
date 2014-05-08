{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Map.Map
 
main :: IO ()
main = $(defaultMainGenerator)

prop_rd_idempot :: [Int] -> Bool
prop_rd_idempot xs = remdups xs == (remdups . remdups) xs

prop_rd_length :: [Int] -> Bool
prop_rd_length xs = length (remdups xs) <= length xs

prop_rd_sorted :: [Int] -> Property
prop_rd_sorted xs = (not . null) xs ==> head (remdups xs) == minimum xs
