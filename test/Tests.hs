module Main where

import Luhn
import Text.Printf
import Test.QuickCheck

tests :: [(String, IO ())]
tests = [
    ("checkLuhn", quickCheck prop_checkLuhn),
    ("checkSingleError", quickCheck prop_checkSingleError)]

main :: IO ()
main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

