module Main where

import Luhn
import Test.QuickCheck

main = do
    quickCheck prop_checkLuhn
    quickCheck prop_checkSingleError
