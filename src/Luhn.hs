{- |
Module      :  Luhn
Description :  An implementation of Luhn's check digit algorithm.
Copyright   :  (c) N-Sim Ltd. 2008
License     :  BSD3

Maintainer  :  jhb@n-sim.com
Stability   :  provisional
Portability :  portable

An implementation of Luhn's check digit algorithm.
-}
module Luhn(
    -- * Creating a check digit
    addLuhnDigit,
    -- * Validating a check digit
    checkLuhnDigit,
    -- * QuickCheck tests
    prop_checkLuhn,
    prop_checkSingleError
) where

import Data.Digits
import Test.QuickCheck hiding (total)

-- | Like Python's enumerate function - returns a tuple where the first
--   element is the index from 0 of the second element in the input list.
enumerate :: Integral n => [a] -> [(n, a)]
enumerate xs = enumerate' 0 xs
    where
        enumerate' _ [] = []
        enumerate' counter (a:as) =
            (counter, a) : enumerate' (counter + 1) as

-- | Appends a Luhn check digit to the end of a number.
addLuhnDigit :: Integral n
    => n -- ^ Number to which a Luhn check digit will be appended.
    -> n -- ^ Number with the appended Luhn check digit.
addLuhnDigit num = num * 10 + checkDigit
    where
        checkDigit = (10 - total `mod` 10) `mod` 10
        total = sum $ concat $ map doubleEven (enumerate $ digitsRev 10 num)
        doubleEven :: Integral n => (Int, n) -> [n]
        doubleEven (i, n) = if odd i
            then [n]
            else digitsRev 10 (2 * n)

-- | Validates that the Luhn check digit (assumed to be the last/least-
--   significant digit in the number) is correct.
checkLuhnDigit :: Integral n
    => n -- ^ Number with a Luhn check digit as its last digit.
    -> Bool -- ^ Whether or not the check digit is consistent.
checkLuhnDigit num = total `mod` 10 == 0
    where
        total = sum $ concat $ map doubleOdd (enumerate $ digitsRev 10 num)
        doubleOdd :: Integral n => (Int, n) -> [n]
        doubleOdd (i, n) = if odd i
            then digitsRev 10 (2 * n)
            else [n]

-- | Validates that a generated check digit validates.
prop_checkLuhn
    :: Integer -- ^ Number to validate a Luhn check digit for.
    -> Property
prop_checkLuhn i = i > 0 ==> (checkLuhnDigit . addLuhnDigit) i

-- | Any single number transcription error should result in a failure in
--   the validation of a Luhn check digit. This property validates this.
prop_checkSingleError
    :: Integer -- ^ The number to transcribe.
    -> Integer -- ^ The position to introduce a transcription error.
    -> Integer -- ^ The number to transcribe in place of the original.
    -> Property
prop_checkSingleError i modDigit replace = i > 0 ==>
    let checkNum = addLuhnDigit i
        checkDigits = digits 10 checkNum
        modDigit' = modDigit `mod` fromIntegral (length checkDigits - 1)
        start = take (fromIntegral modDigit') checkDigits
        rest = drop (fromIntegral (modDigit' + 1)) checkDigits
        newDigits = start ++ [replace `mod` 10] ++ rest
        newNum =  unDigits 10 newDigits
    in
    (newNum == checkNum) || (not $ checkLuhnDigit newNum)

