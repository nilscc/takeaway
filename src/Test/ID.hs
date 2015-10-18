{-# LANGUAGE TemplateHaskell #-}

module Test.ID where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString, int64LE)

import Test.QuickCheck

import Takeaway.Data.ID

--
-- Test properties
--

prop_compareIntegerConversionToByteStringBuilder :: Int -> Bool
prop_compareIntegerConversionToByteStringBuilder i =
  let
    i64 = fromIntegral i
    bs  = toStrict . toLazyByteString $ int64LE i64
  in i == bsToInt bs

--
-- Run tests
--

return []

idTests :: IO Bool
idTests = $quickCheckAll
