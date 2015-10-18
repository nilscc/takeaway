{-# LANGUAGE TemplateHaskell #-}

module Test.Permissions where

import Control.Applicative
import Test.QuickCheck

import Takeaway.Data.Permissions

-- Make `Rights` testable
instance Arbitrary Rights where
  arbitrary = foldl1 (:&) <$>
    sublistOf [Read, Add, Change, Delete] `suchThat` (not . null)

-- | Set rights then test
prop_hasRights :: Rights -> Bool
prop_hasRights r = (noRights `setRights` r) `hasRights` r

-- | Remove rights then test
prop_hasntRights :: Rights -> Bool
prop_hasntRights r = not $ (fromRights allRights `remRights` r) `hasRights` r

return []
permissionTests :: IO Bool
permissionTests = $quickCheckAll
