{-# LANGUAGE TemplateHaskell #-}

module Test.Permissions where

import Control.Applicative
import Test.QuickCheck

import Takeaway.Data.Permissions

-- Make `Rights` testable
instance Arbitrary Rights where
  arbitrary = foldl1 (:&) <$>
    sublistOf [Read, Add, Change, Delete] `suchThat` (not . null)

--
-- Test properties
--

-- | All rights should be available in `allRights`
prop_allRights :: Rights -> Bool
prop_allRights r = fromRights allRights `hasRights` r

-- | Not a single right should be available in `noRights`
prop_noRights :: Rights -> Bool
prop_noRights r = not $ noRights `hasRights` r

-- | Set rights then test if it was set
prop_settingRights :: Rights -> Bool
prop_settingRights r = (noRights `setRights` r) `hasRights` r

-- | Remove rights then test if it was removed
prop_removingRights :: Rights -> Bool
prop_removingRights r = not $ (fromRights allRights `remRights` r) `hasRights` r

-- | Verify setting new rights works just like `fromRights` does
prop_setRightsComparedToFromRights :: Rights -> Bool
prop_setRightsComparedToFromRights r = (noRights `setRights` r) == fromRights r

--
-- Run all tests
--

return [] -- required!

permissionTests :: IO Bool
permissionTests = $quickCheckAll
