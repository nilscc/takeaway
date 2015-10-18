{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Takeaway.Data.Acid.ID_v0 where

import Data.IntSet (IntSet)
import Data.SafeCopy
import Data.Typeable

type ID = Int

data IdManager = IdManager
  { allIds :: IntSet
  }
  deriving (Show, Typeable)

deriveSafeCopy 0 'base ''IdManager
