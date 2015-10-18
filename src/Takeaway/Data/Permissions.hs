module Takeaway.Data.Permissions where

import Data.Bits

data Rights
  = Read
  | Add
  | Change
  | Delete
  | (:&) Rights Rights
  deriving (Show)

allRights :: Rights
allRights = Read :& Add :& Change :& Delete

newtype Bitflag a = Bitflag { bitflag :: Word }
  deriving (Eq, Show)

rightsToWord :: Rights -> Word
rightsToWord r = case r of
  Read    -> 1
  Add     -> 2
  Change  -> 4
  Delete  -> 8
  a :& b  -> rightsToWord a .|. rightsToWord b

fromRights :: Rights -> Bitflag Rights
fromRights = Bitflag . rightsToWord

-- | Set new rights
setRights :: Bitflag Rights -> Rights -> Bitflag Rights
setRights (Bitflag w) r = Bitflag $ rightsToWord r .|. w

-- | Remove rights
remRights :: Bitflag Rights -> Rights -> Bitflag Rights
remRights (Bitflag w) r = Bitflag $ complement (rightsToWord r) .&. w

-- | Test if bitflag contains all rights
hasRights :: Bitflag Rights -> Rights -> Bool
hasRights (Bitflag w) r = let r' = rightsToWord r in (r' .&. w) == r'

-- | No rights at all
noRights :: Bitflag Rights
noRights = Bitflag 0

data Permission = Permission
  { owner :: Bitflag Rights
  , group :: Bitflag Rights
  , other :: Bitflag Rights
  }

-- | Default permission with all rights for owner but none for group/other
defaultPermission :: Permission
defaultPermission = Permission
  { owner = fromRights allRights
  , group = noRights
  , other = noRights
  }
