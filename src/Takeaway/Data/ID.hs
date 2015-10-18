module Takeaway.Data.ID
  ( ID
  , newUniqueId
  ) where

import Crypto.Random
import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import Data.List (foldl')

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.IntSet as IntSet

import Foreign.Storable

-- local includes
import Takeaway.Data.Acid.ID

bsToInt :: ByteString -> Int
bsToInt = foldl' (\i w8 -> i * 256 + fromIntegral w8) 0 . BS.unpack

newUniqueId :: CPRG gen => gen -> Update IdManager (ID, gen)
newUniqueId gen = do

  -- get random ID
  let (i, gen') = withRandomBytes gen (sizeOf (0 :: ID)) bsToInt

  -- check if it's unique
  unique <- liftQuery $ asks (IntSet.notMember i . allIds)
  if unique then do
    -- store ID, return result
    modify $ \idm -> idm { allIds = IntSet.insert i (allIds idm) }
    return (i, gen')
   else do
    -- repeat
    newUniqueId gen'
