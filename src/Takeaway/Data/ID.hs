module Takeaway.Data.ID
  {- ( ID
  , newUniqueId
  ) -} where

import Crypto.Random
import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
--import Data.List (foldl')

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.IntSet as IntSet

import System.IO.Unsafe (unsafePerformIO)
import Foreign

-- local includes
import Takeaway.Data.Acid.ID

-- | Convert bytestring (8 byte size!) to Int using FFI to cast underlying
-- pointers (little endian)
bsToInt :: ByteString -> Int
bsToInt bs = fromIntegral $
  unsafePerformIO $ BS.useAsCStringLen bs $ \(cs, len) -> do
    if len /= 8 then
      return 0
     else
      peek (castPtr cs :: Ptr Int64)

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
