module Takeaway.Data.ID
  {- ( ID
  , newUniqueId
  ) -} where

import Crypto.Random
import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import Data.Binary.Get
--import Data.List (foldl')

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntSet as IntSet

import System.IO.Unsafe (unsafePerformIO)
import Foreign

-- local includes
import Takeaway.Data.Acid.ID

-- | Convert binary ByteString to Int (little endian)
bsToInt :: ByteString -> Int
bsToInt = fromIntegral . runGet getWord64le . BL.fromStrict

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
