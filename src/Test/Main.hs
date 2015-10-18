import Control.Monad
import Data.IORef
import System.Exit

-- local tests
import Test.Permissions

main :: IO ()
main = checkAll $ \run -> do
  run permissionTests

-- | Supply 'run' method to run `IO Bool` actions, but check all results and
-- exit if any of them failed (but do not exit early, all actions are
-- performed)
checkAll :: ((IO Bool -> IO ()) -> IO ()) -> IO ()
checkAll f = do
  ref <- newIORef True
  f $ \io -> do
    success <- io
    modifyIORef ref (&& success)
  success <- readIORef ref
  unless success exitFailure
