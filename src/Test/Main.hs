import Control.Monad
import System.Exit

-- local tests
import Test.Permissions

main :: IO ()
main = do
  assert permissionTests
 where
  assert io = do
    success <- io
    unless success exitFailure
