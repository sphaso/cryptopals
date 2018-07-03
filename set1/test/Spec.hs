import Test.Hspec
import Test.QuickCheck
import Control.Exception
import Data.ByteString.Lazy.Char8

import qualified Set1

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Test" Set1.firstExercise
