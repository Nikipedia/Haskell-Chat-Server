import Test.QuickCheck
import qualified Data.Text as T
import Lib
main :: IO ()
main = verboseCheck(\action -> deserialize (serialize action) == Right action)

instance Arbitrary ClientAction where
    arbitrary = oneof 
        [ pure Quit
        , do Message .T.pack <$> arbitrary
        , do Nickname .T.pack <$> arbitrary
        ]