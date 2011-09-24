import Control.Monad.Writer
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Idme.Util (constF, (>>-))


main = defaultMain tests

tests = [ testGroup "Utility" [ testProperty "constF" prop_constF
                              , testProperty ">--"    prop_util_operator1 ] ]

-- | Test constF with Writer [Bool]
prop_constF :: Bool -> Bool
prop_constF x = let test = runWriter $ constF (tell . return) x
                in test == (x, [x])

-- | Test >>- with Writer [Bool]
prop_util_operator1 :: Bool -> Bool
prop_util_operator1 x = runWriter (return x >>- tell . return) == (x, [x])

