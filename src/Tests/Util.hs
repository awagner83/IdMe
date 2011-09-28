module Tests.Util (tests) where

import Control.Monad.Writer
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Idme.Util (constF, condM, (>>-), pairOf)

-- | Tests provided by this test-module
tests = testGroup "Utility"
    [ testProperty "constF" prop_constF
    , testProperty ">--"    prop_util_operator1
    , testProperty "pairOf" prop_pairOf
    , testProperty "condM"  prop_condM
    ]

-- | Test constF with Writer [Bool]
prop_constF :: Bool -> Bool
prop_constF x = let test = runWriter $ constF (tell . return) x
                in test == (x, [x])

-- | Test >>- with Writer [Bool]
prop_util_operator1 :: Bool -> Bool
prop_util_operator1 x = runWriter (return x >>- tell . return) == (x, [x])

prop_pairOf :: Bool -> Bool
prop_pairOf a = let p = pairOf a in fst p == a && snd p == a

prop_condM :: Integer -> Bool
prop_condM x = let test = runWriter $ condM x even (tell . return)
               in test == (if (even x) then (x, [x]) else (x, []))

