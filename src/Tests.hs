import Test.Framework (defaultMain)
import qualified Tests.Util as Util

-- | All test groups
main :: IO ()
main = defaultMain [ Util.tests ]

