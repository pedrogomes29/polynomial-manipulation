module Main where
import Polynomial
import Control.Monad  (when)
import System.Exit   (exitFailure)
import Test.QuickCheck       


prop_commutative::Polynomial -> Polynomial  -> Bool

prop_commutative x y = (sumPol x y) == (sumPol y x)


main :: IO ()
main = do
        let tests = [ quickCheckResult prop_commutative
                    ]
        success <- fmap (all isSuccess) . sequence $ tests
        when (not success) $ exitFailure



