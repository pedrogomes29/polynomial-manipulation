module Main where
import Polynomial
import Control.Monad  (when)
import System.Exit   (exitFailure)
import Test.QuickCheck       

prop_associative_sum::Polynomial -> Polynomial -> Polynomial -> Bool

prop_associative_sum x y z = sumPol x (sumPol y z) == sumPol (sumPol x y) z



main :: IO ()
main = do
        let tests = [
                      quickCheckResult (withMaxSuccess 10000 prop_associative_sum)
                    ]
        success <- fmap (all isSuccess) . sequence $ tests
        when (not success) $ exitFailure
