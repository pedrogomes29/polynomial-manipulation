module Main where
import Polynomial
import Control.Monad  (when)
import System.Exit   (exitFailure)
import Test.QuickCheck       

prop_commutative_sum::Polynomial -> Polynomial -> Bool

prop_commutative_sum x y = sumPol x y == sumPol y x



main :: IO ()
main = do
        let tests = [
                      quickCheckResult (withMaxSuccess 10000 prop_commutative_sum)
                    ]
        success <- fmap (all isSuccess) . sequence $ tests
        when (not success) $ exitFailure
