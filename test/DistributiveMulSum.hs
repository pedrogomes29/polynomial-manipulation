module Main where
import Polynomial
import Control.Monad  (when)
import System.Exit   (exitFailure)
import Test.QuickCheck       

prop_distributive_mul_sum:: Polynomial -> Polynomial -> Polynomial -> Bool

prop_distributive_mul_sum x y z = mulPol x (sumPol y z) == sumPol (mulPol x y) (mulPol x z)


main :: IO ()
main = do
        let tests = [
                      quickCheckResult prop_distributive_mul_sum
                    ]
        success <- fmap (all isSuccess) . sequence $ tests
        when (not success) $ exitFailure
