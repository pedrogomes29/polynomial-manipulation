module Main where
import Polynomial
import Control.Monad  (when)
import System.Exit   (exitFailure)
import Test.QuickCheck       

prop_commutative_mul::Polynomial -> Polynomial -> Bool

prop_commutative_mul x y = mulPol x y == mulPol y x



main :: IO ()
main = do
        let tests = [
                      quickCheckResult prop_commutative_mul
                    ]
        success <- fmap (all isSuccess) . sequence $ tests
        when (not success) $ exitFailure
