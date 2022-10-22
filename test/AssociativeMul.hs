module Main where
import Polynomial
import Control.Monad  (when)
import System.Exit   (exitFailure)
import Test.QuickCheck       

prop_associative_mul::Polynomial -> Polynomial -> Polynomial -> Bool

prop_associative_mul x y z = mulPol x (mulPol y z) == mulPol (mulPol x y) z



main :: IO ()
main = do
        let tests = [
                      quickCheckResult prop_associative_mul
                    ]
        success <- fmap (all isSuccess) . sequence $ tests
        when (not success) $ exitFailure
