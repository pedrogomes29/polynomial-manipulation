import Polynomial
import System.Exit (exitSuccess)

main :: IO ()
main =  do
            putStrLn "Choose an option: "
            putStrLn "1. Add polynomials"
            putStrLn "2. Subtract polynomials"
            putStrLn "3. Multiply polynomials"
            putStrLn "4. Derive polynomials"
            putStrLn "5. Normalize polynomials"
            putStrLn "6. Exit"
            str <- getLine
            let option = read str
            if option==6 then do
                  exitSuccess
            else if option >6 || option <= 0 then do 
                  putStrLn "Invalid option"
            else do 
                  putStrLn "Insert a polynomial:"
                  pol1str <- getLine
                  let pol1 = parsePol pol1str
                  if option==5 then do
                        putStrLn (printResult pol1) 
                  else if option==4 then do
                        putStrLn "Insert the variable to derive by"
                        var <- getChar
                        putStrLn (printResult (derivePol pol1 var))
                  else do
                        putStrLn "Insert the second polynomial:"
                        pol2str <- getLine
                        let pol2 = parsePol pol2str 
                        if option==1 then do
                              putStrLn "The sum is:"
                              putStrLn (printResult (sumPol pol1 pol2))
                        else if option==2 then do
                              putStrLn "The difference is:"
                              putStrLn (printResult (subPol pol1 pol2))
                        else do
                              putStrLn "The product is:"
                              putStrLn (printResult (mulPol pol1 pol2))
                        
            main