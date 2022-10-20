import Data.Char
import Data.List
import System.Exit (exitSuccess)



type Coef = Int
type Exponent = Int
type Var = Char
type Monomial = (Coef,[(Var,Exponent)])
type Polynomial = [Monomial]

seperator::Char

isPartOfNumber::Char->Bool

cleanInput:: String -> String
splitInput:: String -> [String]
addSeperator::String -> String


orderMon:: Monomial-> Monomial

readVars:: String->[(Var,Exponent)]

parseMon:: String -> Monomial
parsePol:: String -> Polynomial

polToStr:: Polynomial -> String
monToStr:: Monomial -> String
printVars:: [(Var,Exponent)]->String
cleanOutput:: String -> String
printResult:: Polynomial -> String

sumPol:: Polynomial -> Polynomial -> Polynomial
subPol:: Polynomial -> Polynomial -> Polynomial
mulPol:: Polynomial -> Polynomial -> Polynomial
normPol:: Polynomial -> Polynomial
derivePolAux:: Polynomial -> Var -> Polynomial
derivePol:: Polynomial -> Var -> Polynomial


countEqualMon:: Monomial -> Polynomial -> Int
removeEqualMon:: Monomial -> Polynomial -> Polynomial

normVars:: [(Var,Exponent)] -> [(Var,Exponent)]
removeExpZero:: Polynomial -> Polynomial
removeCoefZero:: Polynomial -> Polynomial
sumExponentEqualVars:: Var ->  [(Var,Exponent)] -> Exponent
removeEqualVars:: Var -> [(Var,Exponent)] -> [(Var,Exponent)]

seperator = '?'

isPartOfNumber c = isDigit c || c=='+' || c=='-'

addSeperator "" = []
addSeperator (x:xs) |x=='^' = x:(head xs:addSeperator (tail xs))
                    |x=='-' || x=='+' = seperator:(x:addSeperator xs)
                    |otherwise = x:addSeperator xs

cleanInput xs     | head cleaned=='-' || head cleaned=='+' = cleaned 
                  | otherwise = '+':cleaned
                        where cleaned = filter (\x -> x/='*' && x/=' ') xs

splitInput "" = []
splitInput s = mon1: splitInput (if mon2=="" then "" else tail mon2)
               where (mon1,mon2) = break (seperator==) s



readVars "" = []
readVars [x] = [(x,1)] --if last variable has exponent 1 (ommited)
readVars s = if head (tail s)=='^' 
             then (head var, if sign=='-' then -read absExponent else read exponent): readVars otherVars 
             else (head s,1) :readVars (tail s) -- if the next character isn't a carat (^), then the head is a variable with exponent 1 ommited
                where (var,carat:rest) = break('^'==) s  -- carat is the symbol '^'
                      exponent = takeWhile isPartOfNumber rest
                      (sign:absExponent) = exponent
                      otherVars = dropWhile isPartOfNumber rest

parsePol =  normPol . removeExpZero . map (orderMon . parseMon) . tail . splitInput . addSeperator . cleanInput

parseMon s = (if sign=='-' then -read absCoef else read absCoef,readVars rest)
            where (sign:auxCoef) = takeWhile isPartOfNumber s
                  absCoef = if not (null auxCoef) then auxCoef else "1"
                  rest = dropWhile isPartOfNumber s


monToStr (coef,rest)    | coef==(-1) = "- " ++ printVars rest
                        | coef==1 = "+ " ++ printVars rest
                        | coef<0 = "- " ++ show (abs coef) ++ printVars rest
                        | otherwise =  "+ " ++ show coef ++ printVars rest

printVars [] = ""
printVars ((var,exponent):xs)   | exponent == 1 =  var: printVars xs
                                | otherwise = [var] ++ "^" ++ show exponent ++ printVars xs

printResult = cleanOutput . polToStr  

cleanOutput ('+':' ':mm) = mm
cleanOutput ('-':' ':mm) = '-':mm
cleanOutput mm = mm

polToStr [] = ""
polToStr [x] = monToStr x
polToStr (x:xs) = monToStr x ++ " " ++  polToStr xs
                  
orderMon (coef,vars) = (coef,sortBy (\(var1,exp1) (var2,exp2) -> compare var1 var2) vars)


countEqualMon (coef1,vars1) [] = 0
countEqualMon (coef1,vars1) ((coef2,vars2):xs) | vars1==vars2 = coef2 + countEqualMon (coef1,vars1) xs
                                               | otherwise = countEqualMon (coef1,vars1) xs

removeEqualMon mon [] = []
removeEqualMon (coef1,vars1) ((coef2,vars2):xs) | vars1==vars2 = removeEqualMon (coef1,vars1) xs
                                                | otherwise = (coef2,vars2):removeEqualMon (coef1,vars1) xs

normPol [] = []
normPol ((coef,vars):xs) | coef==0 = normPol xs
                         | otherwise = (coef+countEqualMon (coef,vars) xs,vars):normPol (removeEqualMon (coef,vars) xs) 

sumPol p1 p2 = removeCoefZero (normPol (p1++p2))

subPol p1 p2 = removeCoefZero (normPol (p1++map (\(coef,vars) -> (-coef,vars)) p2))

mulPol p1 p2 = removeExpZero (normPol [orderMon (mulMon m1 m2) | m1<-p1 ,m2<-p2])

sumExponentEqualVars var1 xs = sum [exponent2|(var2,exponent2)<-xs,var1==var2]

removeEqualVars var1 xs = [(var2,exponent2)|(var2,exponent2)<-xs,var1/=var2]

normVars [] = []
normVars ((var,exp):restVars) = (var,exp+(sumExponentEqualVars var restVars)):normVars(removeEqualVars var restVars)


mulMon (coef1,vars1) (coef2,vars2) = (coef1*coef2,normVars (vars1++vars2))

derivePolAux [] _ = []
derivePolAux ((coef,vars):xs) var = if exp==0 then derivePolAux xs var
                                 else orderMon((coef*exp,varsAfterDer)):derivePolAux xs var 
                                 where    exp = (sumExponentEqualVars var vars)  -- actually just gets the exponent of the var, pol is normalized
                                          varsAfterDer = (var,exp-1):(filter (\(v,e) -> v/=var) vars)


derivePol xs var = removeExpZero (derivePolAux xs var)


removeExpZero p = [(coef, [(var,exp)|(var,exp)<-vars,exp/=0]) | (coef,vars) <- p]
removeCoefZero p = [(coef,vars)|(coef,vars)<-p,coef/=0]


main :: IO ()
main = do
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