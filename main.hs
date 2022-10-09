import Data.Char

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

readVars:: String->[(Var,Exponent)]

parseMon:: String -> Monomial
parsePol:: String -> Polynomial

polToStr:: Polynomial -> String
monToStr:: Monomial -> String
printVars:: [(Var,Exponent)]->String

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

parsePol = map parseMon . tail . splitInput . addSeperator . cleanInput

parseMon s = (if sign=='-' then -read absCoef else read absCoef,readVars rest)
            where (sign:auxCoef) = takeWhile isPartOfNumber s
                  absCoef = if not (null auxCoef) then auxCoef else "1"
                  rest = dropWhile isPartOfNumber s


monToStr (coef,rest) = (if coef >0 then "+" else "") ++show coef ++ printVars rest

printVars [] = ""
printVars ((var,exponent):xs)   | exponent == 1 =  var: printVars xs
                                | otherwise = [var] ++ "^" ++ show exponent ++ printVars xs

polToStr [] = ""
polToStr [x] = monToStr x
polToStr (x:xs) = monToStr x ++  polToStr xs
                  


    