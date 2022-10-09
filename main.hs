import Data.Char

type Coef = Int
type Exponent = Int
type Var = Char
type Monomial = (Coef,[(Var,Exponent)])
type Polynomial = [Monomial]


cleanInput:: String -> String
splitInput:: String -> [String]

readVars:: String->[(Var,Exponent)]

parseMon:: String -> Monomial
parsePol:: String -> Polynomial

polToStr:: Polynomial -> String
monToStr:: Monomial -> String
printVars:: [(Var,Exponent)]->String

cleanInput s =  filter (\x -> x/='*' && x/=' ') s

splitInput "" = []
splitInput s = mon1: splitInput (if mon2=="" then "" else (tail mon2))
               where (mon1,mon2) = break ('+'==) s


parseMon s = (read coef,readVars rest)
            where coef = takeWhile isDigit s
                  rest = dropWhile isDigit s

readVars "" = []
readVars (x:[]) = [(x,1)] --if last variable has exponent 1 (ommited)
readVars s = if head (tail s)=='^' 
             then (head var, read exponent::Int): readVars otherVars 
             else (head s,1) :readVars (tail s) -- if the next character isn't a carat (^), then the head is a variable with exponent 1 ommited
                where (var,(carat:rest)) = break('^'==) s  -- carat is the symbol '^'
                      exponent = takeWhile isDigit rest
                      otherVars = dropWhile isDigit rest

parsePol = map (parseMon) . splitInput . cleanInput

monToStr (coef,rest) = show coef ++ printVars rest

printVars [] = ""
printVars ((var,exponent):xs)   | exponent == 1 =  [var] ++ printVars xs
                                | otherwise = [var] ++ "^" ++ show exponent ++ printVars xs

polToStr [] = ""
polToStr (x:[]) = monToStr x
polToStr (x:xs) = monToStr x ++ " + " ++ polToStr xs
                  


    