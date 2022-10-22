module Polynomial where

import Data.Char
import Data.List

type Coef = Int
type Exponent = Int
type Var = Char
type Monomial = (Coef,[(Var,Exponent)])
type Polynomial = [Monomial]

separator::Char

isPartOfNumber::Char->Bool

cleanInput:: String -> String
splitInput:: String -> [String]
addSeparator::String -> String


getBiggestExp:: [(Var,Exponent)] -> ([Var],Exponent)
orderMon:: Monomial -> Monomial
compareVars:: [Var] -> [Var] -> Ordering
compareMon:: Monomial -> Monomial -> Ordering
removeExp:: [(Var,Exponent)]->Exponent->[(Var,Exponent)]
orderPol:: Polynomial -> Polynomial

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
joinMon:: Polynomial -> Polynomial
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


separator = '?'


-- returns true if c is part of the representation of a digit (+,- or a number)
isPartOfNumber c = isDigit c || c=='+' || c=='-'


-- adds a separator ('?') before every '+' or '-', but skips the '-' signs after '^'
-- in essense it adds a separator between each monomial 
addSeparator "" = []
addSeparator (x:xs) |x=='^' = x:(head xs:addSeparator (tail xs))
                    |x=='-' || x=='+' = separator:(x:addSeparator xs)
                    |otherwise = x:addSeparator xs

-- filters characters that aren't part of the representation of a monomial
-- and adds a plus sign in the begging if it is ommited (the signs are used to parse the polynomial)
cleanInput xs     | head cleaned=='-' || head cleaned=='+' = cleaned 
                  | otherwise = '+':cleaned
                        where cleaned = filter (\x -> isDigit x || isLetter x ||  x=='+' || x=='-' || x=='^') xs

-- splits the input string into a list of strings represeting each monomial
splitInput "" = []
splitInput s = mon1: splitInput (if mon2=="" then "" else tail mon2)
               where (mon1,mon2) = break (separator==) s


-- this function recursively reads the variable (and corresponding exponents) part of the input string
-- it expects the second character to be a carat, since variables are represented by only a char
-- if it isn't, it knows that the variable has it's exponent ommited, puts the variable into the internal
-- representation with exp 1 and recusively calls for the rest of the string. If the string is only
-- a character long, it also knows that it corresponds to a variable with it's exponent ommited
-- otherwise, it reads the exponent, stores it into the internal representation and recursively calls for the rest of the string
readVars "" = []
readVars [x] = [(x,1)] --if last variable has exponent 1 (ommited)
readVars s = if head (tail s)=='^' 
             then (head var,if sign=='-' then -read absExponent else if sign=='+' then read absExponent else read exponent): readVars otherVars 
             else (head s,1) :readVars (tail s) -- if the next character isn't a carat (^), then the head is a variable with exponent 1 ommited
                where (var,carat:rest) = break('^'==) s  -- carat is the symbol '^'
                      exponent = takeWhile isPartOfNumber rest
                      (sign:absExponent) = exponent
                      otherVars = dropWhile isPartOfNumber rest

--It cleans unnecessary characters, adds the separator, uses it to split the string into an array of strings representing monomials (which has
--an empty string in the beggining which is removed with tail), it parses each string, orders it's variables in alphabetical order and then
--normalizes the polynomial
parsePol =  normPol . map (orderMon . parseMon) . tail . splitInput . addSeparator . cleanInput

--It uses the first character to know the sign of the coefficient, uses takewhile to get the coefficient's value
--and then uses readVars to represent the vars in the internal representation
parseMon s = (if sign=='-' then -read absCoef else read absCoef,readVars rest)
            where (sign:auxCoef) = takeWhile isPartOfNumber s
                  absCoef = if not (null auxCoef) then auxCoef else "1"
                  rest = dropWhile isPartOfNumber s

-- transforms the internal representation of a monomial  into a string
-- prints the coefficient of the monomial and calls printVars
monToStr (coef,rest)    | coef>=0 && rest==[] = "+ " ++ show coef
                        | rest==[] = "- " ++ show (abs coef) 
                        | coef==(-1) = "- " ++ printVars rest
                        | coef==1 = "+ " ++ printVars rest
                        | coef<0 = "- " ++ show (abs coef) ++ printVars rest
                        | otherwise =  "+ " ++ show coef ++ printVars rest

-- prints the variable part of the internal representation
printVars [] = ""
printVars ((var,exponent):xs)   | exponent == 1 =  var: printVars xs
                                | otherwise = [var] ++ "^" ++ show exponent ++ printVars xs


--calls monToStr to print monomials and adds separators between them
polToStr [] = ""
polToStr [x] = monToStr x
polToStr (x:xs) = monToStr x ++ " " ++  polToStr xs

--orders polynomial, turns the internal representation into a string and then removes the '+' sign in the begging of the string if needed
printResult = cleanOutput . polToStr . orderPol


--removes the '+' sign in the begging of the string if needed
cleanOutput [] = "0"
cleanOutput ('+':' ':mm) = mm
cleanOutput ('-':' ':mm) = '-':mm
cleanOutput mm = mm



--removes variables with a certain exponent
removeExp [] exp = []
removeExp ((var,exp1):xs) exp2| exp1==exp2 = removeExp xs exp2
                              | otherwise = (var,exp1):(removeExp xs exp2)


--returns a tuple with the variables that have the biggest exponent of a polynomial and the corresponding biggest exponent
getBiggestExp ([]) = ([],minBound::Int)
getBiggestExp ((var,exp):xs)  | maxExpRest<exp = ([var],exp)
                              | maxExpRest>exp = (varsRest,maxExpRest)
                              | otherwise = (insert var varsRest,exp)
                              where (varsRest,maxExpRest) = getBiggestExp xs

--given a list of variables (without exponents), it compares the two lists (only used for compareMon to be deterministic)
compareVars [] [] = EQ
compareVars x [] = LT
compareVars [] y = GT
compareVars (x:xs) (y:ys)     | x==y = compareVars xs ys
                              | otherwise = compare x y

--compares two monomials:
--if one monomial has a variable with bigger exponent it is smaller
--if not, it uses compareVars to see which list of variables with that exponent is smaller
--if the list of variables with that exponent is the same, it removes those variables
--from each monomial and recursively compares the resulting monomials
compareMon (x,[]) (y,[]) = compare x y
compareMon (x,xs) (y,[]) = LT
compareMon (x,[]) (y,ys) = GT
compareMon (coef1,vars1) (coef2,vars2)    | maxExp1 /= maxExp2 = compare maxExp2 maxExp1
                                          | compareVarsResult/=EQ = compareVarsResult
                                          | otherwise = compareMon (coef1,removeExp vars1 maxExp1) (coef2,removeExp vars2 maxExp2)
                                          where (maxVars1,maxExp1) = getBiggestExp vars1
                                                (maxVars2,maxExp2) = getBiggestExp vars2
                                                compareVarsResult = compareVars maxVars1 maxVars2

--orders a Monomial's variables by ascending order (to remove distiction between xy and yx in operations)
orderMon (coef,vars) = (coef,sortBy (\(var1,exp1) (var2,exp2) -> compare var1 var2) vars)
orderPol = sortBy compareMon


--sums coefficients of all instances of a given monomial in a polynomial
countEqualMon (coef1,vars1) [] = 0
countEqualMon (coef1,vars1) ((coef2,vars2):xs) | vars1==vars2 = coef2 + countEqualMon (coef1,vars1) xs
                                               | otherwise = countEqualMon (coef1,vars1) xs

--removes all instances of a given monomial in a polynomial
removeEqualMon mon [] = []
removeEqualMon (coef1,vars1) ((coef2,vars2):xs) | vars1==vars2 = removeEqualMon (coef1,vars1) xs
                                                | otherwise = (coef2,vars2):removeEqualMon (coef1,vars1) xs


--normalize polynomial (removes exponents 0, removes coefficients 0  and adds monomials that are the same)
normPol = orderPol . removeCoefZero . joinMon . removeExpZero

--adds monomials that are the same 
--eg: joinMon x^2+2x^2 would result in 3x^2
joinMon [] = []
joinMon ((coef,vars):xs) =(coef+countEqualMon (coef,vars) xs,vars):joinMon (removeEqualMon (coef,vars) xs) 

--sums polynomials (appends the second polynomial to the end of the first one and then normalizes them)
sumPol p1 p2 = normPol (p1++p2)

--same as pol but considers the second polynomial's monomials' coefficients as their symmetric
subPol p1 p2 = normPol (p1++map (\(coef,vars) -> (-coef,vars)) p2)


--applies distribution law between monomials and then uses mulMon to multiply them and orderMon to order their variables
--the polynomial is then normalized
mulPol [] [] = []
mulPol p1 [] = []
mulPol [] p2 = []
mulPol p1 p2 = normPol [orderMon (mulMon m1 m2) | m1<-p1 ,m2<-p2]

--sums the exponents of a variable in a monomial
sumExponentEqualVars var1 xs = sum [exponent2|(var2,exponent2)<-xs,var1==var2]

--removes all variables equal to a certain variable
removeEqualVars var1 xs = [(var2,exponent2)|(var2,exponent2)<-xs,var1/=var2]


--gets the sum of the exponents (in the rest of the monomial) of the variable in the begging of the list, sums the result
--to it's exponent removes that variable from the rest of the monomial and then recursively calls for the rest of the variables
normVars [] = []
normVars ((var,exp):restVars) = (var,exp+(sumExponentEqualVars var restVars)):normVars(removeEqualVars var restVars)

--multiplies coefficients of two monomials and joins their variables
mulMon (coef1,vars1) (coef2,vars2) = (coef1*coef2,normVars (vars1++vars2))


--derives polynomial. If sumExponentEqualVars returns 0, either that variable has exponent 0 in the first monomial or
--it is not present in that monomial. Either way, ignore that monomial (derivative is 0). Otherwise, multiply coefficient
--by the exponent and deduct one to the exponent of the variable which the polynomial is being derived by
derivePolAux [] _ = []
derivePolAux ((coef,vars):xs) var = if exp==0 then derivePolAux xs var
                                    else orderMon((coef*exp,varsAfterDer)):derivePolAux xs var 
                                    where exp = (sumExponentEqualVars var vars)  -- actually just gets the exponent of the var, pol is normalized
                                          varsAfterDer = (var,exp-1):(filter (\(v,e) -> v/=var) vars)

--calls derivePolAux and removes zero exponents afterwards
derivePol xs var = removeExpZero (derivePolAux xs var)

--removes variables with exponent zero from a polynomial (variables with exponent zero are the same as 1)
removeExpZero p = [(coef, [(var,exp)|(var,exp)<-vars,exp/=0]) | (coef,vars) <- p]

--removes monomials with coefficient 0 from the polynomial (zero property of multiplication, 0*monomial=0)
removeCoefZero p = [(coef,vars)|(coef,vars)<-p,coef/=0]