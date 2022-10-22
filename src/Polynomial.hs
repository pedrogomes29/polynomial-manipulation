module Polynomial where

import Data.Char
import Data.List

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


getBiggestExp:: [(Var,Exponent)] -> ([Var],Exponent) --gets biggest exponent of the mon and variables that have that exp
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
             then (head var, if sign=='-'then -read absExponent else read exponent): readVars otherVars 
             else (head s,1) :readVars (tail s) -- if the next character isn't a carat (^), then the head is a variable with exponent 1 ommited
                where (var,carat:rest) = break('^'==) s  -- carat is the symbol '^'
                      exponent = takeWhile isPartOfNumber rest
                      (sign:absExponent) = exponent
                      otherVars = dropWhile isPartOfNumber rest

parsePol =  normPol . map (orderMon . parseMon) . tail . splitInput . addSeperator . cleanInput

parseMon s = (if sign=='-' then -read absCoef else read absCoef,readVars rest)
            where (sign:auxCoef) = takeWhile isPartOfNumber s
                  absCoef = if not (null auxCoef) then auxCoef else "1"
                  rest = dropWhile isPartOfNumber s


monToStr (coef,rest)    | coef>=0 && rest==[] = "+ " ++ show coef
                        | rest==[] = "- " ++ show (abs coef) 
                        | coef==(-1) = "- " ++ printVars rest
                        | coef==1 = "+ " ++ printVars rest
                        | coef<0 = "- " ++ show (abs coef) ++ printVars rest
                        | otherwise =  "+ " ++ show coef ++ printVars rest

printVars [] = ""
printVars ((var,exponent):xs)   | exponent == 1 =  var: printVars xs
                                | otherwise = [var] ++ "^" ++ show exponent ++ printVars xs

printResult = cleanOutput . polToStr . orderPol

cleanOutput ('+':' ':mm) = mm
cleanOutput ('-':' ':mm) = '-':mm
cleanOutput mm = mm

polToStr [] = ""
polToStr [x] = monToStr x
polToStr (x:xs) = monToStr x ++ " " ++  polToStr xs



removeExp [] exp = []
removeExp ((var,exp1):xs) exp2| exp1==exp2 = removeExp xs exp2
                              | otherwise = (var,exp1):(removeExp xs exp2)

getBiggestExp ([]) = ([],minBound::Int)
getBiggestExp ((var,exp):xs)  | maxExpRest<exp = ([var],exp)
                              | maxExpRest>exp = (varsRest,maxExpRest)
                              | otherwise = (insert var varsRest,exp)
                              where (varsRest,maxExpRest) = getBiggestExp xs

compareVars [] [] = EQ
compareVars x [] = LT
compareVars [] y = GT
compareVars (x:xs) (y:ys)     | x==y = compareVars xs ys
                              | otherwise = compare x y

compareMon (0,[]) (0,[]) = EQ
compareMon (0,xs) (0,[]) = LT
compareMon (0,[]) (0,ys) = GT
compareMon (coef1,vars1) (coef2,vars2)    | maxExp1 /= maxExp2 = compare maxExp2 maxExp1
                                          | compareVarsResult/=EQ = compareVarsResult
                                          | otherwise = compareMon (0,removeExp vars1 maxExp1) (0,removeExp vars2 maxExp2)
                                          where (maxVars1,maxExp1) = getBiggestExp vars1
                                                (maxVars2,maxExp2) = getBiggestExp vars2
                                                compareVarsResult = compareVars maxVars1 maxVars2

orderMon (coef,vars) = (coef,sortBy (\(var1,exp1) (var2,exp2) -> compare var1 var2) vars)
orderPol = sortBy compareMon



countEqualMon (coef1,vars1) [] = 0
countEqualMon (coef1,vars1) ((coef2,vars2):xs) | vars1==vars2 = coef2 + countEqualMon (coef1,vars1) xs
                                               | otherwise = countEqualMon (coef1,vars1) xs

removeEqualMon mon [] = []
removeEqualMon (coef1,vars1) ((coef2,vars2):xs) | vars1==vars2 = removeEqualMon (coef1,vars1) xs
                                                | otherwise = (coef2,vars2):removeEqualMon (coef1,vars1) xs


normPol = orderPol . removeCoefZero . joinMon . removeExpZero

joinMon [] = []
joinMon ((coef,vars):xs) =(coef+countEqualMon (coef,vars) xs,vars):joinMon (removeEqualMon (coef,vars) xs) 

sumPol p1 p2 = normPol (p1++p2)

subPol p1 p2 = normPol (p1++map (\(coef,vars) -> (-coef,vars)) p2)

mulPol [] [] = []
mulPol p1 [] = []
mulPol [] p2 = []
mulPol p1 p2 = normPol [orderMon (mulMon m1 m2) | m1<-p1 ,m2<-p2]

sumExponentEqualVars var1 xs = sum [exponent2|(var2,exponent2)<-xs,var1==var2]

removeEqualVars var1 xs = [(var2,exponent2)|(var2,exponent2)<-xs,var1/=var2]

normVars [] = []
normVars ((var,exp):restVars) = (var,exp+(sumExponentEqualVars var restVars)):normVars(removeEqualVars var restVars)


mulMon (coef1,vars1) (coef2,vars2) = (coef1*coef2,normVars (vars1++vars2))

derivePolAux [] _ = []
derivePolAux ((coef,vars):xs) var = if exp==0 then derivePolAux xs var
                                    else orderMon((coef*exp,varsAfterDer)):derivePolAux xs var 
                                    where exp = (sumExponentEqualVars var vars)  -- actually just gets the exponent of the var, pol is normalized
                                          varsAfterDer = (var,exp-1):(filter (\(v,e) -> v/=var) vars)


derivePol xs var = removeExpZero (derivePolAux xs var)


removeExpZero p = [(coef, [(var,exp)|(var,exp)<-vars,exp/=0]) | (coef,vars) <- p]
removeCoefZero p = [(coef,vars)|(coef,vars)<-p,coef/=0]

