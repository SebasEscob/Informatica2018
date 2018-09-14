module Main exposing (PrimeResult, isPrime, getPrimeNumbers)

type PrimeResult =
  Yes |
  No |
  Maybe

getPrimeNumbers : Int -> Int -> List Int
getPrimeNumbers start end =
  List.reverse (createList start end [])

createList : Int -> Int -> List Int -> List Int
createList start end primeNumbers =
  if start > end then
    primeNumbers
  else if (isPrime start) == Yes then
    createList (incr start) end (start :: primeNumbers)
  else
    createList (incr start) end primeNumbers


--isPrime : Int -> PrimeResult
--isPrime num =
--  if num < 1 then
--    No
--  else if num == 1 then
--    No
--  else if num == 2 then
--    Yes
--  else if (isDivisible num 2) then
--    No
--  else
--  check 3 num

isPrime : Int -> PrimeResult
isPrime num = primo 2 num
primo contar num =
    if num == 2 then Yes
    else if modBy contar num == 0 
    then No
    else if contar == num - 1 
    then Yes
    else primo (contar + 1) num


check : Int -> Int -> PrimeResult
check divisor quotient =
  if divisor == quotient then
    Yes
  else if (isDivisible quotient divisor) then
    No
  else
    check (incrBy2 divisor) quotient


isDivisible : Int -> Int -> Bool
isDivisible quotient divisor =
  if quotient // divisor == 0 then
    True
  else
    False

incr : Int -> Int
incr num =
  num + 1

incrBy2 : Int -> Int
incrBy2 num =
  num + 2