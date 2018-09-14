module Main exposing (..)

esPrimo : Int -> Bool
esPrimo n = primo 2 n
primo contar n =
    if n == 2 then True
    else if modBy contar n == 0 
    then False
    else if contar == n - 1 
    then True 
    else primo (contar + 1) n
    
primos: Int -> List Int
primos n =
    if n < 1
    then []
    else if esPrimo n == False
    then primos (n-1)
    else n :: primos (n-1)

nprimos: Int -> List Int
nprimos n = contn (n,2)
contn (n,y) =
    if n == 0
    then []
    else if esPrimo y == False
    then contn (n, y + 1)
    else y:: contn (n - 1, y + 1)
