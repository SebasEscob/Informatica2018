module Main exposing (..)

primos n = esPrimo 2 n
esPrimo x n = 
    if modBy x n == 0
    then False
    else if x == n-1 
    then True
    else esPrimo (x+1) n 


