module Main exposing (frib)

frib : Int -> Int
frib n =
    if n == 0
    then 0
    else if n == 1
    then 1
    else frib (n-1) + frib (n-2) 