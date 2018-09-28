module Main exposing (..)

type Expresion = Valor Int | Suma Expresion Expresion | Mult Expresion Expresion

suma : Int -> Int -> Int
suma n1 n2 = n1 + n2

mult : Int -> Int -> Int
mult n1 n2 = n1 * n2

expre : (Int -> Int -> Int, Int -> Int -> Int) -> Expresion -> Int
expre (n1, n2) n3 = case n3 of
    Suma x y -> n1 (expre (n1,n2) x) (expre (n1,n2) y)
    Mult x y -> n2 (expre (n1,n2) x) (expre (n1,n2) y)
    Valor x -> x