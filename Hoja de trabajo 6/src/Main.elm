module Main exposing (..)

type Natural = Suc Natural | Cero

enteroANatural : Int -> Natural
enteroANatural i = if i == 0
    then Cero
    else Suc (enteroANatural (i - 1))

naturalAEntero : Natural -> Int
naturalAEntero n = case n of
    Cero -> 0
    Suc i -> 1 + naturalAEntero i


sumar : Natural -> Natural -> Natural
sumar n1 n2 = case (n1,n2) of
    (Cero, n2_) -> n2_
    (n1_, Cero) -> n1_
    (Suc n1_, n2_) -> Suc (sumar n1_ n2_)

resta : Natural -> Natural -> Natural
resta n1 n2 = case (n1,n2) of
    (Cero, n2_) -> Cero
    (n1_, Cero) -> n1_
    (Suc n1_, Suc n2_) -> (resta n1_ n2_)

mult : Natural -> Natural -> Natural
mult n1 n2 = case (n1, n2) of
    (Cero, n2_) -> Cero
    (n1_, Cero) -> Cero
    (Suc n1_, n2_) -> sumar n2_ (mult n1_ n2_)
    
div : Natural -> Natural -> (Natural,Natural)
div n1 n2 = contador (n1) (n2) (Cero)
contador n1 n2 n3 =
    if n1 == (Suc(Cero)) && n2 == (Suc(Cero))
    then ((Suc(n3),Cero))
    else if resta n1 n2 == Cero
    then (n3, n2)
    else contador (resta n1 n2) (n2) (Suc (n3))

--type Expresion = 
--    sumar Expresion Expresion Expresion
  --  mult Expresion Expresion Expresion
    --valor Int 

--type Expresion = Valor 
--    Sumar Expresion Expresion
--    Mult Expresion Expresion

--type GExpresion a = Valor a
--    Suma (GExpresion a) (GExpresion a)
--    Mult (GExpresion a) (GExpresion a)
suma: Int -> Int -> Int
suma a b = a + b

multi: Int -> Int -> Int
multi a b = a * b


type Expresion = 
    Suma Expresion Expresion
    Multi Expresion Expresion
    Valor Int

reducir:((Int -> Int -> Int), (Int -> Int -> Int)) -> Expresion -> Int 
reducir (suma, multi) expre = case expre of
    Suma expre1 expre2 -> suma (reducir (suma, multi) expre1)  (reducir (suma, multi) expre2)
    Multiplicacion expre1 expre2 -> multi (reducir (suma, multi) expre1)  (reducir (suma, multi) expre2)
    Valor i -> i