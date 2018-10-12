module Main exposing (..)
--Primera parte
zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith l lis1 lis2  = 
    case (lis1, lis2) of
    ([], _) -> []
    (_, []) -> []
    (n::ns, b::bs) -> l n b ::zipWith l ns bs

--Segunda parte
groupBy : (a -> Bool) -> List a -> (List a, List a) 
groupBy y list = (work y list, dontwork y list)

work s list = case list of 
    [] -> []
    (n::ns) -> if s n then work s ns else n:: work s ns

dontwork s list = case list of 
    [] -> []
    (b::bs) -> if s b then b:: dontwork s bs else dontwork s bs

--Tercera parte
bind : Maybe a -> (a -> Maybe b) -> Maybe b
bind ns b = case ns of 
    Nothing -> Nothing 
    Just a -> b a 