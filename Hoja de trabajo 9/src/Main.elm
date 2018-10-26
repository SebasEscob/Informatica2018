module Main exposing (..)
--Impotar eventos para el programa
import Browser
import Html exposing (Html)
import Html.Events exposing (onClick)

--Definir prototipo como String
type alias Prototipo = String
prototipoInicial : Prototipo
prototipoInicial = ""
--Definir Nota como string
type alias Nota = String

--Se define la funcion del actualizador de la pagina
actualizador : Nota -> Prototipo -> Prototipo
actualizador nota prototipo = if nota == "c" then prototipoInicial else  prototipo ++ nota

--Definimos las operaciones de nuestra calculadora
sumar b bs vs = 
    factor (String.toInt (String.fromChar b ++ String.reverse (String.fromList bs))) + factor (String.toInt (String.fromList vs))
multi b bs vs = 
    factor (String.toInt (String.fromChar b ++ String.reverse (String.fromList bs))) * factor (String.toInt (String.fromList vs))

factor : Maybe Int -> Int
factor n = case n of
    Just a -> a
    Nothing -> 0

--Se integra la solucion al problema propuesta segun los operadores
soluc jlist lista = case (jlist, lista) of
    (_, []) -> 0
    ([], b::bs) -> if b == '+' 
        then sumar '0' [] bs else if b == '*' then multi '0' [] bs else soluc (b::[]) (bs)
    (b::bs, v::vs) -> if v == '+'
        then sumar b bs vs else if v == '*' then multi b bs vs else soluc (b::v::bs) vs

--Se concatena la respuesta al cuadro del modelo
solucion prototipo = soluc [] (String.toList prototipo)

cambio proto = Html.div [] [Html.text (proto)]

vista : Prototipo -> Html Nota
vista prototipo =  Html.div 
        []
        [
         cambio prototipo,
        Html.div [] [
        Html.text (String.fromInt(solucion  prototipo)),
        Html.div [] [
        Html.button [onClick "1"] [Html.text "1"],
        Html.button [onClick "2"] [Html.text "2"],
        Html.button [onClick "3"] [Html.text "3"],
        Html.div [] [
        Html.button [onClick "4"] [Html.text "4"],
        Html.button [onClick "5"] [Html.text "5"],
        Html.button [onClick "6"] [Html.text "6"],
        Html.div [] [
        Html.button [onClick "7"] [Html.text "7"],
        Html.button [onClick "8"] [Html.text "8"],
        Html.button [onClick "9"] [Html.text "9"],
        Html.div [] [
        Html.button [onClick ""] [Html.text "_"],
        Html.button [onClick "0" ] [Html.text "0"],
        Html.button [onClick "" ] [Html.text "="], 
        Html.div [] [
        Html.button [onClick "+"] [Html.text "+"],
        Html.button [onClick "*"] [Html.text "*"],
        Html.button [onClick "c"] [Html.text "c"] ] ] ] ] ] ] 
        ]


main = Browser.sandbox {
        init = prototipoInicial,
        view = vista,
        update = actualizador
    }