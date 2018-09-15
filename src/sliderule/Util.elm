module Sliderule.Util exposing (..)

import Color018 exposing (..)

sToI : String -> Maybe Int
sToI = String.toInt
   

validate : (a -> Maybe b) -> Result b a -> Result b a
validate pred r =
    case r of
        Ok a ->
            case pred a of
                Just err ->
                    Err err

                Nothing ->
                    Ok a

        Err b ->
            Err b


maybeToResult : Maybe a -> b -> Result b a
maybeToResult m b =
    case m of
        Just a ->
            Ok a

        Nothing ->
            Err b


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just a ->
            True

        Nothing ->
            False


isNothing : Maybe a -> Bool
isNothing m =
    not (isJust m)


isOk : Result a b -> Bool
isOk r =
    case r of
        Ok b ->
            True

        Err a ->
            False


maybeToList : Maybe a -> List a
maybeToList m =
    case m of
        Just a ->
            [ a ]

        Nothing ->
            []


join : Maybe (Maybe a) -> Maybe a
join m =
    case m of
        Just a ->
            a

        Nothing ->
            Nothing


resultToList =
    maybeToList << Result.toMaybe


intersperseSections : (a -> b) -> (a -> a) -> List a -> List a
intersperseSections group creator list =
    case list of
        a :: b :: l ->
            if group a /= group b then
                a :: creator a :: intersperseSections group creator (b :: l)

            else
                a :: intersperseSections group creator (b :: l)

        b :: [] ->
            b :: creator b :: []

        [] ->
            []


flatMap : (a -> List b) -> List a -> List b
flatMap fn l =
    List.concat (List.map fn l)


expand : ( a, List b ) -> List ( a, b )
expand pair =
    List.map (\i -> ( Tuple.first pair, i )) (Tuple.second pair)


splitWhile : (a -> Bool) -> List a -> ( List a, List a )
splitWhile pred list =
    case list of
        a :: l ->
            if pred a then
                let
                    recur =
                        splitWhile pred l
                in
                ( a :: Tuple.first recur, Tuple.second recur )

            else
                ( [], a :: l )

        [] ->
            ( [], [] )


partition : (a -> b) -> List a -> List (b, (List a))
partition group list =
    case list of
        a :: l ->
            let
                recur =
                    splitWhile (\i -> group i == group a) (a :: l)
            in
            Tuple.pair (group a) (Tuple.first recur) :: partition group (Tuple.second recur)

        [] ->
            []


colorToCssRgb : Color -> String
colorToCssRgb color = let rgb = toRgb color in 
                      ("rgb(" ++ String.fromInt (rgb.red) ++ ", " ++ 
                                 String.fromInt (rgb.green) ++ ", " ++ 
                                 String.fromInt (rgb.blue) ++ ")")