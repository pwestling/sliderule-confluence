module Sliderule.TradeParser exposing (charIs, createTrade, doubleQuote, factionParser, mkR, parseTrade, prefixMatch, quotes, resourceKindParser, resourceParser, restOfAlpha, stringP, stringPHelp, stringWithout, tradeGroupParser, tradeItemParser, tradeParser, tradeTermParser)

import Debug as Debug
import Parser exposing (..)
import Parser.Extras exposing (..)
import Sliderule.Types exposing (..)
import Sliderule.Util exposing (..)


parseTrade : Turn -> String -> Maybe Trade
parseTrade currentTurn s =
    resultToMaybe <| run (tradeParser currentTurn) s


tradeParser : Turn -> Parser Trade
tradeParser currentTurn =
    succeed (createTrade currentTurn)
        |= factionParser
        |. spaces
        |= many (tradeGroupParser currentTurn)


createTrade : Turn -> Faction -> List ( TradeDirection, List TradeTerm ) -> Trade
createTrade currentTurn faction termGroups =
    let
        getTerms dir =
            List.concat <| List.map Tuple.second <| List.filter (\t -> Tuple.first t == dir) termGroups

        termsGiven =
            getTerms Given

        termsRecieved =
            getTerms Recieved
    in
    { faction = faction
    , termsGiven = termsGiven
    , termsRecieved = termsRecieved
    , turnExecuted = currentTurn
    }


factionParser : Parser Faction
factionParser =
    oneOf
        [ prefixMatch Ktz "Kt"
        , prefixMatch Kjas "Kj"
        , prefixMatch Caylion "C"
        , prefixMatch Faderan "F"
        , prefixMatch Imdril "I"
        , prefixMatch Unity "U"
        , prefixMatch Yengii "Y"
        , prefixMatch Zeth "Z"
        , prefixMatch EniEt "E"
        ]


tradeGroupParser : Turn -> Parser ( TradeDirection, List TradeTerm )
tradeGroupParser currentTurn =
    succeed Tuple.pair
        |= oneOf [ succeed Given |. symbol "-", succeed Recieved |. symbol "+" ]
        |. spaces
        |= many (tradeTermParser currentTurn)


tradeTermParser : Turn -> Parser TradeTerm
tradeTermParser currentTurn =
    succeed TradeTerm
        |= tradeItemParser
        |= oneOf
            [ succeed (\t -> currentTurn + t)
                |. symbol "+"
                |= int
            , succeed currentTurn
            ]
        |. spaces


tradeItemParser : Parser TradeItem
tradeItemParser =
    oneOf
        [ succeed Custom |= quotes (stringWithout doubleQuote)
        , succeed TIResource |= resourceParser
        ]


resourceParser : Parser Resource
resourceParser =
    succeed mkR
        |= int
        |= resourceKindParser


resourceKindParser : Parser ResourceKind
resourceKindParser =
    oneOf
        [ succeed Green |. stringP "g"
        , succeed White |. stringP "w"
        , succeed Yellow |. stringP "y"
        , succeed Ultra |. stringP "u"
        , succeed Ship |. stringP "s"
        , succeed identity
            |. stringP "b"
            |= oneOf
                [ succeed Blue |. stringP "u"
                , succeed Brown |. stringP "r"
                , succeed Black |. stringP "k"
                ]
        ]


quotes : Parser a -> Parser a
quotes p =
    between (symbol "\"") (symbol "\"") p


prefixMatch : a -> String -> Parser a
prefixMatch a prefix =
    succeed a |. stringP prefix |. restOfAlpha


stringP : String -> Parser String
stringP s =
    stringPHelp s (succeed ())


stringPHelp : String -> Parser () -> Parser String
stringPHelp s prev =
    case String.uncons s of
        Just ( c, cs ) ->
            stringPHelp cs (prev |. chompIf (charIs c))

        Nothing ->
            getChompedString prev


stringWithout : Char -> Parser String
stringWithout c =
    getChompedString <| chompWhile (not << charIs c)


charIs : Char -> Char -> Bool
charIs c o =
    c == o


restOfAlpha : Parser ()
restOfAlpha =
    succeed () |. chompWhile Char.isAlpha


mkR : Int -> ResourceKind -> Resource
mkR amount kind =
    { rType = kind, amount = amount, size = sizeOf kind, mode = Normal }


doubleQuote : Char
doubleQuote =
    '"'
