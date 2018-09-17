module Sliderule.Types exposing (CustomTrade, Faction(..), ItemOwed, Model, Msg(..), Resource, ResourceKind(..), ResourceMode(..), ResourceSize(..), ResourceSpec, Trade, TradeDirection(..), TradeItem(..), TradeTerm, Turn, getFactionName, isFutureTrade, mkResource, obligations, sizeOf, youAreOwed, youOwe)

import Element.Input as Input
import Sliderule.Util exposing (..)


type Faction
    = Ktz
    | Kjas
    | Caylion
    | Faderan
    | Unity
    | EniEt
    | Imdril
    | Zeth
    | Yengii


getFactionName : Faction -> String
getFactionName f =
    case f of
        Ktz ->
            "Kt'zr'kt'rtl"

        Kjas ->
            "Kjasjavikalimm"

        EniEt ->
            "Eni Et"

        Imdril ->
            "Im'dril"

        Caylion ->
            "Caylion"

        Faderan ->
            "Faderan"

        Unity ->
            "Unity"

        Zeth ->
            "Zeth"

        Yengii ->
            "Yengii"


type alias Turn =
    Int


type ResourceSize
    = SmallCube
    | LargeCube
    | Octa
    | NoSize


type ResourceKind
    = White
    | Green
    | Brown
    | Black
    | Yellow
    | Blue
    | Ultra
    | VP
    | Wild
    | Any
    | Ship
    | NoKind


type ResourceMode
    = Donation
    | Normal


type alias ResourceSpec =
    { kind : ResourceKind, size : ResourceSize }


type alias Resource =
    { amount : Int
    , size : ResourceSize
    , rType : ResourceKind
    , mode : ResourceMode
    }


sizeOf : ResourceKind -> ResourceSize
sizeOf kind =
    case kind of
        Green ->
            SmallCube

        Brown ->
            SmallCube

        White ->
            SmallCube

        Black ->
            LargeCube

        Yellow ->
            LargeCube

        Blue ->
            LargeCube

        Ultra ->
            Octa

        VP ->
            Octa

        _ ->
            NoSize


mkResource : Int -> ResourceKind -> ResourceSize -> Resource
mkResource amount rType size =
    { amount = amount, rType = rType, size = size, mode = Normal }


type alias CustomTrade =
    String


type TradeItem
    = TIResource Resource
    | Custom CustomTrade


type alias TradeTerm =
    { item : TradeItem
    , paidAt : Turn
    }


type alias Trade =
    { faction : Faction
    , termsGiven : List TradeTerm
    , termsRecieved : List TradeTerm
    , turnExecuted : Turn
    }


type TradeDirection
    = Given
    | Recieved


type alias ItemOwed =
    { item : TradeItem, faction : Faction }


obligations : Model -> (Trade -> List TradeTerm) -> List ItemOwed
obligations model termFn =
    List.map (\t -> { item = (Tuple.second t).item, faction = Tuple.first t }) <|
        List.filter (\t -> (Tuple.second t).paidAt == model.currentTurn) <|
            flatMap expand <|
                List.map (\t -> ( t.faction, List.filter (isFutureTrade t) (termFn t) )) model.trades


isFutureTrade : Trade -> TradeTerm -> Bool
isFutureTrade trade term =
    trade.turnExecuted /= term.paidAt


youOwe : Model -> List ItemOwed
youOwe model =
    obligations model .termsGiven


youAreOwed : Model -> List ItemOwed
youAreOwed model =
    obligations model .termsRecieved


type Msg
    = NoOp
    | ChangeTurn Int
    | DeleteTrade Trade
    | DeleteTradeInProgress
    | ChangeTradeString String
    | CompleteTrade


type alias Model =
    { trades : List Trade
    , resources : List Resource
    , currentTurn : Int
    , nextTradeString : String
    , nextTrade : Maybe Trade
    , nextTradeValid : Bool
    }
