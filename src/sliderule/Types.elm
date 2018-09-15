module Sliderule.Types exposing (..)

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

        other ->
            toString other


type alias Turn =
    Int


type ResourceSize
    = SmallCube
    | LargeCube
    | Octa


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


sizeOf : ResourceKind -> Maybe ResourceSize
sizeOf kind =
    case kind of
        Green ->
            Just SmallCube

        Brown ->
            Just SmallCube

        White ->
            Just SmallCube

        Black ->
            Just LargeCube

        Yellow ->
            Just LargeCube

        Blue ->
            Just LargeCube

        Ultra ->
            Just Octa

        VP ->
            Just Octa

        _ ->
            Nothing


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


type alias InProgressResource =
    { kindSelector : Input.SelectWith ResourceSpec Msg
    , amount : String
    }


type InProgressItem
    = IPR InProgressResource
    | IPC CustomTrade


type alias InProgressTradeTerm =
    { paidAt : String
    , direction : TradeDirection
    , item : InProgressItem
    , validationMsg : Maybe String
    }


type alias Trade =
    { faction : Faction
    , termsGiven : List TradeTerm
    , termsRecieved : List TradeTerm
    , turnExecuted : Turn
    }


type alias InProgressTrade =
    { tradeInProgress : Trade
    , inProgressTermGiven : InProgressTradeTerm
    , inProgressTermRecieved : InProgressTradeTerm
    }


getTerm : TradeDirection -> InProgressTrade -> InProgressTradeTerm
getTerm dir ipt =
    case dir of
        Given ->
            ipt.inProgressTermGiven

        Recieved ->
            ipt.inProgressTermRecieved


transformTerm : TradeDirection -> (InProgressTradeTerm -> InProgressTradeTerm) -> InProgressTrade -> InProgressTrade
transformTerm dir fn ipt =
    case dir of
        Given ->
            { ipt | inProgressTermGiven = fn ipt.inProgressTermGiven }

        Recieved ->
            { ipt | inProgressTermRecieved = fn ipt.inProgressTermRecieved }


liftToState : (InProgressTrade -> InProgressTrade) -> NextTradeState -> NextTradeState
liftToState fn nts =
    case nts of
        None ->
            nts

        ChooseFaction ->
            nts

        InProgress ipt ->
            InProgress (fn ipt)


defaultTerm : Turn -> TradeDirection -> InProgressTradeTerm
defaultTerm turn dir =
    { item = IPR { amount = "0", kindSelector = Input.dropMenu Nothing (SetResourceKind dir) }
    , paidAt = "+0"
    , direction = dir
    , validationMsg = Nothing
    }


toggleTerm : TradeDirection -> NextTradeState -> NextTradeState
toggleTerm dir =
    liftToState
        (transformTerm dir
            (\ign ->
                case ign.item of
                    IPC c ->
                        { ign | item = IPR { amount = "0", kindSelector = Input.dropMenu Nothing (SetResourceKind dir) } }

                    IPR r ->
                        { ign | item = IPC "Custom", paidAt = ign.paidAt, direction = dir }
            )
        )


updateCustom : TradeDirection -> CustomTrade -> NextTradeState -> NextTradeState
updateCustom dir ct =
    liftToState
        (transformTerm dir
            (\ign ->
                case ign.item of
                    IPC c ->
                        { ign | item = IPC ct }

                    IPR r ->
                        ign
            )
        )


updateIPRMenu : Input.SelectMsg ResourceSpec -> InProgressResource -> InProgressResource
updateIPRMenu msg resource =
    { resource | kindSelector = Input.updateSelection msg resource.kindSelector }


setTermValidation : TradeDirection -> Maybe String -> NextTradeState -> NextTradeState
setTermValidation dir msg =
    liftToState (transformTerm dir (\t -> { t | validationMsg = msg }))


updateInProgressResource :
    TradeDirection
    -> (InProgressResource -> InProgressResource)
    -> NextTradeState
    -> NextTradeState
updateInProgressResource dir fn =
    liftToState
        (transformTerm dir
            (\term ->
                case term.item of
                    IPR resource ->
                        { term | item = IPR (fn resource) }

                    IPC custom ->
                        term
            )
        )


updateKindMenu : TradeDirection -> Input.SelectMsg ResourceSpec -> NextTradeState -> NextTradeState
updateKindMenu dir msg =
    updateInProgressResource dir (updateIPRMenu msg)


validateAmount : String -> Result String Int
validateAmount amount =
    case String.toInt amount of
        Ok i ->
            validate (notNegative "Amount") << validate (notZero "Amount") <| Ok i

        Err s ->
            Err "Amount must be an integer"


updateAmount : TradeDirection -> String -> NextTradeState -> NextTradeState
updateAmount dir amount =
    updateInProgressResource dir (\r -> { r | amount = amount }) >> setTermValidation dir Nothing


updateTurn : TradeDirection -> String -> NextTradeState -> NextTradeState
updateTurn dir paidAt =
    liftToState
        (transformTerm dir (\term -> { term | paidAt = paidAt }))
        >> setTermValidation dir Nothing


type NextTradeState
    = None
    | ChooseFaction
    | InProgress InProgressTrade


notGreater : String -> Int -> Int -> Maybe String
notGreater name k i =
    if i > k then
        Just (name ++ " must not be greater than " ++ toString k)

    else
        Nothing


notNegative : String -> Int -> Maybe String
notNegative name i =
    if i < 0 then
        Just (name ++ " must not be negative")

    else
        Nothing


notZero : String -> Int -> Maybe String
notZero name i =
    if i == 0 then
        Just (name ++ " must not be zero")

    else
        Nothing


parseTurn : Model -> String -> Result String Int
parseTurn model turnString =
    (case String.startsWith "+" turnString of
        True ->
            Result.map (\t -> t + model.currentTurn) (String.toInt turnString)

        False ->
            String.toInt turnString
    )
        |> validate (notNegative "Turn")
        |> validate (notGreater "Turn" 6)


finishTerm : Model -> InProgressTradeTerm -> Result InProgressTradeTerm TradeTerm
finishTerm model iptt =
    Result.mapError (\msg -> { iptt | validationMsg = Just msg })
        (case iptt.item of
            IPR resource ->
                let
                    spec =
                        maybeToResult (Input.selected resource.kindSelector) "No resource kind selected"

                    amount =
                        validateAmount resource.amount

                    paidAt =
                        parseTurn model iptt.paidAt
                in
                Result.map3 (\s a p -> { item = TIResource (mkResource a s.kind s.size), paidAt = p }) spec amount paidAt

            IPC custom ->
                let
                    paidAt =
                        parseTurn model iptt.paidAt
                in
                Result.map (\p -> { item = Custom custom, paidAt = p }) paidAt
        )


appendGiven term trade =
    { trade | termsGiven = trade.termsGiven ++ term }


appendRecieved term trade =
    { trade | termsRecieved = trade.termsRecieved ++ term }


finishTrade : Model -> InProgressTrade -> Trade
finishTrade model ipt =
    ipt.tradeInProgress


finishTermForDir : Model -> TradeDirection -> InProgressTrade -> InProgressTrade
finishTermForDir model dir ipt =
    let
        trade =
            ipt.tradeInProgress
    in
    case dir of
        Given ->
            case finishTerm model ipt.inProgressTermGiven of
                Ok term ->
                    { ipt | tradeInProgress = appendGiven [ term ] trade }

                Err ipterm ->
                    { ipt | inProgressTermGiven = ipterm }

        Recieved ->
            case finishTerm model ipt.inProgressTermRecieved of
                Ok term ->
                    { ipt | tradeInProgress = appendRecieved [ term ] trade }

                Err ipterm ->
                    { ipt | inProgressTermRecieved = ipterm }


toTrade : Model -> NextTradeState -> List Trade
toTrade model state =
    case state of
        None ->
            []

        ChooseFaction ->
            []

        InProgress inProgress ->
            [ finishTrade model inProgress ]


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
    | CompleteTrade
    | StartTrade
    | SelectTradeFaction Faction
    | ToggleTermMode TradeDirection
    | SetCustomTerm TradeDirection CustomTrade
    | SetResourceKind TradeDirection (Input.SelectMsg ResourceSpec)
    | SetResourceAmount TradeDirection String
    | SetTermTurn TradeDirection String
    | CompleteTerm TradeDirection


type alias Model =
    { trades : List Trade
    , resources : List Resource
    , currentTurn : Int
    , nextTrade : NextTradeState
    }
