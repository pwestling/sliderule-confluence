import Html exposing (beginnerProgram, Html)
import Element exposing (..)
import Element.Events exposing (..)
import Element.Attributes exposing (..)
import Style exposing(..)

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = emptyModel, view = view, update = update }

-- MODEL

type Faction = Ktz | Kjas | Caylion | Faderan | Unity | EniEt | Imdril | Zeth | Yengii

getFactionName : Faction -> String
getFactionName f = case f of
    Ktz -> "Kt'zr'kt'rtl"
    Kjas -> "Kjasjavikalimm"
    EniEt -> "Eni Et"
    Imdril -> "Im'dril"
    other -> toString other

type alias Turn = Int

type ResourceSize = SmallCube | LargeCube | Octa
type ResourceKind = White | Green | Brown | Black | Yellow | Blue | Ultra | VP | Wild | Any
type ResourceMode = Donation | Normal

type alias Resource = {
    amount : Int,
    size : ResourceSize,
    rType : ResourceKind,
    mode : ResourceMode
}

sizeOf : ResourceKind -> Maybe ResourceSize
sizeOf kind = case kind of
 Green -> Just SmallCube
 Brown -> Just SmallCube
 White -> Just SmallCube
 Black -> Just LargeCube
 Yellow -> Just LargeCube
 Blue -> Just LargeCube
 Ultra -> Just Octa
 VP -> Just Octa
 _ -> Nothing

mkResource : Int -> ResourceKind -> ResourceSize -> Resource
mkResource amount rType size = {amount = amount, rType = rType, size = size, mode = Normal}
    
type alias CustomTrade = String
type TradeItem = TIResource Resource | Custom CustomTrade
type alias TradeTerm = {
    item : TradeItem,
    paidAt : Turn
}

type alias Trade =  {
    faction : Faction,
    termsGiven : List TradeTerm,
    termsRecieved : List TradeTerm
}

type alias Model =  {
    trades : List Trade,
    resources : List Resource,
    currentTurn : Int,
    tradeInProgress : Maybe Trade,
    inProgressTermGiven : Maybe TradeTerm,
    inProgressTermRecieved : Maybe TradeTerm
}

emptyModel : Model
emptyModel = {
    trades = [ { faction = Ktz,
                 termsGiven = [{ item = TIResource (mkResource 3 Green SmallCube), paidAt = 1},
                               { item = TIResource (mkResource 1 Black LargeCube), paidAt = 1}],
                 termsRecieved = [{item = TIResource (mkResource 2 White SmallCube), paidAt = 1},
                                   {item = Custom "First born child and all of his or her friends", paidAt = 6}]}], 
    resources = [mkResource 2 Black LargeCube], 
    currentTurn = 0,
    tradeInProgress = Nothing, 
    inProgressTermGiven = Nothing,
    inProgressTermRecieved = Nothing}

type TradeDirection = Given | Recieved

-- UPDATE

type Msg = NextTurn 
    | DeleteTrade Trade
    | CompleteTrade
    | StartTrade
    | ToggleTermMode TradeDirection
    | SetCustomTerm TradeDirection CustomTrade
    | SetResourceKind TradeDirection ResourceKind
    | SetResourceAmount TradeDirection Int
    | SetTermTurn TradeDirection Turn


update : Msg -> Model -> Model
update msg model = 
  case msg of
    DeleteTrade t -> { model | trades = List.filter (\tr -> t /= t) (model.trades)}
    other -> model
    
type Style = 
      AppStyle
    | TradesStyle
    | TurnCounterStyle
    | TradeStyle Faction
    | FactionNameStyle Faction
    | TradeTermStyle
    | TermsStyle
    | DeleteTradeButtonStyle
    | InfoPaneStyle
    | TradePaneStyle
    | TermText

type Variation = 
      DefaultVar

stylesheet =
    Style.styleSheet
        [ 
        ]


-- VIEW

view : Model -> Html Msg
view model = Element.layout stylesheet <|
   row AppStyle  [] [
   column TradePaneStyle [width fill] [
    currentTurnDiv (model.currentTurn),
    column TradesStyle [] (tradeView model.trades)
  ],
  el InfoPaneStyle [width fill] (text "Info")]
   
currentTurnDiv : Turn -> Element Style Variation Msg
currentTurnDiv turn = el TurnCounterStyle [] (text ("Turn: " ++ (toString turn)))

tradeView : List Trade -> List (Element Style Variation Msg)
tradeView trades = List.map tradeHtml trades

tradeHtml : Trade -> Element Style Variation Msg
tradeHtml trade = 
    let
        tradeFactionString = getFactionName trade.faction
    in
       row (TradeStyle trade.faction) [] [
               el (FactionNameStyle trade.faction) [padding 10, width (percent 20)] (text tradeFactionString),
               el TermText [paddingTop 10, alignLeft, width (percent 10)] (text "You Paid"),
               el TermsStyle [padding 10, width (percent 25)] (termsDiv "terms-given" (trade.termsGiven)),
               el TermText [paddingTop 10, alignLeft, width (percent 10)] (text "You Got"),
               el TermsStyle [padding 10, width (percent 25)] (termsDiv "terms-recieved" (trade.termsRecieved)),
               button DeleteTradeButtonStyle [onClick (DeleteTrade trade), padding 10, width (percent 10)] (text "Delete")
           ] 

termsDiv : String -> List TradeTerm -> Element Style Variation Msg
termsDiv class terms = wrappedColumn TermsStyle [] (List.map termDiv terms)

termDiv : TradeTerm -> Element Style Variation Msg
termDiv term = 
    let termText = case (term.item) of
        TIResource resource -> toString resource.amount ++ " " ++ 
                               toString resource.rType ++ " on turn " ++ 
                               toString (term.paidAt) 
        Custom customTrade -> customTrade
    in el TradeTermStyle [spacing 5] (text termText)
    