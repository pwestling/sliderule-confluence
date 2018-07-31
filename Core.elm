import Html exposing (beginnerProgram, Html, text, div, span, form)
import Material.Layout as Layout
import Material.Snackbar as Snackbar
import Material.Icon as Icon
import Material.Color as Color
import Material.Menu as Menu
import Material.Dialog as Dialog
import Material.Button as Button
import Material.Options as Options exposing (css, cs, when)
import Material.Scheme
import Material
import Material.Snackbar as Snackbar



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
    inProgressTermRecieved : Maybe TradeTerm,

    -- UI stuff

    mdl : Material.Model
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
    inProgressTermRecieved = Nothing,
    
    -- UI stuff

    mdl = Material.model}

type TradeDirection = Given | Recieved

-- UPDATE

type Msg = NextTurn 
    | Mdl (Material.Msg Msg)
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

-- VIEW

view : Model -> Html Msg
view model =
    Material.Scheme.top <|
        Layout.render Mdl
                model.mdl
                [ Layout.fixedHeader
                , Layout.fixedDrawer
                , Options.css "display" "flex !important"
                , Options.css "flex-direction" "row"
                , Options.css "align-items" "center"
                ]
                { header = [ viewHeader model ]
                , drawer = [ drawerHeader model, viewDrawer model ]
                , tabs = ( [], [] )
                , main =
                    [ viewBody model
                    , viewSource model
                    ] 
                }

viewHeader model = text "Header"
drawerHeader model = text "Drawer Header"
viewDrawer model = text "View Drawer"
viewSource model = text "View Source"
viewBody model = text "View Body"