import Html exposing (program, Html)
import Element exposing (..)
import Element.Events exposing (..)
import Element.Attributes exposing (..)
import Html.Attributes as Attr
import Element.Input as Input
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Shadow as Shadow
import Style.Font as Font
import Color exposing (..)
import Maybe as Maybe
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import Color.Convert exposing (..)
import Polygon2d exposing (Polygon2d)
import Point2d exposing (Point2d)
import Geometry.Svg as Geo
import Sliderule.Util exposing (..)

main : Program Never Model Msg
main =
  Html.program { init = init, view = view, update = update, subscriptions = subs }

subs : Model -> Sub Msg
subs model = Sub.none

init : (Model, Cmd Msg)
init =
  (emptyModel, Cmd.none)

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
type ResourceKind = White | Green | Brown | Black | Yellow | Blue | Ultra | VP | Wild | Any | Ship
type ResourceMode = Donation | Normal
type alias ResourceSpec = {kind : ResourceKind, size : ResourceSize}

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

type alias InProgressResource = {
    kindSelector : Input.SelectWith ResourceSpec Msg,
    amount : String
}

type InProgressItem = IPR InProgressResource | IPC CustomTrade

type alias InProgressTradeTerm = {
    paidAt : String,
    direction : TradeDirection,
    item : InProgressItem,
    validationMsg : Maybe String
}

type alias Trade =  {
    faction : Faction,
    termsGiven : List TradeTerm,
    termsRecieved : List TradeTerm,
    turnExecuted : Turn
}

type alias InProgressTrade =  {
        tradeInProgress : Trade,
        inProgressTermGiven : InProgressTradeTerm,
        inProgressTermRecieved : InProgressTradeTerm}

getTerm : TradeDirection -> InProgressTrade -> InProgressTradeTerm
getTerm dir ipt = case dir of
    Given -> ipt.inProgressTermGiven
    Recieved -> ipt.inProgressTermRecieved

transformTerm : TradeDirection -> (InProgressTradeTerm -> InProgressTradeTerm) -> InProgressTrade -> InProgressTrade
transformTerm dir fn ipt = case dir of
    Given -> {ipt | inProgressTermGiven = fn ipt.inProgressTermGiven}
    Recieved -> {ipt | inProgressTermRecieved = fn ipt.inProgressTermRecieved}
    
liftToState : (InProgressTrade -> InProgressTrade) -> NextTradeState -> NextTradeState
liftToState fn nts = case nts of
    None -> nts
    ChooseFaction -> nts
    InProgress ipt -> InProgress (fn ipt)

defaultTerm : Turn -> TradeDirection -> InProgressTradeTerm
defaultTerm turn dir = {item = IPR {amount = "0", kindSelector = Input.dropMenu Nothing (SetResourceKind dir)}, 
    paidAt = "+0", direction = dir, validationMsg = Nothing}

toggleTerm : TradeDirection -> NextTradeState -> NextTradeState
toggleTerm dir = liftToState (transformTerm dir (\ign ->
    case ign.item of
        IPC c -> {ign | item = IPR {amount = "0", kindSelector = Input.dropMenu Nothing (SetResourceKind dir)}}
        IPR r -> {ign | item = (IPC "Custom"), paidAt = ign.paidAt, direction = dir}))

updateCustom : TradeDirection -> CustomTrade ->  NextTradeState -> NextTradeState
updateCustom dir ct = liftToState (transformTerm dir (\ign ->
    case ign.item of
        IPC c -> {ign | item = IPC ct}
        IPR r -> ign))

updateIPRMenu : Input.SelectMsg ResourceSpec -> InProgressResource -> InProgressResource
updateIPRMenu msg resource = {resource | kindSelector = Input.updateSelection msg resource.kindSelector}

setTermValidation : TradeDirection -> Maybe String -> NextTradeState -> NextTradeState
setTermValidation dir msg = liftToState (transformTerm dir (\t -> {t | validationMsg = msg}))

updateInProgressResource : 
    TradeDirection -> 
    (InProgressResource -> InProgressResource) -> 
    NextTradeState -> NextTradeState
updateInProgressResource dir fn =  liftToState (transformTerm dir 
        (\term -> case term.item of 
            IPR resource -> {term | item = IPR (fn resource)}
            IPC custom -> term)) 

updateKindMenu : TradeDirection -> Input.SelectMsg ResourceSpec -> NextTradeState -> NextTradeState
updateKindMenu dir msg = updateInProgressResource dir (updateIPRMenu msg) 
   
validateAmount : String -> Result String Int 
validateAmount amount =  case (String.toInt amount) of 
        Ok i -> validate (notNegative "Amount") << validate (notZero "Amount") <| (Ok i)
        Err s -> Err "Amount must be an integer" 

updateAmount : TradeDirection -> String -> NextTradeState -> NextTradeState
updateAmount dir amount =
    updateInProgressResource dir (\r -> {r | amount = amount}) >> setTermValidation dir Nothing

updateTurn : TradeDirection -> String -> NextTradeState -> NextTradeState
updateTurn dir paidAt = liftToState 
    (transformTerm dir (\term -> {term | paidAt = paidAt})) >> setTermValidation dir Nothing

type NextTradeState = None | ChooseFaction | 
    InProgress InProgressTrade

notGreater : String -> Int -> Int -> Maybe String
notGreater name k i = if i > k then Just (name ++" must not be greater than "++(toString k)) else Nothing

notNegative : String -> Int -> Maybe String
notNegative name i = if i < 0 then Just (name ++" must not be negative") else Nothing

notZero: String -> Int -> Maybe String
notZero name i = if i == 0 then Just (name ++" must not be zero") else Nothing

parseTurn : Model -> String -> Result String Int
parseTurn model turnString = (case String.startsWith "+" turnString of
    True -> Result.map (\t -> t + model.currentTurn) (String.toInt turnString) 
    False -> (String.toInt turnString)) |> validate (notNegative "Turn") |> validate (notGreater "Turn" 6)

finishTerm : Model -> InProgressTradeTerm -> Result InProgressTradeTerm TradeTerm 
finishTerm model iptt =  Result.mapError (\msg -> {iptt | validationMsg = Just msg}) (case iptt.item of 
    IPR resource -> 
        let 
             spec = maybeToResult (Input.selected (resource.kindSelector)) "No resource kind selected"
             amount = validateAmount resource.amount
             paidAt = parseTurn model iptt.paidAt
        in
            Result.map3 (\s a p -> {item = TIResource (mkResource a s.kind s.size), paidAt = p}) spec amount paidAt
    IPC custom ->  
             let paidAt = parseTurn model iptt.paidAt 
             in Result.map (\p -> {item = Custom custom, paidAt = p}) paidAt)

appendGiven term trade = { trade | termsGiven = trade.termsGiven ++ term}
appendRecieved term trade = { trade | termsRecieved = trade.termsRecieved ++ term}

finishTrade : Model -> InProgressTrade -> Trade
finishTrade model ipt = ipt.tradeInProgress

finishTermForDir : Model -> TradeDirection -> InProgressTrade -> InProgressTrade
finishTermForDir model dir ipt = let trade = ipt.tradeInProgress in
            case dir of
                Given -> case (finishTerm model ipt.inProgressTermGiven) of
                        Ok term -> {ipt | tradeInProgress = appendGiven [term] trade}
                        Err ipterm -> {ipt | inProgressTermGiven = ipterm}
                Recieved -> case (finishTerm model ipt.inProgressTermRecieved) of
                    Ok term -> {ipt | tradeInProgress = appendRecieved [term] trade}
                    Err ipterm -> {ipt | inProgressTermRecieved = ipterm}
              
toTrade : Model -> NextTradeState -> List Trade
toTrade model state = case state of
    None -> []
    ChooseFaction -> []
    InProgress inProgress -> [finishTrade model inProgress]

type alias Model =  {
    trades : List Trade,
    resources : List Resource,
    currentTurn : Int,
    nextTrade : NextTradeState
}

emptyModel : Model
emptyModel = {
    trades = [ { faction = EniEt,
                 termsGiven = [{ item = TIResource (mkResource 3 Green SmallCube), paidAt = 2},
                               { item = TIResource (mkResource 3 Green SmallCube), paidAt = 2},
                               { item = TIResource (mkResource 1 Black LargeCube), paidAt = 2},
                               {item = TIResource (mkResource 10 Ultra Octa), paidAt = 2}],
                 termsRecieved = [{item = TIResource (mkResource 2 White SmallCube), paidAt = 2},
                                   {item = Custom "Lend 3->4 Large Converter", paidAt = 2},
                                   {item = TIResource (mkResource 1 Any SmallCube), paidAt = 2},
                                   {item = TIResource (mkResource 1 Any LargeCube), paidAt = 2},
                                   {item = TIResource (mkResource 1 Wild LargeCube), paidAt = 2}], 
                turnExecuted = 1},
                { faction = Zeth,
                 termsGiven = [{ item = TIResource (mkResource 3 Green SmallCube), paidAt = 2},
                               { item = TIResource (mkResource 1 Black LargeCube), paidAt = 2},
                               {item = TIResource (mkResource 10 Ultra Octa), paidAt = 2}],
                 termsRecieved = [{item = TIResource (mkResource 2 White SmallCube), paidAt = 2},
                                   {item = Custom "Lend 3->4 Large Converter", paidAt = 2},
                                   {item = TIResource (mkResource 1 Any SmallCube), paidAt = 2},
                                   {item = TIResource (mkResource 1 Any LargeCube), paidAt = 2},
                                   {item = TIResource (mkResource 1 Wild LargeCube), paidAt = 2}],
                turnExecuted = 1}],
    resources = [], 
    currentTurn = 1,
    nextTrade = None}

type TradeDirection = Given | Recieved

type alias ItemOwed = {item :TradeItem, faction : Faction}

obligations : Model -> (Trade -> List TradeTerm) -> List ItemOwed
obligations model termFn = List.map (\t -> {item = (Tuple.second t).item, faction = Tuple.first t}) <| 
    List.filter (\t -> (Tuple.second t).paidAt == model.currentTurn) <| 
    flatMap expand  <| 
    List.map (\t -> (t.faction, (List.filter (isFutureTrade t) (termFn t)))) model.trades

isFutureTrade : Trade -> TradeTerm -> Bool
isFutureTrade trade term = trade.turnExecuted /= term.paidAt

youOwe : Model -> List ItemOwed
youOwe model = obligations model .termsGiven

youAreOwed : Model -> List ItemOwed
youAreOwed model = obligations model .termsRecieved

-- UPDATE

type Msg = 
      NoOp
    | NextTurn 
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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    let
        updated = updatePure msg model
    in
        (updated, Cmd.none)
            
updatePure : Msg -> Model -> Model
updatePure msg model = 
  case msg of
    NextTurn -> {model | currentTurn = min 6 (model.currentTurn + 1)}
    DeleteTrade t -> { model | trades = List.filter (\tr -> t /= tr) (model.trades)}
    StartTrade -> {model | nextTrade = ChooseFaction}
    SelectTradeFaction f -> {model | nextTrade = InProgress 
        {tradeInProgress = {faction = f, termsGiven = [], termsRecieved = [], turnExecuted = model.currentTurn}, 
        inProgressTermGiven = defaultTerm model.currentTurn Given,
        inProgressTermRecieved = defaultTerm  model.currentTurn Recieved}}
    DeleteTradeInProgress -> {model | nextTrade = None}
    CompleteTrade -> {model | nextTrade = None, trades = (model.trades ++ (toTrade model (model.nextTrade)))}
    SetResourceKind dir (select) -> {model | nextTrade = (updateKindMenu dir select model.nextTrade)}
    SetResourceAmount dir amountStr -> {model | nextTrade = (updateAmount dir amountStr model.nextTrade)}
    SetTermTurn dir turnString -> {model | nextTrade = (updateTurn dir turnString model.nextTrade)}
    SetCustomTerm dir str -> {model | nextTrade = (updateCustom dir str model.nextTrade)}
    CompleteTerm dir -> {model | nextTrade = (liftToState (finishTermForDir model dir) model.nextTrade)}
    ToggleTermMode dir -> {model | nextTrade = toggleTerm dir model.nextTrade}
    other -> model
    
type Style = 
      NoStyle
    | AppStyle
    | HeaderStyle
    | TradesStyle
    | TurnCounterStyle
    | TradeStyle
    | FactionNameStyle Faction
    | TradeTermStyle
    | TermsStyle
    | DeleteButtonStyle
    | InfoPaneStyle
    | TradePaneStyle
    | TermText
    | TextStyle
    | ContainerStyle
    | ResourceStyle ResourceKind
    | NewTradeButton
    | AddButtonStyle
    | ToggleButtonStyle
    | AddTermStyle
    | TextFieldStyle
    | ResourceSelectStyle
    | SubMenu
    | TurnMarker
    | SubHeader
    | InfoLabel
    | ShipImageStyle
    | ValidationStyle
    | ObligationPaneStyle
    | ObligationHeader
    | YouOweStyle
    | YoureOwedStyle

type Variation = 
      DefaultVar

black = rgb 0 0 0 
white = rgb 255 255 255

stylesheet =
    Style.styleSheet [ 
            style TurnCounterStyle [
                Font.size 20
            ],
            style InfoLabel [
                Font.size 20
            ],
            style HeaderStyle [
                Color.background (rgb 13 94 132),
                Color.text (rgb 255 255 255),
                Font.size 40
            ],
            style TradeTermStyle [
              
            ],
            style TradePaneStyle [
               Border.right 3,
               Border.solid
            ],
            style TurnMarker [
              Font.size 15,
              Color.text (rgb 255 255 255),
              Color.background (rgb 0 0 0)
            ],
            style SubHeader [
               Border.bottom 1,
               Border.solid,
               Color.border (rgb 190 190 190)
            ],
            factionStyle Ktz (rgb 244 188 66) black,
            factionStyle Kjas (rgb 196 72 60) white,
            factionStyle Caylion (rgb 65 173 89) white,
            factionStyle EniEt (rgb 23 40 196) white,
            factionStyle Imdril (rgb 61 217 244) black,
            factionStyle Unity (rgb 211 211 211) black,
            factionStyle Zeth (rgb 133 24 150) white,
            factionStyle Faderan (rgb 255 249 76) black,
            factionStyle Yengii (rgb 91 91 91) white,

            style TradeStyle [
                Shadow.simple,
                Color.background (rgb 240 240 240)
            ],
            style ObligationPaneStyle [
                Shadow.simple,
                Color.background (rgb 240 240 240)
            ],
            style ObligationHeader [
                Font.size 20,
                Color.text (rgb 255 255 255),
                Color.background (rgb 0 0 0)
            ],
            style AddTermStyle [
                Shadow.simple,
                Color.background (rgb 240 240 240)
            ],
            style TermsStyle [
            ],
            style TermText [
                Font.size 13
            ],
            style DeleteButtonStyle [
                Color.background (rgb 200 200 200),
                Color.text (rgb 200 30 30),
                Shadow.simple,
                hover [
                    Color.background (rgb 150 150 150)
                ]
            ],
            style NewTradeButton [
                Color.background (rgb 77 188 101),
                Color.text (rgb 0 0 0),
                Shadow.simple,
                hover [
                    Color.background (rgb 129 239 153)
                ]
            ],
            style AddButtonStyle [
              Color.background (rgb 83 244 66)  
            ],
            style ToggleButtonStyle [
              Color.background (rgb 217 125 232)  
            ],
            resourceStyle Green ,
            resourceStyle White,
            resourceStyle Brown ,
            resourceStyle Black ,
            resourceStyle Blue ,
            resourceStyle Yellow ,
            resourceStyle Ultra,
            resourceStyle Wild ,
            resourceStyle VP ,
            resourceStyle Any,
            style ValidationStyle [
                Color.text white,
                Color.background Color.red,
                Shadow.simple
            ]
        ]

resourceStyle : ResourceKind -> Style.Style Style variation
resourceStyle kind = style (ResourceStyle kind) 
    [Color.background (resourceColor kind), 
    Color.border (rgb 80 80 80),
    Border.all 1,
    Border.solid,
    Font.size 10]

factionStyle : Faction -> Color -> Color -> Style.Style Style variation
factionStyle faction color textColor = style (FactionNameStyle faction) 
    [Color.background color,
    Color.text textColor]

-- VIEW

view : Model -> Html Msg
view model = Element.viewport stylesheet <|
   column AppStyle [height fill, yScrollbar] [
   header,
   row AppStyle [width fill,height fill] [
    el TradePaneStyle [width fill, height fill] (
        column ContainerStyle [spacing 10] [
            row SubHeader [width fill, height (px 30)] [
                          currentTurnDiv (model.currentTurn), 
                          (el ContainerStyle [width fill] (button ToggleButtonStyle [alignRight, onClick NextTurn, padding 5, height fill] (text "Next Turn")))],
            inProgressTradeView model,
            column TradesStyle [spacing 1] (tradeView model.trades)
  ]),
  el InfoPaneStyle [width fill, height fill] 
      (column ContainerStyle [width fill, height fill, spacing 3] [
        row SubHeader [width fill] [el InfoLabel [height (px 30), padding 5] (text "Info")],
        el ContainerStyle [width fill, height (percent 33)] (obligationView (el YouOweStyle [height fill] (text "You Owe")) youOwe model),
        el ContainerStyle [width fill, height (percent 33)] (obligationView (el YoureOwedStyle [height fill] (text "You Are Owed")) youAreOwed model)])]]

obligationView : Element Style Variation Msg -> (Model -> List ItemOwed) -> Model -> Element Style Variation Msg
obligationView header obFn model = 
    let obls = (obFn model) in
    column ContainerStyle [spacing 2, height fill] <|
    [el ObligationHeader [] header] ++ [column ContainerStyle [height fill, yScrollbar]
       (owedForFaction <|
       partition .faction <|
       List.sortBy (getFactionName << .faction) (obFn model))]

owedForFaction : List (List ItemOwed) -> List (Element Style Variation Msg)
owedForFaction itemsGroupedByFaction = List.map
    (\fact -> let faction = (fromJust (List.head fact)).faction in 
        row ObligationPaneStyle [] [
            factionNameplate faction,
            column ContainerStyle [] (List.map owedItemView fact)
        ]) itemsGroupedByFaction

owedItemView owedItem = row ContainerStyle [] (itemView (owedItem.item))

header : Element Style Variation Msg
header = h1 HeaderStyle [width fill, alignLeft, padding 10] (text "Sliderule Confluence")
 
currentTurnDiv : Turn -> Element Style Variation Msg
currentTurnDiv turn = el TurnCounterStyle [] (text ("Turn: " ++ (toString turn) ++ "/6"))

tradeView : List Trade -> List (Element Style Variation Msg)
tradeView trades = 
    List.reverse <| 
    List.map Tuple.first
    (intersperseSections
    Tuple.second
    (makeDivider <<  Tuple.second)
    (List.map (turnLabledTrade DeleteTrade) trades))

makeDivider : Turn -> (Element Style Variation Msg, Turn)
makeDivider turn = (el TurnMarker [width fill] (text ("Turn " ++ (toString turn))), turn)

turnLabledTrade : (Trade -> Msg) -> Trade -> (Element Style Variation Msg, Turn)
turnLabledTrade act trade = (tradeHtml act trade, trade.turnExecuted)

factionNameplate faction = el (FactionNameStyle faction) [center, padding 5, width (percent 17)] (text (getFactionName faction))

tradeHtml : (Trade -> Msg) -> Trade -> Element Style Variation Msg
tradeHtml deleteAction trade = 
    row TradeStyle [alignLeft] [
            factionNameplate trade.faction,
            el TermText [paddingTop 8, paddingLeft 4, width (percent 9)] (text "You Paid"),
            el TermsStyle [padding 3, width (percent 25)] (termsDiv (trade.termsGiven)),
            el TermText [paddingTop 8, paddingLeft 4, width (percent 9)] (text "You Got"),
            el TermsStyle [padding 3, width (percent 25)] (termsDiv (trade.termsRecieved)),
            el ContainerStyle [center, verticalCenter, width (percent 15)] 
                (button DeleteButtonStyle 
                [onClick (deleteAction trade), alignRight, padding 3, width (px 70)] 
                (text "Delete"))]

termsDiv : List TradeTerm -> Element Style Variation Msg
termsDiv terms = wrappedColumn TermsStyle [] (List.map termDiv terms)

termDiv : TradeTerm -> Element Style Variation Msg
termDiv term = 
    let itemElement = itemView term.item
    in row ContainerStyle [center, verticalCenter] 
                                (itemElement ++ 
                                [el TermText [width (px 20)] (text "@"),
                                text (toString term.paidAt)])

itemView : TradeItem -> List (Element Style Variation Msg)
itemView item = case item of
        TIResource resource -> [el TermText [padding 3, width (px 25)] (text (toString resource.amount)), 
                                resourceView resource]
        Custom customTrade -> [paragraph TextStyle [padding 3, width (px 125)] [text customTrade]]

inProgressTradeView : Model -> Element Style Variation Msg
inProgressTradeView model = case (model.nextTrade) of
    InProgress inProgressTrade -> column ContainerStyle [] [
            (tradeHtml (\n -> DeleteTradeInProgress) (inProgressTrade.tradeInProgress)),
            row ContainerStyle [width fill, padding 2] [
                el ContainerStyle [width (percent 25)] empty,
                el ContainerStyle [width (percent 30)] (addTermsView Given model inProgressTrade),
                el ContainerStyle [width (percent 5)] empty,
                el ContainerStyle [width (percent 30)] (addTermsView Recieved model inProgressTrade),
                el ContainerStyle [width (percent 10), height fill] 
                    (button AddButtonStyle [width fill, onClick (CompleteTrade)] (text "Complete"))
            ] 
    ]
    None ->  button NewTradeButton 
                    [center, onClick StartTrade, width (px 100), height (px 40), padding 10] 
                    (text "New Trade")
    ChooseFaction -> el ContainerStyle [center, width (percent 50)] makeFactionPicker
   

addTermsView : TradeDirection -> Model -> InProgressTrade -> Element Style Variation Msg
addTermsView dir model inProgressTrade = el AddTermStyle 
                                        [padding 3, width fill] 
                                        (addTermView model dir (getTerm dir inProgressTrade))

resourceTermControls : Model -> TradeDirection -> InProgressResource -> List (Element Style Variation Msg)
resourceTermControls model dir r = [
         Input.text TextFieldStyle [width (px 25)] {
             onChange = SetResourceAmount dir,
             value = r.amount,
             label = Input.hiddenLabel "Amount",
             options = []
         },
         Input.select ResourceSelectStyle
                    []
                    { label = Input.hiddenLabel "Kind"
                    , with = r.kindSelector
                    , max = 11
                    , options = []
                    , menu =
                        Input.menu SubMenu
                            []
                            [ resourceChoice Green SmallCube, 
                              resourceChoice Brown SmallCube, 
                              resourceChoice White SmallCube, 
                              resourceChoice Black LargeCube,
                              resourceChoice Yellow LargeCube,
                              resourceChoice Blue LargeCube,
                              resourceChoice Ultra Octa,
                              resourceChoice Wild SmallCube,
                              resourceChoice Wild LargeCube,
                              resourceChoice Any SmallCube,
                              resourceChoice Any LargeCube,
                              resourceChoice VP Octa,
                              resourceChoice Ship SmallCube]
                    }]

isIPC : InProgressItem -> Bool
isIPC ipi = case ipi of
    IPR r -> False
    IPC c -> True 

addTermView : Model -> TradeDirection -> InProgressTradeTerm -> Element Style Variation Msg
addTermView model dir term = 
    let controls = case term.item of
        IPR r -> resourceTermControls model dir r
        IPC s -> [ 
            Input.text TextFieldStyle [width fill, height (px 40)] {
             onChange = SetCustomTerm dir,
             value = s,
             label = Input.hiddenLabel "Custom Trade",
             options = []
         }]
    in row AddTermStyle [center, verticalCenter] (
         controls ++
         [el TermText [verticalCenter, width (px 20)] (text "@"),
         Input.text TextFieldStyle [] {
             onChange = SetTermTurn dir,
             value = term.paidAt,
             label = Input.hiddenLabel "Turn Paid",
             options = []
         },
         el ContainerStyle [] 
             (button ToggleButtonStyle 
                 [width (px 20), height (px 20), onClick (ToggleTermMode dir)] 
                 (fullCenter (text (if isIPC (term.item) then "R" else "C")))),
         el ContainerStyle [] 
             (button AddButtonStyle 
                 [width (px 20), height (px 20), onClick (CompleteTerm dir)] 
                 (fullCenter (text "✔"))),
        el ContainerStyle [] 
             (button DeleteButtonStyle 
                 [width (px 20), height (px 20), onClick (CompleteTerm dir)] 
                 (fullCenter (text "❌")))
    ]) |> below [
        case term.validationMsg of
            Just msg -> el ValidationStyle [padding 4] (text msg)
            Nothing -> empty ]

fullCenter : Element Style Variation Msg -> Element Style Variation Msg
fullCenter elem = el ContainerStyle [center, verticalCenter] elem

resourceChoice : ResourceKind -> ResourceSize -> Input.Choice ResourceSpec Style Variation Msg
resourceChoice kind size = Input.choice {kind = kind, size = size} (mkResourceView kind size)

makeFactionPicker : Element Style Variation Msg
makeFactionPicker = row ContainerStyle [width fill] [
    column ContainerStyle [width fill] [
        factionButton Ktz,
        factionButton Kjas,
        factionButton Caylion

    ],
    column ContainerStyle [width fill] [
       factionButton Unity,
       factionButton EniEt,
       factionButton Imdril

    ],
    column ContainerStyle [width fill] [
        factionButton Zeth,
        factionButton Faderan,
        factionButton Yengii
    ]
  ]

factionButton : Faction -> Element Style Variation Msg
factionButton f =  button (FactionNameStyle f) 
                   [padding 5, width fill, onClick (SelectTradeFaction f)] 
                   (text (getFactionName f))

resourceColor : ResourceKind -> Color
resourceColor kind = case kind of
    Green ->  rgb 50 188 89
    White ->  rgb 247 249 248
    Brown ->  rgb 131 92 59
    Black ->  rgb 30 30 30
    Blue ->   rgb 64 210 229
    Yellow -> rgb 237 237 37
    Ultra ->  rgb 249 211 72
    Wild ->   rgb 128 128 128
    VP   ->   rgb 169 78 183
    Any  ->   rgba 210 210 210 0.5
    Ship ->   rgb 0 0 0

resourceText : ResourceKind -> ResourceSize -> String
resourceText kind size = case (kind, size) of
    (VP,_)   ->   "VP"
    (Any, LargeCube)  ->   "??"
    (Any, SmallCube)  ->   "?"
    other ->  ""

resourceDrawing : ResourceSize -> ResourceKind -> Element Style Variation Msg
resourceDrawing size kind = case (size,kind) of
    (_, Ship) ->   image ShipImageStyle [width (px 22), height (px 22)] {src = "spaceship.png", caption = "spaceship"}
    (SmallCube, _)   -> html (cubeSvg 10 10 (resourceColor kind) (resourceText kind size))
    (LargeCube, _)   -> html (cubeSvg 15 13 (resourceColor kind) (resourceText kind size))
    (Octa, _) -> html (octagonSvg 8 12 (resourceColor kind) (resourceText kind size))

mkResourceView : ResourceKind -> ResourceSize -> Element Style Variation Msg
mkResourceView kind size = el ContainerStyle [center, verticalCenter, padding 5]  
                             (resourceDrawing size kind)

resourceView : Resource -> Element Style Variation Msg
resourceView resource = mkResourceView resource.rType resource.size

cubeSvg : Float -> Int -> Color -> String -> Svg Msg
cubeSvg size fontSize color label = 
        Svg.svg [Attr.width (round size), Attr.height (round size)] [(Geo.polygon2d
        [ Attributes.stroke "black"
        , Attributes.fill (colorToCssRgb color)
        , Attributes.strokeWidth "1"
        ]
        (Polygon2d.convexHull 
            [ Point2d.fromCoordinates ( 0, 0 )
            , Point2d.fromCoordinates ( 0, size )
            , Point2d.fromCoordinates ( size, size )
            , Point2d.fromCoordinates ( size, 0 )
            ]
        )),
        Svg.text_ [Attributes.x (toString (size/2)), 
                   Attributes.y (toString (size/2 +1)), 
                   Attributes.textAnchor "middle",
                   Attributes.alignmentBaseline "middle",
                   Attributes.fontSize (toString fontSize)] 
                   [Svg.text label]]

octagonSvg : Float -> Int -> Color -> String -> Svg Msg
octagonSvg size fontSize color label = let 
            s = size / (sqrt 2)
            l =  (size + (2 * s))
        in
        Svg.svg [Attr.width (round l), Attr.height (round l)] [(Geo.polygon2d
        [ Attributes.shapeRendering "geometricPrecision",
          Attributes.stroke "black"
        , Attributes.fill (colorToCssRgb color)
        , Attributes.strokeWidth "0.5%"
        ]
        (Polygon2d.convexHull 
            [ Point2d.fromCoordinates ( 0, s ),
              Point2d.fromCoordinates ( 0, s + size ),
              Point2d.fromCoordinates ( s, s + size + s ),
              Point2d.fromCoordinates ( s + size, s + size + s ),
              Point2d.fromCoordinates ( s + size + s, s + size ),
              Point2d.fromCoordinates ( s + size + s, s),
              Point2d.fromCoordinates ( s + size, 0),
              Point2d.fromCoordinates ( s, 0)
            ]
        )),
        Svg.text_ [Attributes.x (toString (l/2)), 
                   Attributes.y (toString (l/2 +1)), 
                   Attributes.textAnchor "middle",
                   Attributes.alignmentBaseline "middle",
                   Attributes.fontSize (toString fontSize)] 
                   [Svg.text label]]
    