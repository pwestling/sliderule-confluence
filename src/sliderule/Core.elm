module Sliderule.Core exposing (Style(..), Variation(..), black, cubeSvg, currentTurnDiv, emptyModel, factionButton, factionNameplate, factionStyle, fullCenter, header, hover, init, itemView, main, makeDivider, makeFactionPicker, mkResourceView, obligationView, octagonSvg, owedForFaction, owedItemView, resourceChoice, resourceColor, resourceDrawing, resourceStyle, resourceText, resourceView, rgb, style, stylesheet, subs, termDiv, termsDiv, tradeHtml, tradeInputStyle, tradeView, turnLabledTrade, update, updatePure, view, viewWithTitle, white)

import Browser as Browser
import Color018 as Rgb
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Element.Input as Input
import Geometry.Svg as Geo
import Html exposing (Html)
import Html.Attributes as Attr
import Maybe as Maybe
import Point2d exposing (Point2d)
import Polygon2d exposing (Polygon2d)
import Sliderule.TradeParser exposing (..)
import Sliderule.Types exposing (..)
import Sliderule.Util exposing (..)
import Style as Style
import Style.Border as Border
import Style.Color as Color
import Style.Filter as Filter
import Style.Font as Font
import Style.Shadow as Shadow
import Svg exposing (Svg)
import Svg.Attributes as Attributes


main : Program () Model Msg
main =
    Browser.document { init = \f -> init, view = viewWithTitle, update = update, subscriptions = subs }


viewWithTitle : Model -> Browser.Document Msg
viewWithTitle model =
    { title = "Sliderule Confluence", body = [ view model ] }


subs : Model -> Sub Msg
subs model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )



-- MODEL
-- emptyModel : Model
-- emptyModel = {
--     trades = [ { faction = EniEt,
--                  termsGiven = [{ item = TIResource (mkResource 3 Green SmallCube), paidAt = 2},
--                                { item = TIResource (mkResource 3 Green SmallCube), paidAt = 2},
--                                { item = TIResource (mkResource 1 Black LargeCube), paidAt = 2},
--                                {item = TIResource (mkResource 10 Ultra Octa), paidAt = 2}],
--                  termsRecieved = [{item = TIResource (mkResource 2 White SmallCube), paidAt = 2},
--                                    {item = Custom "Lend 3->4 Large Converter", paidAt = 2},
--                                    {item = TIResource (mkResource 1 Any SmallCube), paidAt = 2},
--                                    {item = TIResource (mkResource 1 Any LargeCube), paidAt = 2},
--                                    {item = TIResource (mkResource 1 Wild LargeCube), paidAt = 2}],
--                 turnExecuted = 1},
--                 { faction = Zeth,
--                  termsGiven = [{ item = TIResource (mkResource 3 Green SmallCube), paidAt = 2},
--                                { item = TIResource (mkResource 1 Black LargeCube), paidAt = 2},
--                                {item = TIResource (mkResource 10 Ultra Octa), paidAt = 2}],
--                  termsRecieved = [{item = TIResource (mkResource 2 White SmallCube), paidAt = 2},
--                                    {item = Custom "Lend 3->4 Large Converter", paidAt = 2},
--                                    {item = TIResource (mkResource 1 Any SmallCube), paidAt = 2},
--                                    {item = TIResource (mkResource 1 Any LargeCube), paidAt = 2},
--                                    {item = TIResource (mkResource 1 Wild LargeCube), paidAt = 2}],
--                 turnExecuted = 1}],
--     resources = [],
--     currentTurn = 1,
--     nextTrade = None}


emptyModel : Model
emptyModel =
    { trades = []
    , resources = []
    , currentTurn = 1
    , nextTradeString = ""
    , nextTrade = Nothing
    , nextTradeValid = True
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updated =
            updatePure msg model
    in
    ( updated, Cmd.none )


updatePure : Msg -> Model -> Model
updatePure msg model =
    case msg of
        ChangeTurn i ->
            { model | currentTurn = max 1 (min 6 (model.currentTurn + i)) }

        DeleteTrade t ->
            { model | trades = List.filter (\tr -> t /= tr) model.trades }

        CompleteTrade ->
            case model.nextTradeValid of
                True ->
                    { model | nextTrade = Nothing, nextTradeString = "", trades = model.trades ++ maybeToList model.nextTrade }

                False ->
                    model

        ChangeTradeString s ->
            case parseTrade model.currentTurn s of
                Just trade ->
                    { model | nextTrade = Just trade, nextTradeString = s, nextTradeValid = True }

                Nothing ->
                    { model | nextTradeString = s, nextTradeValid = False }

        other ->
            model


type Style
    = NoStyle
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
    | TradeInputStyle
    | InvalidTradeInputStyle
    | UnsubmittedTradeStyle


type Variation
    = DefaultVar


black =
    Rgb.black


white =
    Rgb.white


style =
    Style.style


hover =
    Style.hover


rgb : Int -> Int -> Int -> Style.Color
rgb r g b =
    Style.rgb (toFloat r / 255) (toFloat g / 255) (toFloat b / 255)


stylesheet =
    Style.styleSheet
        [ style TurnCounterStyle
            [ Font.size 20
            ]
        , style InfoLabel
            [ Font.size 20
            ]
        , style HeaderStyle
            [ Color.background (rgb 13 94 132)
            , Color.text (rgb 255 255 255)
            , Font.size 40
            ]
        , style TradeTermStyle
            []
        , style TradeInputStyle
            [ Border.solid
            , Border.all 1
            , Color.border (toStyle Rgb.black)
            ]
        , style InvalidTradeInputStyle
            [ Border.solid
            , Border.all 1
            , Color.border (toStyle Rgb.red)
            ]
        , style TradePaneStyle
            [ Border.right 3
            , Border.solid
            ]
        , style TurnMarker
            [ Font.size 15
            , Color.text (rgb 255 255 255)
            , Color.background (rgb 0 0 0)
            ]
        , style SubHeader
            [ Border.bottom 1
            , Border.solid
            , Color.border (rgb 190 190 190)
            ]
        , factionStyle Ktz (Rgb.rgb 244 188 66) black
        , factionStyle Kjas (Rgb.rgb 196 72 60) white
        , factionStyle Caylion (Rgb.rgb 65 173 89) white
        , factionStyle EniEt (Rgb.rgb 23 40 196) white
        , factionStyle Imdril (Rgb.rgb 61 217 244) black
        , factionStyle Unity (Rgb.rgb 211 211 211) black
        , factionStyle Zeth (Rgb.rgb 133 24 150) white
        , factionStyle Faderan (Rgb.rgb 255 249 76) black
        , factionStyle Yengii (Rgb.rgb 91 91 91) white
        , style TradeStyle
            [ Shadow.simple
            , Color.background (rgb 240 240 240)
            ]
        , style UnsubmittedTradeStyle
            [ Filter.opacity 70
            , Shadow.simple
            , Color.background (rgb 240 240 240)
            ]
        , style ObligationPaneStyle
            [ Shadow.simple
            , Color.background (rgb 240 240 240)
            ]
        , style ObligationHeader
            [ Font.size 20
            , Color.text (rgb 255 255 255)
            , Color.background (rgb 0 0 0)
            ]
        , style AddTermStyle
            [ Shadow.simple
            , Color.background (rgb 240 240 240)
            ]
        , style TermsStyle
            []
        , style TermText
            [ Font.size 13
            ]
        , style DeleteButtonStyle
            [ Color.background (rgb 200 200 200)
            , Color.text (rgb 200 30 30)
            , Shadow.simple
            , hover
                [ Color.background (rgb 150 150 150)
                ]
            ]
        , style NewTradeButton
            [ Color.background (rgb 77 188 101)
            , Color.text (rgb 0 0 0)
            , Shadow.simple
            , hover
                [ Color.background (rgb 129 239 153)
                ]
            ]
        , style AddButtonStyle
            [ Color.background (rgb 83 244 66)
            ]
        , style ToggleButtonStyle
            [ Color.background (rgb 217 125 232)
            ]
        , resourceStyle Green
        , resourceStyle White
        , resourceStyle Brown
        , resourceStyle Black
        , resourceStyle Blue
        , resourceStyle Yellow
        , resourceStyle Ultra
        , resourceStyle Wild
        , resourceStyle VP
        , resourceStyle Any
        , style ValidationStyle
            [ Color.text (toStyle white)
            , Color.background (toStyle Rgb.red)
            , Shadow.simple
            ]
        ]


resourceStyle : ResourceKind -> Style.Style Style variation
resourceStyle kind =
    style (ResourceStyle kind)
        [ Color.background (toStyle (resourceColor kind))
        , Color.border (rgb 80 80 80)
        , Border.all 1
        , Border.solid
        , Font.size 10
        ]


factionStyle : Faction -> Rgb.Color -> Rgb.Color -> Style.Style Style variation
factionStyle faction color textColor =
    style (FactionNameStyle faction)
        [ Color.background (toStyle color)
        , Color.text (toStyle textColor)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    Element.viewport stylesheet <|
        column AppStyle
            [ height fill, yScrollbar ]
            [ header
            , row AppStyle
                [ width fill, height fill ]
                [ tradePane model
                , infoPane model
                ]
            ]


tradePane model = el TradePaneStyle
                    [ width fill, height fill ]
                    (column ContainerStyle
                        [ spacing 10 ]
                        [ row SubHeader
                            [ width fill, height (px 30) ]
                            [ currentTurnDiv model.currentTurn
                            , el ContainerStyle
                                [ width fill ]
                                (row ContainerStyle
                                    [ width fill, alignRight, spacing 2 ]
                                    [ button ToggleButtonStyle [ alignRight, onClick (ChangeTurn -1), padding 5, height fill ] (text "Prev Turn")
                                    , button ToggleButtonStyle [ alignRight, onClick (ChangeTurn 1), padding 5, height fill ] (text "Next Turn")
                                    ]
                                )
                            ]
                        , row ContainerStyle
                            [ spacing 5, width fill, height (px 30) ]
                            [ Input.text (tradeInputStyle model.nextTradeValid)
                                [ center, spacing 3, height (px 32), width (percent 98) ]
                                { onChange = ChangeTradeString
                                , value = model.nextTradeString
                                , label = Input.hiddenLabel "Trade Input"
                                , options = []
                                }
                            , button AddButtonStyle [ alignRight, onClick CompleteTrade, padding 3 ] (text "Submit")
                            ]
                        , column TradesStyle [ spacing 1 ] (tradeView model.nextTrade model.trades)
                        ]
                    )

infoPane model = el InfoPaneStyle
                    [ width fill, height fill ]
                    (column ContainerStyle
                        [ width fill, height fill, spacing 3 ]
                        [ row SubHeader [ width fill ] [ el InfoLabel [ height (px 30), padding 5 ] (text "Info") ]
                        , el ContainerStyle [ width fill, height (percent 45) ] (obligationView (el YouOweStyle [ height fill ] (text "You Owe")) youOwe model)
                        , el ContainerStyle [ width fill, height (percent 45) ] (obligationView (el YoureOwedStyle [ height fill ] (text "You Are Owed")) youAreOwed model)
                        ]
                    )

tradeInputStyle : Bool -> Style
tradeInputStyle t =
    if t then
        TradeInputStyle

    else
        InvalidTradeInputStyle


obligationView : Element Style Variation Msg -> (Model -> List ItemOwed) -> Model -> Element Style Variation Msg
obligationView headerEl obFn model =
    let
        obls =
            obFn model
    in
    column ContainerStyle [ spacing 2, height fill ] <|
        [ el ObligationHeader [] headerEl ]
            ++ [ column ContainerStyle
                    [ height fill, yScrollbar ]
                    (owedForFaction <|
                        partition .faction <|
                            List.sortBy (getFactionName << .faction) (obFn model)
                    )
               ]


owedForFaction : List ( Faction, List ItemOwed ) -> List (Element Style Variation Msg)
owedForFaction itemsGroupedByFaction =
    List.map
        (\fact ->
            let
                faction =
                    Tuple.first fact
            in
            row ObligationPaneStyle
                []
                [ factionNameplate faction
                , column ContainerStyle [] (List.map owedItemView (Tuple.second fact))
                ]
        )
        itemsGroupedByFaction


owedItemView owedItem =
    row ContainerStyle [] (itemView owedItem.item)


header : Element Style Variation Msg
header =
    h1 HeaderStyle [ width fill, alignLeft, padding 10 ] (text "Sliderule Confluence")


currentTurnDiv : Turn -> Element Style Variation Msg
currentTurnDiv turn =
    el TurnCounterStyle [] (text ("Turn: " ++ String.fromInt turn ++ "/6"))


tradeView : Maybe Trade -> List Trade -> List (Element Style Variation Msg)
tradeView nextTrade remTrades =
    let
        trades =
            List.map (Tuple.pair TradeStyle) remTrades
                ++ List.map (Tuple.pair UnsubmittedTradeStyle) (maybeToList nextTrade)
    in
    List.reverse <|
        List.map Tuple.first
            (intersperseSections
                Tuple.second
                (makeDivider << Tuple.second)
                (List.map (turnLabledTrade DeleteTrade) trades)
            )


makeDivider : Turn -> ( Element Style Variation Msg, Turn )
makeDivider turn =
    ( el TurnMarker [ width fill ] (text ("Turn " ++ String.fromInt turn)), turn )


turnLabledTrade : (Trade -> Msg) -> ( Style, Trade ) -> ( Element Style Variation Msg, Turn )
turnLabledTrade act trade =
    ( tradeHtml act trade, (Tuple.second trade).turnExecuted )


factionNameplate faction =
    el (FactionNameStyle faction) [ center, padding 5, width (percent 17) ] (text (getFactionName faction))


tradeHtml : (Trade -> Msg) -> ( Style, Trade ) -> Element Style Variation Msg
tradeHtml deleteAction tradeAndStyle =
    let
        trade =
            Tuple.second tradeAndStyle
    in
    row (Tuple.first tradeAndStyle)
        [ alignLeft ]
        [ factionNameplate trade.faction
        , el TermText [ paddingTop 8, paddingLeft 4, width (percent 9) ] (text "You Paid")
        , el TermsStyle [ padding 3, width (percent 25) ] (termsDiv trade.termsGiven)
        , el TermText [ paddingTop 8, paddingLeft 4, width (percent 9) ] (text "You Got")
        , el TermsStyle [ padding 3, width (percent 25) ] (termsDiv trade.termsRecieved)
        , el ContainerStyle
            [ center, verticalCenter, width (percent 15) ]
            (button DeleteButtonStyle
                [ onClick (deleteAction trade), alignRight, padding 3, width (px 70) ]
                (text "Delete")
            )
        ]


termsDiv : List TradeTerm -> Element Style Variation Msg
termsDiv terms =
    wrappedColumn TermsStyle [] (List.map termDiv terms)


termDiv : TradeTerm -> Element Style Variation Msg
termDiv term =
    let
        itemElement =
            itemView term.item
    in
    row ContainerStyle
        [ center, verticalCenter ]
        (itemElement
            ++ [ el TermText [ width (px 20) ] (text "@")
               , text (String.fromInt term.paidAt)
               ]
        )


itemView : TradeItem -> List (Element Style Variation Msg)
itemView item =
    case item of
        TIResource resource ->
            [ el TermText [ padding 3, width (px 50) ] (text (String.fromInt resource.amount))
            , resourceView resource
            ]

        Custom customTrade ->
            [ paragraph TextStyle [ padding 3, width (px 125) ] [ text customTrade ] ]


fullCenter : Element Style Variation Msg -> Element Style Variation Msg
fullCenter elem =
    el ContainerStyle [ center, verticalCenter ] elem


resourceChoice : ResourceKind -> ResourceSize -> Input.Choice ResourceSpec Style Variation Msg
resourceChoice kind size =
    Input.choice { kind = kind, size = size } (mkResourceView kind size)



-- This code left for use in game-creation UI


makeFactionPicker : Element Style Variation Msg
makeFactionPicker =
    row ContainerStyle
        [ width fill ]
        [ column ContainerStyle
            [ width fill ]
            [ factionButton Ktz
            , factionButton Kjas
            , factionButton Caylion
            ]
        , column ContainerStyle
            [ width fill ]
            [ factionButton Unity
            , factionButton EniEt
            , factionButton Imdril
            ]
        , column ContainerStyle
            [ width fill ]
            [ factionButton Zeth
            , factionButton Faderan
            , factionButton Yengii
            ]
        ]


factionButton : Faction -> Element Style Variation Msg
factionButton f =
    button (FactionNameStyle f)
        [ padding 5, width fill ]
        (text (getFactionName f))


resourceColor : ResourceKind -> Rgb.Color
resourceColor kind =
    case kind of
        Green ->
            Rgb.rgb 50 188 89

        White ->
            Rgb.rgb 247 249 248

        Brown ->
            Rgb.rgb 131 92 59

        Black ->
            Rgb.rgb 30 30 30

        Blue ->
            Rgb.rgb 64 210 229

        Yellow ->
            Rgb.rgb 237 237 37

        Ultra ->
            Rgb.rgb 249 211 72

        Wild ->
            Rgb.rgb 128 128 128

        VP ->
            Rgb.rgb 169 78 183

        Any ->
            Rgb.rgba 210 210 210 0.5

        Ship ->
            Rgb.rgb 0 0 0

        NoKind ->
            Rgb.rgb 0 0 0


resourceText : ResourceKind -> ResourceSize -> String
resourceText kind size =
    case ( kind, size ) of
        ( VP, _ ) ->
            "VP"

        ( Any, LargeCube ) ->
            "??"

        ( Any, SmallCube ) ->
            "?"

        other ->
            ""


resourceDrawing : ResourceSize -> ResourceKind -> Element Style Variation Msg
resourceDrawing size kind =
    case ( size, kind ) of
        ( _, Ship ) ->
            image ShipImageStyle [ width (px 22), height (px 22) ] { src = "../../resources/spaceship.png", caption = "spaceship" }

        ( SmallCube, _ ) ->
            html (cubeSvg 10 10 (resourceColor kind) (resourceText kind size))

        ( LargeCube, _ ) ->
            html (cubeSvg 15 13 (resourceColor kind) (resourceText kind size))

        ( Octa, _ ) ->
            html (octagonSvg 8 12 (resourceColor kind) (resourceText kind size))

        ( _, _ ) ->
            text "?"


mkResourceView : ResourceKind -> ResourceSize -> Element Style Variation Msg
mkResourceView kind size =
    el ContainerStyle
        [ center, verticalCenter, padding 5 ]
        (resourceDrawing size kind)


resourceView : Resource -> Element Style Variation Msg
resourceView resource =
    mkResourceView resource.rType resource.size


cubeSvg : Float -> Int -> Rgb.Color -> String -> Svg Msg
cubeSvg size fontSize color label =
    Svg.svg [ Attr.width (round size), Attr.height (round size) ]
        [ Geo.polygon2d
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
            )
        , Svg.text_
            [ Attributes.x (toStringF (size / 2))
            , Attributes.y (toStringF (size / 2 + 1))
            , Attributes.textAnchor "middle"
            , Attributes.alignmentBaseline "middle"
            , Attributes.fontSize (toString fontSize)
            ]
            [ Svg.text label ]
        ]


octagonSvg : Float -> Int -> Rgb.Color -> String -> Svg Msg
octagonSvg size fontSize color label =
    let
        s =
            size / sqrt 2

        l =
            size + (2 * s)
    in
    Svg.svg [ Attr.width (round l), Attr.height (round l) ]
        [ Geo.polygon2d
            [ Attributes.shapeRendering "geometricPrecision"
            , Attributes.stroke "black"
            , Attributes.fill (colorToCssRgb color)
            , Attributes.strokeWidth "0.5%"
            ]
            (Polygon2d.convexHull
                [ Point2d.fromCoordinates ( 0, s )
                , Point2d.fromCoordinates ( 0, s + size )
                , Point2d.fromCoordinates ( s, s + size + s )
                , Point2d.fromCoordinates ( s + size, s + size + s )
                , Point2d.fromCoordinates ( s + size + s, s + size )
                , Point2d.fromCoordinates ( s + size + s, s )
                , Point2d.fromCoordinates ( s + size, 0 )
                , Point2d.fromCoordinates ( s, 0 )
                ]
            )
        , Svg.text_
            [ Attributes.x (toStringF (l / 2))
            , Attributes.y (toStringF (l / 2 + 1))
            , Attributes.textAnchor "middle"
            , Attributes.alignmentBaseline "middle"
            , Attributes.fontSize (toString fontSize)
            ]
            [ Svg.text label ]
        ]
