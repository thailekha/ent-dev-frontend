module ViewComponents exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (..)
import Debug
import Time.Date as Date
import RemoteData exposing (WebData)
import Components.Portfolio as Portfolio
import Components.LiveData as LiveData
import State exposing (..)
import Json.Decode as Decode


liveDataSelectBox : Html Msg
liveDataSelectBox =
    select
        [ onChange SetLiveDataUrl
        , cssSelectBox
        ]
        [ option [ attribute "disabled" "", attribute "selected" "", value "" ] [ text "" ]
        , option [ value "/test?n=0" ] [ text "Reset Original" ]
        , option [ value "/test?n=1" ] [ text "case1 +10%" ]
        , option [ value "/test?n=2" ] [ text "case2 -10%" ]
        , option [ value "/test?n=3" ] [ text "case3 +20%" ]
        , option [ value "/test?n=4" ] [ text "case4 -20%" ]
        , option [ value "/all" ] [ text "/all" ]
        ]


cashHoldingRow : Model -> Html msg
cashHoldingRow model =
    let
        cash =
            tryGetCashHolding model |> toString
    in
        tr []
            (List.concat <|
                [ [ td []
                        [ text "Cash Holding" ]
                  ]
                , List.repeat 4 (td [] [])
                , [ td []
                        [ text "1" ]
                  , td []
                        []
                  , td []
                        [ text cash ]
                  , td []
                        [ text cash ]
                  , td []
                        []
                  , td []
                        [ text cash ]
                  ]
                , List.repeat 4 (td [] [])
                ]
            )


tableHeadings : List String
tableHeadings =
    [ "Description / Narrative", "Exchange", "Symbol", "Date In", "Date Out", "Qty", "Total Qty", "Cost", "Purchase Price", "Price", "Value", "Detail Gain / Loss", "Cumulative Gain/Loss", "% Gain/Loss", "Sell Costs" ]


maybeLivePrice : WebData LiveData.Data -> Html Msg
maybeLivePrice response =
    case response of
        RemoteData.NotAsked ->
            text "Stocks not fetched ..."

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success _ ->
            text "Live data loaded."

        RemoteData.Failure error ->
            text (toString error)


tryGetCashHolding : Model -> Float
tryGetCashHolding model =
    case model.portfolio of
        RemoteData.Success p ->
            Portfolio.tryGetCashHolding p

        _ ->
            0


tableHeadingsRow : Html msg
tableHeadingsRow =
    tr []
        (tableHeadings
            |> List.map (\h -> th [] [ text h ])
        )


getTotal : Model -> List Portfolio.Holding -> Portfolio.Total
getTotal model holdings =
    let
        ( p, v, s, n ) =
            holdings
                |> List.map
                    (\holding ->
                        holding.shares
                            |> List.map
                                (\share ->
                                    case model.livePriceWebData of
                                        RemoteData.Success _ ->
                                            case (Portfolio.getFullShare holding share model.livePrice) of
                                                Ok fullShare ->
                                                    ( fullShare.purchasePrice * (toFloat fullShare.quantity), fullShare.value, fullShare.sellingCost, fullShare.detailGainOrLoss )

                                                Err _ ->
                                                    ( 0, 0, 0, 0 )

                                        _ ->
                                            ( 0, 0, 0, 0 )
                                )
                    )
                |> List.concat
                |> List.foldl
                    (\( p, v, s, n ) ( accP, accV, accS, accN ) ->
                        Debug.log "Plusing" (toString ( p, v, s, n ))
                            |> always ( p + accP, v + accV, s + accS, n + accN )
                    )
                    ( 0, 0, 0, 0 )
    in
        { purchasePrice = p + (tryGetCashHolding model)
        , sellCost = s
        , gpv = v + (tryGetCashHolding model)
        , gpvAfterSell = v - s + (tryGetCashHolding model)
        , net = n
        }


dateString : String -> String
dateString iso =
    case Date.fromISO8601 iso of
        Ok date ->
            toString (Date.day date) ++ "/" ++ toString (Date.month date) ++ "/" ++ toString (Date.year date)

        Err _ ->
            ""


gainLossCss : Float -> List (Attribute msg)
gainLossCss number =
    if (number < 0) then
        [ style [ ( "color", "red" ) ] ]
    else
        [ style [ ( "color", "green" ) ] ]


cssSelectBox : Html.Attribute msg
cssSelectBox =
    style
        [ ( "border", "1px solid #ccc" )
        , ( "font-size", "16px" )
        , ( "height", "34px" )
        , ( "width", "268px" )
        ]


onChange : (String -> Msg) -> Html.Attribute Msg
onChange tagger =
    on "change" (Decode.map tagger Html.Events.targetValue)
