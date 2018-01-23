module Views exposing (view)

import Html exposing (..)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (..)
import Debug
import Time.Date as Date
import Round
import RemoteData exposing (WebData)
import Components.Portfolio as Portfolio
import Components.LiveData as LiveData
import State exposing (..)
import Json.Decode as Decode


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Investments Portfolio Application" ]
        , liveDataSelectBox
        , br [] []
        , button [ onClick GetPortfolio ] [ text "Fetch portfolio" ]
        , button [ onClick GetLivePrice ] [ text "Fetch live" ]
        , case model.portfolio of
            RemoteData.NotAsked ->
                text "Portfolio not fetched ..."

            RemoteData.Loading ->
                text "Loading..."

            RemoteData.Success p ->
                portfolioView model p.user

            RemoteData.Failure error ->
                text (toString error)
        ]


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


tableHeadings : List String
tableHeadings =
    [ "Description / Narrative", "Exchange", "Symbol", "Date In", "Date Out", "Qty", "Total Qty", "Cost", "Purchase Price", "Price", "Value", "Detail Gain / Loss", "Cumulative Gain/Loss", "% Gain/Loss", "Sell Costs" ]


tableHeadingsRow : Html msg
tableHeadingsRow =
    tr []
        (tableHeadings
            |> List.map (\h -> th [] [ text h ])
        )


tryGetCashHolding : Model -> Float
tryGetCashHolding model =
    case model.portfolio of
        RemoteData.Success p ->
            Portfolio.tryGetCashHolding p

        _ ->
            0


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


portfolioView : Model -> Portfolio.Portfolio -> Html Msg
portfolioView model portfolio =
    let
        net =
            portfolio.holdings
                |> List.map
                    (\holding ->
                        holding.shares
                            |> List.map
                                (\share ->
                                    case model.livePriceWebData of
                                        RemoteData.Success _ ->
                                            case (Portfolio.getFullShare holding share model.livePrice) of
                                                Ok fullShare ->
                                                    fullShare.detailGainOrLoss

                                                Err _ ->
                                                    0

                                        _ ->
                                            0
                                )
                    )
                |> List.concat
                |> List.foldl (+) 0
    in
        div []
            [ h3 (gainLossCss net) [ text ("Net Profit/Loss: " ++ (toString net)) ]
            , h5 [] [ text <| toString <| getTotal model portfolio.holdings ]
            , table [ attribute "border" "1" ]
                (List.concat
                    [ [ tableHeadingsRow
                      , cashHoldingRow model
                      ]
                    , List.concat <| List.map (holdingView model) portfolio.holdings
                    --, List.concat <| List.map (holdingView model) portfolio.stocksSold
                    ]
                )
            ]


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


holdingView : Model -> Portfolio.Holding -> List (Html Msg)
holdingView model holding =
    let
        ( rows, totalRow ) =
            ( holding.shares
                |> List.map
                    (\share ->
                        case model.livePriceWebData of
                            RemoteData.Success _ ->
                                case (Portfolio.getFullShare holding share model.livePrice) of
                                    Ok fullShare ->
                                        fullShareView fullShare

                                    Err err ->
                                        Debug.log "cannot display fullshare" err
                                            |> always (shareView holding share)

                            _ ->
                                shareView holding share
                    )
            , [ tr [] <|
                    List.concat
                        [ List.repeat 6 (td [] [ text "" ])
                        , [ td []
                                [ holding.shares
                                    |> List.map (\s -> s.quantity)
                                    |> List.foldl (+) 0
                                    |> toString
                                    |> text
                                ]
                          ]
                        , List.repeat 5 (td [] [ text "" ])
                        , [ let
                                cummulativePercentage =
                                    holding.shares
                                        |> List.map
                                            (\share ->
                                                case model.livePriceWebData of
                                                    RemoteData.Success _ ->
                                                        case (Portfolio.getFullShare holding share model.livePrice) of
                                                            Ok fullShare ->
                                                                fullShare.detailGainOrLoss

                                                            Err _ ->
                                                                0

                                                    _ ->
                                                        0
                                            )
                                        |> List.foldl (+) 0
                            in
                                td
                                    (gainLossCss cummulativePercentage)
                                    [ cummulativePercentage
                                        |> Round.round 2
                                        |> toString
                                        |> text
                                    ]
                          ]
                        , List.repeat 2 (td [] [ text "" ])
                        ]
              ]
            )
    in
        List.append rows totalRow


shareView : Portfolio.Holding -> Portfolio.Share -> Html Msg
shareView holding share =
    tr []
        [ td []
            [ text holding.displayName ]
        , td []
            [ text holding.exchange ]
        , td []
            [ text holding.symbol ]
        , td []
            [ text (dateString share.dateIn) ]
        , td []
            [ (case share.dateOut of
                Just date_out ->
                    text (dateString date_out)

                Nothing ->
                    text ""
              )
            ]
        , td []
            [ text (toString share.quantity) ]
        , td []
            [ text "" ]
        , td []
            [ text "" ]
        , td []
            [ text (toString share.purchasePrice) ]
        , td []
            [ text "" ]
        , td []
            [ text "" ]
        , td []
            [ text "" ]
        , td []
            [ text "" ]
        , td []
            [ text "" ]
        , td []
            [ text "" ]
        ]


fullShareView : Portfolio.FullShare -> Html Msg
fullShareView share =
    tr []
        [ td []
            [ text share.displayName ]
        , td []
            [ text share.exchange ]
        , td []
            [ text share.symbol ]
        , td []
            [ text (dateString share.dateIn) ]
        , td []
            [ (case share.dateOut of
                Just date_out ->
                    text (dateString date_out)

                Nothing ->
                    text ""
              )
            ]
        , td []
            [ text (toString share.quantity) ]
        , td []
            [ text "" ]
        , td []
            [ text (toString share.cost) ]
        , td []
            [ text (toString share.purchasePrice) ]
        , td []
            [ text (toString share.price) ]
        , td []
            [ text (toString <| Round.round 2 share.value) ]
        , td
            (gainLossCss share.detailGainOrLoss)
            [ text (toString <| Round.round 2 share.detailGainOrLoss) ]
        , td []
            [ text "" ]
        , td
            (gainLossCss share.percentageGainOrLoss)
            [ text (toString <| Round.round 2 share.percentageGainOrLoss) ]
        , td []
            [ text (toString <| Round.round 2 share.sellingCost) ]
        ]
