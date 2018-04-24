module Views exposing (view)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import RemoteData exposing (WebData)
import Components.Portfolio as Portfolio
import Components.Auth as Auth
import State exposing (..)
import ViewComponents exposing (..)
import Dict


view : Model -> Html Msg
view model =
    div [ class "container", style [ ( "margin-top", "30px" ), ( "text-align", "center" ) ] ]
        [ case model.auth.authenticationState of
            Auth.LoggedOut ->
                div []
                    [ loginForm model
                    ]

            Auth.LoggedIn creds ->
                loggedinView model
        ]


loggedinView : Model -> Html Msg
loggedinView model =
    div []
        [ button [ onClick Logout ] [ text "Logout" ]
        , h1 [] [ text "Investments Portfolio Application" ]
        , liveDataSelectBox
        , br [] []
        , button [ onClick GetPortfolio ] [ text "Fetch portfolio" ]
        , button [ onClick GetLivePrice ] [ text "Fetch live" ]
        , br [] []
        , case model.user of
            RemoteData.NotAsked ->
                text "Portfolio not fetched ..."

            RemoteData.Loading ->
                text "Loading..."

            RemoteData.Success p ->
                div []
                    [ button [ onClick ResetPortfolio ] [ text "Reset Portfolio" ]
                    , portfolioView model p.portfolio
                    , hr [] []
                    , sellView model p.portfolio
                    , hr [] []
                    , buyView model
                    ]

            RemoteData.Failure error ->
                text (toString error)
        ]


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
            , h3 [] [ text "Holdings" ]
            , table [ attribute "border" "1" ]
                (List.concat
                    [ [ tableHeadingsRow
                      , cashHoldingRow model
                      ]
                    , List.concat <| List.map (holdingView model) <| Portfolio.sortHoldings <| portfolio.holdings
                    ]
                )
            , h3 [] [ text "Stock solds" ]
            , table [ attribute "border" "1" ]
                (List.concat
                    [ [ tableHeadingsRow
                      ]
                    , List.concat <| List.map (holdingView model) portfolio.stocksSold
                    ]
                )
            ]


sellView : Model -> Portfolio.Portfolio -> Html Msg
sellView model p =
    case model.livePriceWebData of
        RemoteData.Success _ ->
            div []
                [ h3 [] [ text "Sell stocks" ]
                , input [ type_ "text", placeholder "Symbol", onInput Input_Selling_Symbol ] []
                , input [ type_ "text", placeholder "Quantity", onInput Input_Selling_Quantity ] []
                , button
                    [ if Portfolio.validSellStockQuery p model.input_Selling_Symbol model.input_Selling_Quantity then
                        onClick SellStock
                      else
                        disabled True
                    ]
                    [ text "Sell" ]
                ]

        _ ->
            div [] []


buyView : Model -> Html Msg
buyView model =
    case model.livePriceWebData of
        RemoteData.Success _ ->
            let
                headingsRow =
                    tr []
                        [ th []
                            [ text "Description" ]
                        , th []
                            [ text "Exchange" ]
                        , th []
                            [ text "Symbol" ]
                        , th []
                            [ text "Price" ]
                        ]

                stockRows =
                    model.livePrice.exchange
                        |> Dict.toList
                        |> List.map
                            (\( exchange, stocksDict ) ->
                                stocksDict
                                    |> Dict.toList
                                    |> List.map
                                        (\( symbol, stock ) ->
                                            { description = stock.company
                                            , exchange = exchange
                                            , symbol = stock.symbol
                                            , price = stock.price
                                            }
                                        )
                                    |> List.sortBy .symbol
                            )
                        |> List.concat
                        |> List.map
                            (\stockItem ->
                                tr []
                                    [ td []
                                        [ text stockItem.description ]
                                    , td []
                                        [ text stockItem.exchange ]
                                    , td []
                                        [ text stockItem.symbol ]
                                    , td []
                                        [ text stockItem.price ]
                                    ]
                            )
            in
                div []
                    [ h3 [] [ text "Buy stocks" ]
                    , table [ attribute "border" "1" ] (headingsRow :: stockRows)
                    ]

        _ ->
            div [] []
