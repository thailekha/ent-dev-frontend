module Views exposing (view)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import RemoteData exposing (WebData)
import Components.Portfolio as Portfolio
import Components.Auth as Auth
import Components.LiveData as LiveData
import State exposing (..)
import ViewComponents exposing (..)
import Array


view : Model -> Html Msg
view model =
    let
        elmInfo =
            if String.contains "localhost" model.elmBackendUrl then
                "Local Elm"
            else
                "Remote Elm"

        nodeInfo =
            if String.contains "localhost" model.nodeBackendUrl then
                "Local Node.js"
            else
                "Remote Node.js"
    in
        div []
            [ p [] [ text <| "(" ++ elmInfo ++ "/" ++ nodeInfo ++ ")" ]
            , div [ class "container", style [ ( "margin-top", "30px" ), ( "text-align", "center" ) ] ]
                [ case model.auth.authenticationState of
                    Auth.LoggedOut ->
                        div []
                            [ loginForm model
                            ]

                    Auth.LoggedIn creds ->
                        loggedinView model
                ]
            ]


loggedinView : Model -> Html Msg
loggedinView model =
    div []
        [ button [ onClick Logout ] [ text "Logout" ]
        , h1 [] [ text "Investments Portfolio Application" ]
        , liveDataSelectBox
        , br [] []
        , button [ onClick GetPortfolio ] [ text "Fetch portfolio" ]
        , button [ onClick GetLivePrice ] [ text "Call Price Scraper" ]
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

        summary =
            getTotal model portfolio.holdings
    in
        div []
            [ h3 (gainLossCss net) [ text ("Net Profit/Loss: " ++ (toString net)) ]
            , h4 [] [ text <| "Purchase Price Total: " ++ (toString summary.purchasePrice) ++ "  |  " ++ "Gross Present Value: " ++ (toString summary.gpv) ]
            , h4 [] [ text <| "Total Sell Costs: " ++ (toString summary.sellCost) ++ "  |  " ++ "GPV after Sell Costs: " ++ (toString summary.gpvAfterSell) ]
            , h4 (gainLossCss summary.net)
                [ text <|
                    (if summary.net < 0 then
                        "Gross Loss: "
                     else
                        "Gross Profit: "
                    )
                        ++ (toString summary.net)
                ]
            , table [ attribute "border" "1" ]
                (List.concat
                    [ [ tableHeadingsRow
                      , cashHoldingRow model
                      ]
                    , List.concat <| List.map (holdingView True model) <| Portfolio.sortHoldings <| portfolio.holdings
                    ]
                )
            , hr [] []
            , h3 [] [ text "Stock solds" ]
            , table [ attribute "border" "1" ]
                (List.concat
                    [ [ tableHeadingsRow
                      ]
                    , List.concat <| List.map (holdingView False model) <| Portfolio.sortHoldings <| portfolio.stocksSold
                    ]
                )
            ]


sellView : Model -> Portfolio.Portfolio -> Html Msg
sellView model p =
    case model.livePriceWebData of
        RemoteData.Success _ ->
            div []
                [ h3 [] [ text "Sell stocks" ]
                , button [ onClick SellAll ] [ text "Liquidate all" ]
                , br [] []
                , br [] []
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

                stocks =
                    model.livePrice
                        |> LiveData.stocks

                stockRows =
                    stocks
                        |> List.filter (\s -> String.startsWith model.input_Buying_Symbol s.symbol)
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
                                        [ text <| toString <| stockItem.price ]
                                    ]
                            )

                ( cost, canBuy ) =
                    case
                        (stocks
                            |> List.filter (\s -> s.symbol == model.input_Buying_Symbol)
                            |> Array.fromList
                            |> Array.get 0
                        )
                    of
                        Just s ->
                            case String.toFloat model.input_Buying_Quantity of
                                Ok q ->
                                    ( s.price * q, True )

                                Err _ ->
                                    ( -1, False )

                        Nothing ->
                            ( -1, False )
            in
                div []
                    [ h3 [] [ text "Buy stocks" ]
                    , input [ type_ "text", placeholder "Symbol", onInput Input_Buying_Symbol ] []
                    , input [ type_ "text", placeholder "Quantity", onInput Input_Buying_Quantity ] []
                    , if canBuy then
                        div []
                            [ text <| "Cost: " ++ (toString cost)
                            , br [] []
                            , br [] []
                            , button [ onClick BuyStock ] [ text "Buy" ]
                            ]
                      else
                        div [] []
                    , table [ attribute "border" "1" ] (headingsRow :: stockRows)
                    ]

        _ ->
            div [] []
