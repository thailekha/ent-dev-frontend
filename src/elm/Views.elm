module Views exposing (view)

import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (..)
import RemoteData exposing (WebData)
import Components.Portfolio as Portfolio
import State exposing (..)
import ViewComponents exposing (..)


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
                div [] 
                    [ sellView model p.user
                    , portfolioView model p.user
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


sellView : Model -> Portfolio.Portfolio -> Html Msg
sellView model p =
    case model.livePriceWebData of
        RemoteData.Success _ ->
            div [] 
                [ input [ type_ "text", placeholder "Symbol", onInput Input_Selling_Symbol ] []
                , input [ type_ "text", placeholder "Quantity", onInput Input_Selling_Quantity ] []
                , button [ 
                    if Portfolio.validSellStockQuery p model.input_Selling_Symbol model.input_Selling_Quantity then
                        onClick SellStock
                    else
                        disabled True
                    ] 
                    [ text "Sell" ]
                ]

        _ ->
            div [] []
    