module Views exposing (view)

import Html exposing (..)
import Html.Events exposing (onClick, on)
import Html.Attributes exposing (..)
import Debug
import Round
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
                portfolioView model p.user

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


holdingView : Model -> Portfolio.Holding -> List (Html Msg)
holdingView model holding =
    let
        rows =
            holding.shares
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

        totalRow =
            [ tr [] <|
                List.concat
                    [ List.repeat 6 (td [] [ text "" ])
                    , [ td []
                            [ holding
                                |> Portfolio.totalQuantityOfHolding
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
