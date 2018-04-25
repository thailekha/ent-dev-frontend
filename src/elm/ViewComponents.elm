module ViewComponents exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Debug
import Round
import Time.Date as Date
import RemoteData exposing (WebData)
import Components.Portfolio as Portfolio
import Components.LiveData as LiveData
import State exposing (..)
import Json.Decode as Decode


loginForm : Model -> Html Msg
loginForm model =
    div []
        [ case model.auth.credentialsWebdata of
            RemoteData.NotAsked ->
                text "Please login or signup"

            RemoteData.Loading ->
                text "Loading..."

            RemoteData.Success p ->
                text ""

            RemoteData.Failure error ->
                text (toString error)
        , br [] []
        , input [ type_ "text", placeholder "email", name "email", onInput Input_Login_Email ] []
        , br [] []
        , input [ type_ "password", placeholder "password", name "password", onInput Input_Login_Password ] []
        , br [] []
        , button [ onClick Login ] [ text "Login" ]
        , button [ onClick Login ] [ text "Signup" ]
        ]


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
                [ [ td [] [] ]
                , [ td []
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


holdingView : Model -> Portfolio.Holding -> List (Html Msg)
holdingView model holding =
    let
        rows =
            holding.shares
                |> List.indexedMap
                    (\index share ->
                        case model.livePriceWebData of
                            RemoteData.Success _ ->
                                case (Portfolio.getFullShare holding share model.livePrice) of
                                    Ok fullShare ->
                                        fullShareView index fullShare

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
            [ (case share.dateIn of
                Just date_in ->
                    text (dateString date_in)

                Nothing ->
                    text ""
              )
            ]
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


fullShareView : Int -> Portfolio.FullShare -> Html Msg
fullShareView index share =
    tr []
        [ td []
            [ button [ onClick (RemoveShare index share.symbol) ] [ text "X" ] ]
        , td []
            [ text share.displayName ]
        , td []
            [ text share.exchange ]
        , td []
            [ text share.symbol ]
        , td []
            [ (case share.dateIn of
                Just date_in ->
                    text (dateString date_in)

                Nothing ->
                    text ""
              )
            ]
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
    case model.user of
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


buyTableHeadingsRow : Html msg
buyTableHeadingsRow =
    tr []
        (("" :: tableHeadings)
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
            Debug.log "parsed" iso
                |> always (toString (Date.day date) ++ "/" ++ toString (Date.month date) ++ "/" ++ toString (Date.year date))

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
