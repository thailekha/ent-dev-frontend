module Components.LiveData exposing (..)

import Json.Decode exposing (..)
import Dict exposing (Dict)
import Debug


type alias Data =
    { exchange : Dict String (Dict String Item)
    }


type alias Item =
    { price : String
    , pChg : String
    , company : String
    , symbol : String
    , change : String
    }



-- used for buying


type alias Stock =
    { description : String, exchange : String, price : Float, symbol : String }


init : Data
init =
    { exchange = Dict.empty
    }


exchangeIds : List ( String, String )
exchangeIds =
    [ ( "ise", "ise" ), ( "ftse", "ftse350" ), ( "coinranking", "coinranking" ) ]


decodeData : Value -> Data
decodeData res =
    { exchange =
        exchangeIds
            |> List.map (\( dictKey, jsonKey ) -> ( dictKey, decodeItems jsonKey res ))
            |> Dict.fromList
    }


decodeItems : String -> Value -> Dict String Item
decodeItems fieldName res =
    case decodeValue (maybe (field fieldName value)) res of
        Ok maybeExchange ->
            case maybeExchange of
                Just exchangeValue ->
                    case decodeValue (field "data" (list decodeItem)) exchangeValue of
                        Ok items ->
                            Dict.fromList <| List.map (\i -> ( i.symbol, i )) items

                        Err err ->
                            Debug.log ("Error decoding " ++ fieldName) (toString err)
                                |> always Dict.empty

                Nothing ->
                    Debug.log ("Warning decoding " ++ fieldName) (" Nothing\n" ++ toString res)
                        |> always Dict.empty

        Err err ->
            Debug.log ("Error decoding " ++ fieldName) (toString err)
                |> always Dict.empty


decodeItem : Decoder Item
decodeItem =
    map5 Item
        (field "price" string)
        (field "pChg" string)
        (field "company" string)
        (field "symbol" string)
        (field "change" string)


stocks : Data -> List Stock
stocks data =
    data.exchange
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
                            , price =
                                case String.toFloat stock.price of
                                    Ok pr ->
                                        pr

                                    Err _ ->
                                        0
                            }
                        )
                    |> List.sortBy .symbol
            )
        |> List.concat
