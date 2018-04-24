module Components.Portfolio exposing (..)

import Json.Decode exposing (..)
import Dict exposing (Dict)
import Components.LiveData as LiveData
import Date
import Debug
import Array
import Json.Encode as Encode


type alias User =
    { portfolio : Portfolio
    , cashHolding : Maybe Float
    }


type alias Portfolio =
    { holdings : List Holding
    , stocksSold : List Holding
    }


type alias Holding =
    { shares : List Share
    , symbol : String
    , displayName : String
    , exchange : String
    }


type alias Share =
    { dateIn : Maybe String
    , dateOut : Maybe String
    , quantity : Int
    , purchasePrice : Float
    }


encodeShare : Share -> Value
encodeShare share =
    Encode.object
        [ ( "dateIn"
          , case share.dateIn of
                Just d ->
                    Encode.string d

                Nothing ->
                    Encode.null
          )
        , ( "dateOut"
          , case share.dateOut of
                Just d ->
                    Encode.string d

                Nothing ->
                    Encode.null
          )
        , ( "quantity", Encode.int share.quantity )
        , ( "purchasePrice", Encode.float share.purchasePrice )
        ]


encodeHolding : Holding -> Value
encodeHolding holding =
    Encode.object
        [ ( "shares", Encode.list <| List.map encodeShare holding.shares )
        , ( "symbol", Encode.string holding.symbol )
        , ( "displayName", Encode.string holding.displayName )
        , ( "exchange", Encode.string holding.exchange )
        ]


encodePortfolio : Portfolio -> Value
encodePortfolio portfolio =
    Encode.object
        [ ( "holdings", Encode.list <| List.map encodeHolding portfolio.holdings )
        , ( "stocksSold", Encode.list <| List.map encodeHolding portfolio.stocksSold )
        ]


encodeUser : User -> Value
encodeUser user =
    Encode.object
        [ ( "portfolio", encodePortfolio user.portfolio )
        , ( "cashHolding"
          , case user.cashHolding of
                Just cash ->
                    Encode.float cash

                Nothing ->
                    Encode.null
          )
        ]


type alias Total =
    { purchasePrice : Float
    , sellCost : Float
    , gpv : Float
    , gpvAfterSell : Float
    , net : Float
    }



-- for used after livedata has been loaded


type alias FullShare =
    { displayName : String
    , exchange : String
    , symbol : String
    , dateIn : Maybe String
    , dateOut : Maybe String
    , quantity : Int
    , cost : Float
    , purchasePrice : Float
    , price : Float
    , value : Float
    , detailGainOrLoss : Float
    , percentageGainOrLoss : Float
    , sellingCost : Float
    }


flatten : Maybe (Maybe x) -> Maybe x
flatten nested =
    case nested of
        Just x_ ->
            x_

        Nothing ->
            Nothing


sortHoldings : List Holding -> List Holding
sortHoldings holdings =
    holdings
        |> List.sortBy .symbol
        |> List.map (\h -> { h | shares = List.sortBy sortShare h.shares })


getFullShare : Holding -> Share -> LiveData.Data -> Result String FullShare
getFullShare holding share livedata =
    case
        (Dict.get holding.exchange livedata.exchange
            |> Maybe.map (\exchange -> Dict.get holding.symbol exchange)
            |> flatten
        )
    of
        Just data ->
            case String.toFloat data.price of
                Ok price ->
                    let
                        ( value, cost ) =
                            ( (toFloat share.quantity) * price, (toFloat share.quantity) * share.purchasePrice )
                    in
                        let
                            sellingCost =
                                if (value < 25000) then
                                    if (value * 0.01 + 1.25 >= 25) then
                                        value * 0.01 + 1.25
                                    else
                                        25 + 1.25
                                else if (25000 * 0.01 + 0.005 * (value - 25000) + 1.25 >= 25) then
                                    25000 * 0.01 + 0.005 * (value - 25000) + 1.25
                                else
                                    25 + 1.25
                        in
                            Ok
                                ({ displayName = holding.displayName
                                 , exchange = holding.exchange
                                 , symbol = holding.symbol
                                 , dateIn = share.dateIn
                                 , dateOut = share.dateOut
                                 , quantity = share.quantity
                                 , cost = cost
                                 , purchasePrice = share.purchasePrice
                                 , price = price
                                 , value = value
                                 , detailGainOrLoss = value - cost - sellingCost
                                 , percentageGainOrLoss = ((value - cost - sellingCost) / cost) * 100
                                 , sellingCost = sellingCost
                                 }
                                )

                Err err ->
                    Err err

        Nothing ->
            Err ("Cannot find " ++ holding.exchange ++ "/" ++ holding.symbol ++ " in live data")


tryGetCashHolding : User -> Float
tryGetCashHolding user =
    case user.cashHolding of
        Just cash ->
            cash

        Nothing ->
            0


totalQuantityOfHolding : Holding -> Int
totalQuantityOfHolding holding =
    holding.shares
        |> List.map (\s -> s.quantity)
        |> List.foldl (+) 0


validSellStockQuery : Portfolio -> String -> String -> Bool
validSellStockQuery portfolio symbol qty =
    case String.toInt qty of
        Ok quantity ->
            let
                stocks =
                    portfolio.holdings
                        |> List.filter (\h -> h.symbol == symbol)

                totalQuantityOfStock =
                    stocks
                        |> List.map totalQuantityOfHolding
                        |> List.foldl (+) 0
            in
                totalQuantityOfStock > 0 && totalQuantityOfStock >= quantity

        Err _ ->
            False


sortShare : Share -> Int
sortShare s =
    case s.dateIn of
        Just date_in ->
            case Date.fromString date_in of
                Ok date ->
                    let
                        monthNo =
                            case (Date.month date) of
                                Date.Jan ->
                                    1

                                Date.Feb ->
                                    2

                                Date.Mar ->
                                    3

                                Date.Apr ->
                                    4

                                Date.May ->
                                    5

                                Date.Jun ->
                                    6

                                Date.Jul ->
                                    7

                                Date.Aug ->
                                    8

                                Date.Sep ->
                                    9

                                Date.Oct ->
                                    10

                                Date.Nov ->
                                    11

                                Date.Dec ->
                                    12
                    in
                        (Date.day date) + (monthNo * 30) + ((Date.year date) * 365)

                Err _ ->
                    -1

        Nothing ->
            -1


sellStock : Portfolio -> String -> String -> Portfolio
sellStock portfolio symbol qty =
    case String.toInt qty of
        Ok quantity ->
            let
                maybeHolding =
                    portfolio.holdings
                        |> List.filter (\h -> h.symbol == symbol)
                        |> Array.fromList
                        |> Array.get 0
            in
                case maybeHolding of
                    Nothing ->
                        portfolio

                    Just holding ->
                        let
                            ( sold, remain, _ ) =
                                portfolio.holdings
                                    |> List.filter (\h -> h.symbol == symbol)
                                    |> List.map (\h -> h.shares)
                                    |> List.concat
                                    |> List.sortBy sortShare
                                    |> List.foldl
                                        (\share ( toBeAddedToSold, remainInHoldings, remainQuantity ) ->
                                            if remainQuantity == 0 then
                                                ( toBeAddedToSold, share :: remainInHoldings, remainQuantity )
                                            else if share.quantity <= remainQuantity then
                                                ( share :: toBeAddedToSold, remainInHoldings, remainQuantity - share.quantity )
                                            else
                                                ( { share | quantity = remainQuantity } :: toBeAddedToSold, { share | quantity = share.quantity - remainQuantity } :: remainInHoldings, 0 )
                                        )
                                        ( [], [], quantity )
                        in
                            { portfolio
                                | holdings =
                                    if List.length remain > 0 then
                                        portfolio.holdings
                                            |> List.filter (\h -> h.symbol /= symbol)
                                            |> (::)
                                                { shares = remain
                                                , symbol = holding.symbol
                                                , displayName = holding.displayName
                                                , exchange = holding.exchange
                                                }
                                    else
                                        portfolio.holdings
                                , stocksSold =
                                    if List.length sold > 0 then
                                        portfolio.stocksSold
                                            |> (::)
                                                { shares = sold
                                                , symbol = holding.symbol
                                                , displayName = holding.displayName
                                                , exchange = holding.exchange
                                                }
                                    else
                                        portfolio.stocksSold
                            }

        Err _ ->
            portfolio


decodeUser : Decoder User
decodeUser =
    map2 User
        (field "portfolio" decodePortfolio)
        (field "cashHolding" float
            |> maybe
            |> andThen always100
        )


always100 : Maybe Float -> Decoder (Maybe Float)
always100 res =
    case res of
        _ ->
            succeed (Just 100)


decodePortfolio : Decoder Portfolio
decodePortfolio =
    map2 Portfolio
        (field "holdings" (list decodeHolding))
        (field "stocksSold" (list decodeHolding))


decodeHolding : Decoder Holding
decodeHolding =
    map4 Holding
        (field "shares" (list decodeShare))
        (field "symbol" string)
        (field "displayName" string)
        (field "exchange" string)


decodeShare : Decoder Share
decodeShare =
    map4 Share
        (field "dateIn" (nullable string))
        (field "dateOut" (nullable string))
        (field "quantity" int)
        (field "purchasePrice" float)
