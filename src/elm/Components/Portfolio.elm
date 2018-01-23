module Components.Portfolio exposing (..)

import Json.Decode exposing (..)
import Dict exposing (Dict)
import Components.LiveData as LiveData
import Debug


type alias User =
    { user : Portfolio
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
    { dateIn : String
    , dateOut : Maybe String
    , quantity : Int
    , purchasePrice : Float
    }


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
    , dateIn : String
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


decodeUser : Decoder User
decodeUser =
    map2 User
        (field "user" decodePortfolio)
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
        (field "dateIn" string)
        (field "dateOut" (nullable string))
        (field "quantity" int)
        (field "purchasePrice" float)
