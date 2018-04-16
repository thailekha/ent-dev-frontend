module State exposing (..)

import Http
import RemoteData exposing (WebData)
import Components.Portfolio as Portfolio
import Components.LiveData as LiveData
import Json.Decode exposing (Value, value)


type alias Model =
    { portfolio : WebData Portfolio.User
    , livePriceWebData : WebData Value
    , livePrice : LiveData.Data
    , liveDataUrl : String
    }


init : ( Model, Cmd Msg )
init =
    { portfolio = RemoteData.NotAsked
    , livePriceWebData = RemoteData.NotAsked
    , livePrice = LiveData.init
    , liveDataUrl = "/test?n=0"
    }
        ! []


type Msg
    = GetPortfolio
    | OnResponsePortfolio (WebData Portfolio.User)
    | GetLivePrice
    | OnResponseLivePrice (WebData Value)
    | SetLiveDataUrl String
    | NoChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetPortfolio ->
            ( { model | portfolio = RemoteData.Loading }, getPortfolio )

        OnResponsePortfolio res ->
            { model | portfolio = res } ! []

        GetLivePrice ->
            ( { model | livePriceWebData = RemoteData.Loading }, getLivePrice model.liveDataUrl )

        OnResponseLivePrice response ->
            case response of
                RemoteData.Success res ->
                    { model
                        | livePriceWebData = response
                        , livePrice = LiveData.decodeData res
                    }
                        ! []

                _ ->
                    { model | livePriceWebData = response } ! []

        SetLiveDataUrl url ->
            { model | liveDataUrl = url } ! []

        NoChange _ ->
            model ! []


getLivePrice : String -> Cmd Msg
getLivePrice url =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"
            , Http.header "Access-Control-Allow-Methods" "GET"
            ]
        , url = "http://localhost:5000/proxy/scrape" ++ url
        , body = Http.emptyBody
        , expect = Http.expectJson value
        , timeout = Nothing
        , withCredentials = False
        }
        |> RemoteData.sendRequest
        |> Cmd.map OnResponseLivePrice


getPortfolio : Cmd Msg
getPortfolio =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"

            --, Http.header "x-access-token" "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpYXQiOjE1MjM4NjgxNDAsImV4cCI6MTUyMzg3NTM0MH0.F7zuxQJ1KPF9_fpXm1kTpFRiuOJcA3U5BXfNY1KB02Q"
            ]

        --, url = "https://pawelpaszki-ent-dev.herokuapp.com/api/users/5a7f2f5bce6979001451b00d"
        , url = "http://localhost:4040/api/users/5ad4959081fe7e0974b77c34"
        , body = Http.emptyBody
        , expect = Http.expectJson Portfolio.decodeUser
        , timeout = Nothing
        , withCredentials = False
        }
        |> RemoteData.sendRequest
        |> Cmd.map OnResponsePortfolio
