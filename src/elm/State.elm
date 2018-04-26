port module State exposing (..)

import Http
import RemoteData exposing (WebData)
import Components.Portfolio as Portfolio
import Components.LiveData as LiveData
import Components.Auth as Auth
import Json.Decode as Decode exposing (Value, value)
import Json.Encode as Encode
import Date
import Task


type alias Model =
    { auth : Auth.Model
    , input_Login_Email : String
    , input_Login_Password : String
    , user : WebData Portfolio.User
    , livePriceWebData : WebData Value
    , livePrice : LiveData.Data
    , liveDataUrl : String
    , input_Selling_Symbol : String
    , input_Selling_Quantity : String
    , input_Buying_Symbol : String
    , input_Buying_Quantity : String
    }


init : Maybe Value -> ( Model, Cmd Msg )
init config =
    { auth = Auth.init config
    , input_Login_Email = ""
    , input_Login_Password = ""
    , user = RemoteData.NotAsked
    , livePriceWebData = RemoteData.NotAsked
    , livePrice = LiveData.init
    , liveDataUrl = "/test?n=0"
    , input_Selling_Symbol = ""
    , input_Selling_Quantity = ""
    , input_Buying_Symbol = ""
    , input_Buying_Quantity = ""
    }
        ! []


type Msg
    = Input_Login_Email String
    | Input_Login_Password String
    | Login
    | OnResponseLogin (WebData Auth.Credentials)
    | Logout
    | GetPortfolio
    | OnResponsePortfolio (WebData Portfolio.User)
    | GetLivePrice
    | OnResponseLivePrice (WebData Value)
    | SetLiveDataUrl String
    | Input_Selling_Symbol String
    | Input_Selling_Quantity String
    | SellStock
    | SellAll
    | RemoveShare Int String
    | Input_Buying_Symbol String
    | Input_Buying_Quantity String
    | BuyStock
    | ImportHolding Date.Date
    | ResetPortfolio
    | NoChange String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input_Login_Email email ->
            { model | input_Login_Email = email } ! []

        Input_Login_Password password ->
            { model | input_Login_Password = password } ! []

        Login ->
            model ! [ requestLogin model ]

        OnResponseLogin webdata ->
            { model | auth = Auth.updateCredentialsWebdata model.auth webdata }
                ! (case webdata of
                    RemoteData.Success creds ->
                        [ saveCreds creds ]

                    _ ->
                        []
                  )

        Logout ->
            { model
                | auth = Auth.init Nothing
                , input_Login_Email = ""
                , input_Login_Password = ""
            }
                ! [ logout () ]

        GetPortfolio ->
            ( { model | user = RemoteData.Loading }, getPortfolio model )

        OnResponsePortfolio res ->
            { model | user = res } ! []

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

        Input_Selling_Symbol str ->
            { model | input_Selling_Symbol = str } ! []

        Input_Selling_Quantity str ->
            { model | input_Selling_Quantity = str } ! []

        SellStock ->
            { model
                | user =
                    RemoteData.map
                        (\u ->
                            { u
                                | portfolio = Portfolio.sellStock u.portfolio model.livePrice model.input_Selling_Symbol model.input_Selling_Quantity
                            }
                        )
                        model.user
            }
                ! []

        SellAll ->
            { model
                | user =
                    RemoteData.map
                        (\u ->
                            { u
                                | portfolio = Portfolio.sellAll u.portfolio
                            }
                        )
                        model.user
            }
                ! []

        RemoveShare index symbol ->
            { model
                | user =
                    RemoteData.map
                        (\u ->
                            { u
                                | portfolio = Portfolio.removeShare u.portfolio symbol index
                            }
                        )
                        model.user
            }
                ! []

        Input_Buying_Symbol str ->
            { model | input_Buying_Symbol = str } ! []

        Input_Buying_Quantity str ->
            { model | input_Buying_Quantity = str } ! []

        BuyStock ->
            model ! [ Task.perform ImportHolding Date.now ]

        ImportHolding date ->
            { model
                | user =
                    RemoteData.map
                        (\u ->
                            { u
                                | portfolio = Portfolio.buystock u.portfolio (LiveData.stocks model.livePrice) model.input_Buying_Symbol model.input_Buying_Quantity date
                            }
                        )
                        model.user
            }
                ! []

        ResetPortfolio ->
            model ! [ resetPortfolio model ]

        NoChange _ ->
            model ! []


port saveCreds : Auth.Credentials -> Cmd msg


port logout : () -> Cmd msg


requestLogin : Model -> Cmd Msg
requestLogin model =
    let
        body =
            Encode.object [ ( "email", Encode.string model.input_Login_Email ), ( "password", Encode.string model.input_Login_Password ) ]
    in
        Http.request
            { method = "POST"
            , headers =
                [ Http.header "Access-Control-Allow-Origin" "*"
                ]
            , url = "http://localhost:4040/api/auth/login"
            , body = Http.jsonBody body
            , expect = Http.expectJson Auth.decodeCredentials
            , timeout = Nothing
            , withCredentials = False
            }
            |> RemoteData.sendRequest
            |> Cmd.map OnResponseLogin


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


getPortfolio : Model -> Cmd Msg
getPortfolio model =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"
            , Http.header "Authorization" ("Bearer " ++ (Auth.tryGetToken model.auth))

            --, Http.header "x-access-token" "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpYXQiOjE1MjM4NjgxNDAsImV4cCI6MTUyMzg3NTM0MH0.F7zuxQJ1KPF9_fpXm1kTpFRiuOJcA3U5BXfNY1KB02Q"
            ]

        --, url = "https://pawelpaszki-ent-dev.herokuapp.com/api/users/5a7f2f5bce6979001451b00d"
        , url = "http://localhost:4040/api/users/" ++ (Auth.tryGetId model.auth)
        , body = Http.emptyBody
        , expect = Http.expectJson Portfolio.decodeUser
        , timeout = Nothing
        , withCredentials = False
        }
        |> RemoteData.sendRequest
        |> Cmd.map OnResponsePortfolio


resetPortfolio : Model -> Cmd Msg
resetPortfolio model =
    case model.user of
        RemoteData.Success user ->
            Http.request
                { method = "PUT"
                , headers =
                    [ Http.header "Access-Control-Allow-Origin" "*"
                    , Http.header "Authorization" ("Bearer " ++ (Auth.tryGetToken model.auth))

                    --, Http.header "x-access-token" "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpYXQiOjE1MjM4NjgxNDAsImV4cCI6MTUyMzg3NTM0MH0.F7zuxQJ1KPF9_fpXm1kTpFRiuOJcA3U5BXfNY1KB02Q"
                    ]

                --, url = "https://pawelpaszki-ent-dev.herokuapp.com/api/users/5a7f2f5bce6979001451b00d"
                , url = "http://localhost:4040/api/users/reset/" ++ (Auth.tryGetId model.auth)
                , body = Http.jsonBody <| Portfolio.encodeUser user
                , expect = Http.expectJson Portfolio.decodeUser
                , timeout = Nothing
                , withCredentials = False
                }
                |> RemoteData.sendRequest
                |> Cmd.map OnResponsePortfolio

        _ ->
            Cmd.none


updatePortfolio : Portfolio.User -> Cmd Msg
updatePortfolio user =
    Http.request
        { method = "PUT"
        , headers =
            [ Http.header "Access-Control-Allow-Origin" "*"
            ]
        , url = "http://localhost:4040/api/users/5add52c2090d910f9f20d685"
        , body = Http.jsonBody <| Portfolio.encodeUser user
        , expect = Http.expectJson Portfolio.decodeUser
        , timeout = Nothing
        , withCredentials = False
        }
        |> RemoteData.sendRequest
        |> Cmd.map OnResponsePortfolio



--need timestamp --> need the update function --> do it here
