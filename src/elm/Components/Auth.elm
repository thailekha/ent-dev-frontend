module Components.Auth exposing (..)

import Json.Decode exposing (..)
import Json.Encode
import RemoteData exposing (WebData)


type alias Credentials =
    { token : String
    , userId : String
    }


type AuthenticationState
    = LoggedOut
    | LoggedIn Credentials


decodeCredentials : Decoder Credentials
decodeCredentials =
    map2 Credentials
        (field "token" string)
        (field "_id" string)


type alias Model =
    { authenticationState : AuthenticationState
    , credentialsWebdata : WebData Credentials
    }


init : Maybe Value -> Model
init config =
    { authenticationState =
        case config of
            Just initialData ->
                case (decodeValue decodeCredentials initialData) of
                    Ok decodedCredentials ->
                        LoggedIn decodedCredentials

                    Err _ ->
                        Debug.log "Cannot decode auth data from config" (toString config)
                            |> always LoggedOut

            Nothing ->
                Debug.log "No init config data" ""
                    |> always LoggedOut
    , credentialsWebdata = RemoteData.NotAsked
    }


updateCredentialsWebdata : Model -> WebData Credentials -> Model
updateCredentialsWebdata model response =
    { model
        | credentialsWebdata = response
        , authenticationState =
            (case response of
                RemoteData.Success creds ->
                    LoggedIn creds

                _ ->
                    model.authenticationState
            )
    }


tryGetToken : Model -> String
tryGetToken model =
    case model.authenticationState of
        LoggedIn creds ->
            creds.token

        LoggedOut ->
            ""
