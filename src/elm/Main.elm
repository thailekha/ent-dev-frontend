module Main exposing (..)

import Html exposing (programWithFlags)
import Views
import State exposing (Msg, Model)
import Json.Decode


main : Program (Maybe Json.Decode.Value) Model Msg
main =
    programWithFlags
        { init = State.init
        , view = Views.view
        , update = State.update
        , subscriptions = (\_ -> Sub.none)
        }
