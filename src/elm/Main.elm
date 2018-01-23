module Main exposing (..)

import Html exposing (program)
import Views
import State exposing (Msg, Model)


main : Program Never Model Msg
main =
    program
        { init = State.init
        , view = Views.view
        , update = State.update
        , subscriptions = (\_ -> Sub.none)
        }
