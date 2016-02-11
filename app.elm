module FantasySenate where

import Html exposing (..)
import StartApp
import Effects

view action model =
  h1 [] [ text "test" ]

update action model = ( model, Effects.none )

initialModel = {}

app =
  StartApp.start
    { init = ( initialModel, Effects.none )
    , update = update
    , view = view
    , inputs = []
    }

main =
  app.html
