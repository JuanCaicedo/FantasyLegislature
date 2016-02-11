module FantasySenate where

import Html exposing (..)
import StartApp
import Effects exposing (Effects)
import Signal exposing (Address)

type alias Action = {}


type alias Model = {}


view : Address action -> Model -> Html
view action model =
  h1 [] [ text "test" ]


update : Action -> Model -> ( Model, Effects Action )
update action model = ( model, Effects.none )

initialModel : Model
initialModel = {}

app =
  StartApp.start
    { init = ( initialModel, Effects.none )
    , update = update
    , view = view
    , inputs = []
    }

main : Signal Html
main =
  app.html
