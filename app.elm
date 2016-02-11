module FantasySenate where

import Html exposing (..)
import Html.Attributes exposing (..)
import StartApp
import Effects exposing (Effects)
import Signal exposing (Address)
import List

type alias Action = {}


type alias Model =
  { senators: List String }


view : Address action -> Model -> Html
view action model =
  div
    []
    [ h1 [] [ text "test" ]
    , ul [ class "senators-list" ]
         (List.map senatorListItem model.senators )
    ]

senatorListItem senator =
  li [] [ text senator ]

update : Action -> Model -> ( Model, Effects Action )
update action model = ( model, Effects.none )

initialModel : Model
initialModel =
  { senators =
      [ "Ron Wyden"
      , "Jeff Merkeley"
      ]
  }

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
