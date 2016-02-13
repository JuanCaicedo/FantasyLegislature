module FantasySenate where

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import StartApp
import Json.Decode as Json exposing ((:=))
import Effects exposing (Effects, Never)
import Signal exposing (Address)
import List
import Task

type Action
  = DisplaySenators (Result Http.Error (List Senator))


type alias Model =
  { senators: List Senator }


view : Address action -> Model -> Html
view action model =
  div
    []
    [ h1 [] [ text "test" ]
    , table
        [ classList
            [ ("table", True)
            , ("table-striped", True)
            ]
        ]
        [ tbody
            []
            (List.map senatorListItem model.senators )
        ]
    ]

senatorListItem : Senator -> Html
senatorListItem senator =
  tr
    []
    [ td [] [ text senator.firstName ]
    , td [] [ text senator.lastName ]
    ]


update : Action -> Model -> ( Model, Effects Action)
update msg model =
  case msg of
    DisplaySenators result ->
      case result of
        Ok senators ->
          ({ model | senators = senators }
          , Effects.none)
        Err error ->
          ({ model | senators = [{firstName="error", lastName="error"}]}, Effects.none)

initialModel : Model
initialModel =
  { senators =
      [
       { firstName = "Juan"
       , lastName = "Caicedo"
       }
      ,{ firstName = "Carson"
       , lastName = "Banov"
       }
      ]
  }

getSenators :  Effects Action
getSenators =
  Http.get senators senatorsUrl
      |> Task.toResult
      |> Task.map DisplaySenators
      |> Effects.task


type alias Senator = { firstName: String, lastName: String }
senators : Json.Decoder (List Senator)
senators =
  let senator =
    Json.object2 Senator
          ("first_name" := Json.string)
          ("last_name" := Json.string)
  in
    "results" := Json.list senator


senatorsUrl : String
senatorsUrl =
  Http.url "https://congress.api.sunlightfoundation.com/legislators"
      [ ("apikey", "d6ef0d61cbd241bc9d89109e4f70e128") ]

app: StartApp.App Model
app =
  StartApp.start
    { init = ( initialModel, getSenators )
    , update = update
    , view = view
    , inputs = []
    }

main : Signal Html
main =
  app.html


port runner : Signal (Task.Task Never ())
port runner =
  app.tasks
