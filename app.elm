module FantasySenate where

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import StartApp
import Json.Decode as Json exposing ((:=))
import Effects exposing (Effects)
import Signal exposing (Address)
import List
import Task

type Action
  = GetSenators
  | DisplaySenators (Maybe (List Senator))


type alias Model =
  { senators: List Senator }


view : Address action -> Model -> Html
view action model =
  div
    []
    [ h1 [] [ text "test" ]
    , ul [ class "senators-list" ]
         (List.map senatorListItem model.senators )
    ]


senatorListItem senator =
  li [] [ text (senator.firstName ++ senator.lastName) ]


update : Action -> Model -> ( Model, Effects Action)
update msg model =
  case msg of
    GetSenators ->
      ( model
      , getSenators)
    DisplaySenators maybe ->
      case maybe of
        Just senators ->
          ({ model | senators = senators }
          , Effects.none)
        Nothing ->
          (model
          , Effects.none)

initialModel : Model
initialModel =
  { senators = [] }

getSenators :  Effects Action
getSenators =
  Http.get senators senatorsUrl
      |> Task.toMaybe
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
    { init = ( initialModel, Effects.none )
    , update = update
    , view = view
    , inputs = []
    }

main : Signal Html
main =
  app.html
