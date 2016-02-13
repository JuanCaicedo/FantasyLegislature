module FantasySenate where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import StartApp
import Json.Decode as Json exposing ((:=))
import Effects exposing (Effects, Never)
import Signal exposing (Address)
import List
import Task exposing (Task)


type Action
  = PopulateAvailableSenators (Result Http.Error (List Senator))
  | ToggleSenator Destination (Senator)


type Destination
  = Available
  | Selected

type alias Model =
  { selectedSenators: List Senator
  , availableSenators: List Senator
  }


view : Address Action -> Model -> Html
view address model =
  div
    [ class "container-fluid" ]
    [ div [ class "col-xs-6" ] [ h1 [] [ text "Your team" ]
                               , senatorsTable address Available model.selectedSenators ]
    , div [ class "col-xs-6" ] [ h1 [] [ text "Available" ]
                               , senatorsTable address Selected model.availableSenators]
    ]


senatorsTable : Signal.Address Action -> Destination -> (List Senator) -> Html
senatorsTable address destination senators =
  div
    []
    [ table
        [ classList
            [ ("table", True)
            , ("table-striped", True)
            , ("table-bordered", True)
            ]
        ]
        [ tbody
            []
            ( senatorsHeader :: (List.map (senatorListItem address destination) senators) )
        ]
    ]


senatorsHeader : Html
senatorsHeader =
  tr
    []
    [ th [] [ text "First Name" ]
    , th [] [ text "Last Name" ]
    ]


senatorListItem : Signal.Address Action -> Destination -> Senator -> Html
senatorListItem address destination senator =
  tr
    [ onClick address (ToggleSenator (destination) (senator))]
    [ td [] [ text senator.firstName ]
    , td [] [ text senator.lastName ]
    ]


update : Action -> Model -> ( Model, Effects Action)
update msg model =
  case msg of
    PopulateAvailableSenators result ->
      case result of
        Ok senators ->
          ({ model | availableSenators = senators }
          , Effects.none)
        Err error ->
          ({ model | availableSenators = [] }
          , Effects.none)
    ToggleSenator destination senator ->
      let
        senatorFilter currentSenator =
          not (currentSenator.firstName == senator.firstName ) && not ( currentSenator.lastName == senator.lastName )
      in
        case destination of
         Available ->
           ({ model
              | availableSenators = senator :: model.availableSenators
              , selectedSenators = (List.filter senatorFilter model.selectedSenators)}
           , Effects.none)
         Selected ->
           ({ model
              | selectedSenators = senator :: model.selectedSenators
              , availableSenators = (List.filter senatorFilter model.availableSenators)}
           , Effects.none)


initialModel : Model
initialModel =
  { selectedSenators =
      [
       { firstName = "Juan"
       , lastName = "Caicedo"
       }
      ,{ firstName = "Carson"
       , lastName = "Banov"
       }
      ]
  , availableSenators = []
  }


getSenators :  Effects Action
getSenators =
  Http.get senatorsDecoder senatorsUrl
      |> Task.toResult
      |> Task.map PopulateAvailableSenators
      |> Effects.task


type alias Senator = { firstName: String, lastName: String }
senatorsDecoder : Json.Decoder (List Senator)
senatorsDecoder =
  let senator =
    Json.object2 Senator
          ("first_name" := Json.string)
          ("last_name" := Json.string)
  in
    "results" := Json.list senator


senatorsUrl : String
senatorsUrl =
  Http.url "https://congress.api.sunlightfoundation.com/legislators"
      [ ("apikey", "d6ef0d61cbd241bc9d89109e4f70e128")
      , ("per_page", "all") ]


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


port runner : Signal (Task Never ())
port runner =
  app.tasks
