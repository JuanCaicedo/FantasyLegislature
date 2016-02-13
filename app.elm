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
  = PopulateAvailableLegislators (Result Http.Error (List Legislator))
  | ToggleLegislator Destination Legislator


type Destination
  = Available
  | Selected

type alias Model =
  { selectedLegislators: List Legislator
  , availableLegislators: List Legislator
  }


view : Address Action -> Model -> Html
view address model =
  div
    [ class "container-fluid" ]
    [ div [ class "col-xs-6" ] [ h1 [] [ text "Your team" ]
                               , legislatorsTable address Available model.selectedLegislators ]
    , div [ class "col-xs-6" ] [ h1 [] [ text "Available" ]
                               , legislatorsTable address Selected model.availableLegislators]
    ]


legislatorsTable : Signal.Address Action -> Destination -> (List Legislator) -> Html
legislatorsTable address destination legislators =
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
            ( legislatorsHeader :: (List.map (legislatorListItem address destination) legislators) )
        ]
    ]


legislatorsHeader : Html
legislatorsHeader =
  tr
    []
    [ th [] [ text "First Name" ]
    , th [] [ text "Last Name" ]
    ]


legislatorListItem : Signal.Address Action -> Destination -> Legislator -> Html
legislatorListItem address destination legislator =
  tr
    [ onClick address (ToggleLegislator destination legislator)]
    [ td [] [ text legislator.firstName ]
    , td [] [ text legislator.lastName ]
    ]


update : Action -> Model -> ( Model, Effects Action)
update msg model =
  case msg of
    PopulateAvailableLegislators result ->
      case result of
        Ok legislators ->
          ({ model | availableLegislators = legislators }
          , Effects.none)
        Err error ->
          ({ model | availableLegislators = [] }
          , Effects.none)
    ToggleLegislator destination legislator ->
      let
        legislatorFilter currentLegislator =
          not (currentLegislator.firstName == legislator.firstName ) && not ( currentLegislator.lastName == legislator.lastName )
      in
        case destination of
         Available ->
           ({ model
              | availableLegislators = legislator :: model.availableLegislators
              , selectedLegislators = (List.filter legislatorFilter model.selectedLegislators)}
           , Effects.none)
         Selected ->
           ({ model
              | selectedLegislators = legislator :: model.selectedLegislators
              , availableLegislators = (List.filter legislatorFilter model.availableLegislators)}
           , Effects.none)


initialModel : Model
initialModel =
  { selectedLegislators =
      [
       { firstName = "Juan"
       , lastName = "Caicedo"
       }
      ,{ firstName = "Carson"
       , lastName = "Banov"
       }
      ]
  , availableLegislators = []
  }


getLegislators :  Effects Action
getLegislators =
  Http.get legislatorsDecoder legislatorsUrl
      |> Task.toResult
      |> Task.map PopulateAvailableLegislators
      |> Effects.task


type alias Legislator = { firstName: String, lastName: String }
legislatorsDecoder : Json.Decoder (List Legislator)
legislatorsDecoder =
  let legislator =
    Json.object2 Legislator
          ("first_name" := Json.string)
          ("last_name" := Json.string)
  in
    "results" := Json.list legislator


legislatorsUrl : String
legislatorsUrl =
  Http.url "https://congress.api.sunlightfoundation.com/legislators"
      [ ("apikey", "d6ef0d61cbd241bc9d89109e4f70e128")
      , ("per_page", "all") ]


app: StartApp.App Model
app =
  StartApp.start
    { init = ( initialModel, getLegislators )
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
