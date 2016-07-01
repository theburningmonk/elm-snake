import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Keyboard exposing (..)
import Time exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- step 1: define your Model
type alias Position = (Float, Float)
type Direction 
  = Up
  | Down
  | Left
  | Right

type alias Snake = 
  { head: Position
  , tail: List Position
  , direction: Direction 
  }

type Model 
  = NotStarted
  | Started Snake

-- step 2: define Msg that can trigger updates to Model
type Msg 
  = KeyPress Keyboard.KeyCode
  | Tick Time

-- step 3: define the initial state for the app
init : (Model, Cmd Msg)
init = (NotStarted, Cmd.none)

-- step 4: define your subscriptions - WebSocket, Keyboard, etc.
subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    NotStarted ->
      Keyboard.presses KeyPress

    Started _ ->
      Sub.batch [
        Keyboard.presses KeyPress
      , Time.every (Time.inMilliseconds 50) Tick
      ]

-- step 5: how to render your model
view : Model -> Html Msg
view model = div [] []

-- step 6: implement state transition
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (NotStarted, Cmd.none)