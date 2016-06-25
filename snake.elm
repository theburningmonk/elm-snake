import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- step 1: define your Model
type Model = NotStarted

-- step 2: define Msg that can trigger updates to Model
type Msg = Tick Time

-- step 3: define the initial state for the app
init : (Model, Cmd Msg)
init = (NotStarted, Cmd.none)

-- step 4: define your subscriptions - WebSocket, Keyboard, etc.
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- step 5: how to render your model
view : Model -> Html Msg
view model = div [] []

-- step 6: implement state transition
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (NotStarted, Cmd.none)