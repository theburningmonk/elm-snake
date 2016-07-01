import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Keyboard exposing (..)
import Time exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Text exposing (..)
import Char exposing (..)

segmentDim = 15
cherryRadius = 7.5
(width, height) = (500, 500)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- step 1: define your Model
type alias Position = (Float, Float)
pos : Float -> Float -> Position
pos = (,)

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

initSnake : Snake 
initSnake =
  { head = (0, 0)
  , tail = [1..8] |> List.map (\n -> (-n * segmentDim, 0))
  , direction = Right
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
view model =
  let bg = rect (toFloat width) (toFloat height) |> filled black
      content =
        case model of 
          NotStarted -> 
            [txt "press SPACE to start"]

          Started snake ->
            let head = rect segmentDim segmentDim |> filled white |> move snake.head 
                tail = 
                  snake.tail
                  |> List.map (\p -> 
                    rect segmentDim segmentDim
                    |> filled Color.yellow
                    |> move p)
             in head::tail

  in collage width height (bg::content)
     |> Element.toHtml

-- step 6: implement state transition
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    NotStarted ->
      case msg of
        KeyPress 32 ->
          (Started initSnake, Cmd.none)

        _ ->
          (model, Cmd.none)

    Started snake -> 
      case msg of
        KeyPress keyCode ->
          let newDir = getNewDirection keyCode snake.direction
              newSnake = { snake | direction = newDir }
          in (Started newSnake, Cmd.none)

        Tick _ -> 
          let newHead = getNewSegment snake.head snake.direction
              oldBody = (snake.head::snake.tail)
              newTail = List.take (List.length oldBody-1) oldBody
              newSnake = { snake | head=newHead, tail=newTail }
          in (Started newSnake, Cmd.none)

txt : String -> Form
txt msg =
  msg
  |> Text.fromString
  |> Text.color white
  |> Text.monospace
  |> Element.centered
  |> Collage.toForm

getNewDirection : Char.KeyCode -> Direction -> Direction
getNewDirection keyCode currentDir =
  let (changeableDirs, newDir) =
    case Char.fromCode keyCode of
      'a' -> ([ Up, Down ], Left)
      'w' -> ([ Left, Right ], Up)
      'd' -> ([ Up, Down ], Right)
      's' -> ([ Left, Right ], Down)
      _  -> ([], currentDir)
  in if List.any ((==) currentDir) changeableDirs then newDir else currentDir

getNewSegment : Position -> Direction -> Position
getNewSegment (x, y) direction =
  case direction of
    Up    -> pos x (y+segmentDim)
    Down  -> pos x (y-segmentDim)
    Left  -> pos (x-segmentDim) y
    Right -> pos (x+segmentDim) y