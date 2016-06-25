import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Char exposing (..)
import Random
import List
import Keyboard exposing (..)
import Text 
import Time exposing (..)
import Debug

segmentDim = 15.0
cherryRadius = 7.5
(width, height) = (600, 600)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Model
type Direction 
  = Up 
  | Down 
  | Left 
  | Right

type alias Snake = 
  { segments : List(Float, Float)
  , direction : Direction }

type alias Cherry = Maybe (Float, Float)

type Model
  = NotStarted
  | Started Snake Cherry

initSnake : Snake
initSnake = 
  { segments =
      [0.0..8.0]
      |> List.map (\n -> (n * segmentDim, 0))
      |> List.reverse
  , direction = Right }

init : (Model, Cmd Msg)
init = (NotStarted, Cmd.none)

-- Msg
type Msg
  = Tick Time
  | KeyPress Keyboard.KeyCode
  | Spawn (Float, (Float, Float))

randFloat : Random.Generator Float
randFloat = Random.float 0 1

randGenerator : Random.Generator (Float, (Float, Float))
randGenerator = Random.pair randFloat (Random.pair randFloat randFloat)

-- Update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case model of
    NotStarted ->
      case msg of
        KeyPress 32 -> (Started initSnake Nothing, Cmd.none)
        _  -> (model, Cmd.none)
    Started snake cherry ->
      case msg of
        KeyPress keyCode -> 
          let newDir = getNewDirection keyCode snake.direction
              newSnake = { snake | direction = newDir }
          in (Started newSnake cherry, Cmd.none)

        Tick _ ->
          case snake.segments of
            head::tail ->
              let newHead = getNewSegment head snake.direction
                  ateCherry =
                    case cherry of
                      Nothing -> False
                      Just pos -> isOverlap newHead pos
                  newTail = 
                    if ateCherry then
                      snake.segments
                    else 
                      List.take (List.length snake.segments-1) snake.segments
                  newSnake = { snake | segments = newHead::newTail }
                  newCherry =
                    if ateCherry then
                      Nothing
                    else 
                      cherry
                  gameOver = isGameOver newHead newTail
              in if gameOver then 
                    (NotStarted, Cmd.none)
                 else if (newCherry == Nothing) then
                    (Started newSnake newCherry, Random.generate Spawn randGenerator)
                 else
                    (Started newSnake newCherry, Cmd.none)
            _ -> Debug.crash "found a headless snake!"

        Spawn (chance, (randX, randY)) ->
          if chance <= 0.1 then
            let newCherry = spawnCherry randX randY
            in (Started snake newCherry, Cmd.none)
          else 
            (Started snake cherry, Cmd.none)

-- View
view : Model -> Html Msg
view model =
  let bg = rect (toFloat width) (toFloat height) |> filled black
      content =
        case model of
          NotStarted -> 
            [txt "press SPACE to start\n use [a, w, s, d] to control snake"]
          Started snake cherry ->
            let segments =
              snake.segments
              |> List.map (\pos ->
                rect segmentDim segmentDim
                |> filled yellow
                |> move pos)
            in case cherry of
                Nothing -> segments
                Just pos ->
                  (circle cherryRadius
                   |> filled white
                   |> move pos)::segments
  in collage width height (bg::content)
     |> Element.toHtml

-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    NotStarted ->
      Keyboard.presses KeyPress
    Started _ _ ->
      Sub.batch [
        Keyboard.presses KeyPress
      , Time.every (Time.inMilliseconds 50) Tick
      ]

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
    case fromCode keyCode of
      'a' -> ([ Up, Down ], Left) 
      'w' -> ([ Left, Right ], Up)
      'd' -> ([ Up, Down ], Right)
      's' -> ([ Left, Right ], Down)
      _  -> ([], currentDir)
  in if List.any ((==) currentDir) changeableDirs then newDir else currentDir

getNewSegment : (Float, Float) -> Direction -> (Float, Float)
getNewSegment (x, y) direction =
  case direction of
    Up    -> (x, y+segmentDim)
    Down  -> (x, y-segmentDim)
    Left  -> (x-segmentDim, y)
    Right -> (x+segmentDim, y)

isGameOver : (Float, Float) -> List (Float, Float) -> Bool
isGameOver newHead newTail =
  List.any ((==) newHead) newTail   -- eat itself
  || fst newHead > (width / 2)      -- hit bottom
  || snd newHead > (height / 2)     -- hit top
  || fst newHead < (-width / 2)     -- hit left
  || snd newHead < (-height / 2)    -- hit right

spawnCherry : Float -> Float -> Maybe (Float, Float)
spawnCherry randW randH =
  let x = randW * width - width / 2
      y = randH * height - height / 2
  in Just (x, y)

isOverlap : (Float, Float) -> (Float, Float) -> Bool
isOverlap (snakeX, snakeY) (cherryX, cherryY) =
  let (xd, yd) = (cherryX - snakeX, cherryY - snakeY)
      distance = sqrt(xd * xd + yd * yd)
  in distance <= (cherryRadius * 2)