port module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Keyboard
import Time exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Text
import Char
import Random exposing (..)

segmentDim = 15
cherryRadius = 7.5
(width, height) = (600, 600)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- step 1: define your Model
type Direction 
  = Up
  | Down
  | Left
  | Right

type alias Position = (Float, Float)

pos : Float -> Float -> Position
pos = (,)

type alias Snake = 
  { head: Position
  , tail: List Position
  , direction: Direction }

type alias Cherry = Maybe Position

type alias Score = Int
type alias HighScore = Int

type Model 
  = NotStarted
  | Started Snake Cherry Score HighScore

-- step 2: define Msg that can trigger updates to Model
type Msg 
  = Tick Time
  | KeyPress Keyboard.KeyCode
  | Spawn (Float, Position)

randPos = Random.pair (Random.float 0 1) (Random.float 0 1)

generator: Random.Generator (Float, Position)
generator = Random.pair (Random.float 0 1) randPos

-- step 3: define the initial state for the app
initSnake : Snake
initSnake = 
  let head = (0, 0)
      tail = [1..8] |> List.map (\n -> pos (-n*segmentDim) 0)
  in { head=head, tail=tail, direction=Right }

init : (Model, Cmd Msg)
init = (NotStarted, Cmd.none)

-- step 4: define your subscriptions - WebSocket, Keyboard, etc.
port playSound : () -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model = 
  case model of
    NotStarted ->
      Keyboard.presses KeyPress

    Started _ _ _ _ ->
      Sub.batch
        [ Keyboard.presses KeyPress
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

          Started snake cherry score hiScore ->
            let head = rect segmentDim segmentDim |> filled white |> move snake.head 
                tail =
                  snake.tail
                  |> List.map (\pos -> 
                    rect segmentDim segmentDim |> filled yellow |> move pos)
                scoreLbl = txt (toString score)
                hiScoreLbl = 
                  txt ("HI-SCORE " ++ toString hiScore) 
                  |> move (-width/2 + 50, height/2 - 15) 
            in case cherry of
                Nothing -> scoreLbl::hiScoreLbl::head::tail
                Just pos ->
                  (circle cherryRadius |> filled red |> move pos)::scoreLbl::hiScoreLbl::head::tail
  in collage width height (bg::content)
     |> Element.toHtml

-- step 6: implement state transition
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case model of
    NotStarted ->
      case msg of
        KeyPress 32 -> 
          (Started initSnake Nothing 0 0, Cmd.none)
        
        _ -> 
          (model, Cmd.none)

    Started snake cherry score hiScore ->
      case msg of
        KeyPress keyCode ->
          let newDir = getNewDirection keyCode snake.direction
              newSnake = { snake | direction=newDir }
          in (Started newSnake cherry score hiScore, Cmd.none)

        Spawn (chance, (randX, randY)) ->
          if chance <= 0.1 then
            let newCherry = spawnCherry randX randY
            in (Started snake newCherry score hiScore, Cmd.none)
          else
            (model, Cmd.none)

        Tick _ ->
          let newHead = getNewSegment snake.head snake.direction
              ateCherry =
                case cherry of
                  Just pos -> isOverlap newHead pos
                  Nothing -> False
              newTail = 
                if ateCherry then
                  snake.head::snake.tail
                else
                  snake.head::(List.take (List.length snake.tail-1) snake.tail)
              newSnake = { snake | head=newHead, tail=newTail }
              (newCherry, newScore) =
                if ateCherry then
                  (Nothing, score+1)
                else 
                  (cherry, score)
              gameOver = isGameOver newHead newTail
              newModel = 
                if gameOver then 
                  NotStarted 
                else
                  Started newSnake newCherry newScore hiScore
              cmd =
                if gameOver || newCherry /= Nothing then 
                  Cmd.none
                else 
                  Cmd.batch [
                    (if ateCherry then playSound () else Cmd.none),
                    Random.generate Spawn generator
                  ]
          in if gameOver then
              (NotStarted, Cmd.none)
             else if newCherry == Nothing then
              (Started newSnake newCherry newScore hiScore, Cmd.batch [cmd, Random.generate Spawn generator])
             else 
              (Started newSnake newCherry newScore hiScore, cmd)

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

isGameOver : Position -> List Position -> Bool
isGameOver newHead newTail =
  List.any ((==) newHead) newTail   -- eat itself
  || fst newHead > (width / 2)      -- hit bottom
  || snd newHead > (height / 2)     -- hit top
  || fst newHead < (-width / 2)     -- hit left
  || snd newHead < (-height / 2)    -- hit right

spawnCherry : Float -> Float -> Cherry
spawnCherry randW randH =
  let x = randW * width - width / 2
      y = randH * height - height / 2
  in pos x y |> Just

isOverlap : Position -> Position -> Bool
isOverlap (snakeX, snakeY) (cherryX, cherryY) =
  let (xd, yd) = (cherryX - snakeX, cherryY - snakeY)
      distance = sqrt(xd * xd + yd * yd)
  in distance <= (cherryRadius * 2)