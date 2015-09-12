import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Keyboard
import List
import List exposing (..)
import Random
import Random exposing (..)
import Signal
import Signal exposing (..)
import Text 
import Text exposing (..)
import Time
import Window
import Debug

segmentDim = 15.0
cherryRadius = 7.5
initSeed = Random.initialSeed 42

type UserInput = Arrow {x:Int, y:Int} | Space
defaultUserInput = Arrow {x=0, y=0}

arrows : Signal UserInput
arrows = Arrow <~ Keyboard.arrows

spaces : Signal UserInput
spaces = (\p -> if p then Space else defaultUserInput) <~ Keyboard.space

userInput : Signal UserInput
userInput = merge arrows spaces

type alias Input =
    { windowDim : (Int, Int)
    , userInput : UserInput
    }
type Direction = Up | Down | Left | Right
type alias Snake = {segments:List(Float, Float), direction:Direction}
defaultSnake = 
  {segments =
    [0.0..8.0]
    |> List.map (\n -> (n*segmentDim, 0))
    |> List.reverse,
   direction=Right}
type alias Cherry = Maybe(Float, Float)
type GameState = NotStarted | Started Snake Cherry Seed

defaultGame : GameState
defaultGame = NotStarted

stepGame : Input -> GameState -> GameState
stepGame {windowDim,userInput} gameState =
  case gameState of
    NotStarted ->
      if userInput == Space then Started defaultSnake Nothing initSeed
      else gameState
    Started {segments, direction} cherry seed ->  
      let newDir = getNewDirection userInput direction
          (head::tail) = segments
          newHead = getNewSegment head newDir
          ([spawn, randX, randY], newSeed) = genRandoms 3 seed
          ateCherry =
            case cherry of
              Nothing -> False
              Just pos -> isOverlap newHead pos
          newCherry =
            if ateCherry then Nothing
            else
              if cherry == Nothing && spawn <= 0.1
              then spawnCherry windowDim randX randY
              else cherry
          newTail = 
            if ateCherry then segments
            else List.take (List.length segments-1) segments
          newSnake = {segments=newHead::newTail, direction=newDir}
          gameOver = isGameOver windowDim newHead newTail
      in if gameOver then NotStarted
         else Started newSnake newCherry newSeed

display : (Int,Int) -> GameState -> Element
display (w,h) gameState = 
  let bg = rect (toFloat w) (toFloat h) |> filled black
      content =
        case (Debug.watch "gamestate" gameState) of
          NotStarted -> [txt "press SPACE to start"]
          Started snake cherry _ ->
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
  in collage w h (bg::content)

delta : Signal Float
delta = Time.fps 10

input : Signal Input
input = Signal.sampleOn delta (Input<~Window.dimensions~userInput)

gameState : Signal GameState
gameState = Signal.foldp stepGame defaultGame input

main : Signal Element
main = display<~Window.dimensions~gameState


txt msg = 
    msg 
    |> Text.fromString 
    |> Text.color white 
    |> Text.monospace 
    |> leftAligned 
    |> toForm


getNewDirection : UserInput -> Direction -> Direction
getNewDirection userInput currentDir =
  let (x, y) = 
    case userInput of
        Arrow {x, y} -> (x, y)
        _            -> (0, 0)
  in if | x == 0  && y == 0              -> currentDir
        | x == -1 && currentDir == Right -> currentDir
        | x == 1  && currentDir == Left  -> currentDir
        | y == -1 && currentDir == Up    -> currentDir
        | y == 1  && currentDir == Down  -> currentDir
        | x == -1 -> Left
        | x == 1  -> Right
        | y == -1 -> Down
        | y == 1  -> Up
     
getNewSegment (x, y) direction =
  case direction of
    Up    -> (x, y+segmentDim)
    Down  -> (x, y-segmentDim)
    Left  -> (x-segmentDim, y)
    Right -> (x+segmentDim, y)

isOverlap (snakeX, snakeY) (cherryX, cherryY) =
  let (xd, yd) = (cherryX - snakeX, cherryY - snakeY)
      distance = sqrt(xd * xd + yd * yd)
  in distance <= (cherryRadius * 2)

isGameOver (w,h) newHead newTail =
    List.any (\t -> t == newHead) newTail -- eat itself
    || fst newHead > (toFloat w / 2)      -- hit bottom
    || snd newHead > (toFloat h / 2)      -- hit top
    || fst newHead < (toFloat -w / 2)     -- hit left
    || snd newHead < (toFloat -h / 2)     -- hit right

genRandoms n seed = Random.generate (Random.list n (Random.float 0 1)) seed

spawnCherry (w, h) randW randH =
    let x = randW * toFloat w - toFloat w/2
        y = randH * toFloat h - toFloat h/2
    in Just (x, y)
--}