module Snake where

import Debug
import Keyboard
import List
import Random
import Text
import Window

segmentDim   = 15.0
cherryRadius = 7.5

txt msg = msg |> toText |> Text.color white |> Text.monospace |> leftAligned |> toForm

data UserInput = Arrow { x:Int, y:Int } | Space
data Direction = Left | Right | Up | Down
type Snake     = { segments:[(Float, Float)], direction:Direction }
defaultSnake   = { segments  = [ 0.0..8.0 ] |> List.reverse |> List.map (\x -> (x * segmentDim, 0)),
                   direction = Right }
type Cherry    = Maybe (Float, Float)
data GameState = NotStarted | Started (Snake, Cherry)

getNewDirection : { x:Int, y:Int } -> Direction -> Direction
getNewDirection { x, y } currentDir =
  if | x == 0  && y == 0              -> currentDir
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
     
stepGame : (UserInput, (Int, Int), Float, Float) -> GameState -> GameState
stepGame (input, (w, h), rand1, rand2) gameState =
  case gameState of
    NotStarted -> if input == Space then Started (defaultSnake, Nothing) else gameState
    Started ({ segments, direction }, cherry) ->
      let arrow =  case input of
                    Arrow arrow -> arrow
                    _           -> { x=0, y=0 }
          newDirection = getNewDirection arrow direction
          newHead      = getNewSegment (List.head segments) newDirection
          newTail      = List.take (List.length segments-1) segments
          isGameOver   = 
            List.any (\t -> t == newHead) newTail -- eat itself
            || fst newHead > (toFloat w / 2)  -- hit bottom
            || snd newHead > (toFloat h / 2)  -- hit top
            || fst newHead < (toFloat -w / 2) -- hit left
            || snd newHead < (toFloat -h / 2) -- hit right
          newCherry    = if cherry == Nothing && rand1 <= 0.5 
                         then Just (0, 0) 
                         else cherry
      in if isGameOver then NotStarted
         else Started ({ segments = newHead::newTail, direction = newDirection }, newCherry)
              |> Debug.watch "state"

display : (Int,Int) -> GameState -> Element
display (w, h) gameState = 
  let background = rect (toFloat w) (toFloat h) |> filled (rgb 0 0 0)
      content = 
        case gameState of
          NotStarted -> [ txt "Press SPACE to start." ]
          Started (snake, cherry) -> 
            let snakeSegments = 
              snake.segments
              |> List.map (\(x, y) -> 
                   rect segmentDim segmentDim |> filled yellow |> move (x, y))
            in case cherry of
                 Just (x, y) -> (circle cherryRadius |> filled white |> move (x, y))::snakeSegments
                 _ -> snakeSegments
  in collage w h (background::content)
  
arrows = Arrow <~ Keyboard.arrows
spaces = (\t -> Space) <~ (keepIf (\t -> t) False Keyboard.space)
userInput = sampleOn (fps 20) (merge arrows spaces)

chances : Signal [Float]
chances   = Random.floatList <| sampleOn (every second) (constant 2) -- prob of spawning cherry

chance = Random.float (every second)

gameState = (,,,) <~ userInput ~ Window.dimensions ~ chance ~ chance |> foldp (Debug.watch "input" >> stepGame) NotStarted

main = display <~ Window.dimensions ~ gameState