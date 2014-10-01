module Snake where

import Keyboard
import List
import Text
import Window

segmentDim = 15.0
startSegments = [ 0.0..8.0 ] |> List.reverse |> List.map (\x -> (x * segmentDim, 0))

txt msg = msg |> toText |> Text.color white |> Text.monospace |> leftAligned |> toForm

data Direction = Left | Right | Up | Down
type Snake     = { segments:[(Float, Float)], direction:Direction }
type GameState = { cherry:Maybe (Float, Float), snake:Snake }
defaultGameState = { 
  cherry = Nothing, 
  snake  = { segments=startSegments, direction=Right } }

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
     
stepGame : { x:Int, y:Int } -> GameState -> GameState
stepGame input gameState =
    let { segments, direction } = .snake gameState
        newDirection = getNewDirection input direction
        newSegment   = getNewSegment (List.head segments) newDirection        
        count        = List.length segments
        newSegments  = newSegment::(List.take (count-1) segments)
        newSnake     = { segments=newSegments, direction=newDirection }
    in { gameState | snake <- newSnake }

display : (Int,Int) -> GameState -> Element
display (w, h) gameState = 
  let snake = 
        gameState.snake.segments 
        |> List.map (\(x, y) -> rect segmentDim segmentDim 
                                |> filled yellow
                                |> move (x, y))
      background = rect (toFloat w) (toFloat h) |> filled (rgb 0 0 0)
  in collage w h (background::snake)

input : Signal { x:Int, y:Int }
input = sampleOn (fps 20) Keyboard.arrows

gameState = foldp stepGame defaultGameState input

main = lift2 display Window.dimensions gameState