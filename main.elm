module Snake where

import Keyboard
import List
import Text
import Window

segmentDim = 15.0

txt msg = msg |> toText |> Text.color white |> Text.monospace |> leftAligned |> toForm

data Direction = Left | Right | Up | Down
type Snake     = { segments:[(Float, Float)], direction:Direction }
defaultSnake   = { segments=[ 0.0..8.0 ] |> List.reverse |> List.map (\x -> (x * segmentDim, 0)),    
                   direction=Right }
data GameState = NotStarted
               | Started Snake

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
  case gameState of
    NotStarted -> if input.x == 0 && input.y == 0 then gameState else Started defaultSnake
    Started snake ->
      let { segments, direction } = snake
          newDirection = getNewDirection input direction
          newHead      = getNewSegment (List.head segments) newDirection
          newTail      = List.take (List.length segments-1) segments
          isGameOver   = List.any (\t -> t == newHead) newTail
      in 
        if isGameOver then NotStarted
        else Started { segments = newHead::newTail, direction = newDirection }

display : (Int,Int) -> GameState -> Element
display (w, h) gameState = 
  let background = rect (toFloat w) (toFloat h) |> filled (rgb 0 0 0)
      content = 
        case gameState of
          NotStarted -> [ txt "Press any arrow key to start." ]
          Started snake -> 
            snake.segments
            |> List.map (\(x, y) -> rect segmentDim segmentDim 
                                    |> filled yellow
                                    |> move (x, y))
      
  in collage w h (background::content)
  
input : Signal { x:Int, y:Int }
input = sampleOn (fps 20) Keyboard.arrows

gameState = foldp stepGame NotStarted input

main = display <~ Window.dimensions ~ gameState