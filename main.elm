module Snake where

import Keyboard
import List
import Text
import Window

txt msg = msg |> toText |> Text.color white |> Text.monospace |> leftAligned |> toForm

data Direction = Left | Right | Up | Down
fromArrow { x, y } =
  case (x, y) of
    (0,  0)   -> Nothing
    (-1, _)   -> Just Left
    (1,  _)   -> Just Right
    (_, -1)   -> Just Down
    (_, 1 )   -> Just Up

type Snake     = { segments:[(Int, Int)], direction:Direction }
type GameState = { cherry:Maybe (Int, Int), snake:Snake }

defaultGame : GameState
defaultGame = { 
  cherry = Nothing, 
  snake  = { segments=[(0, 0), (1, 0), (2, 0), (3, 0), (4, 0)], direction=Right } }

stepGame : (Maybe Direction) -> GameState -> GameState
stepGame newDirection gameState =
    let { segments, direction } = .snake gameState
        (hdx, hdy) = List.head segments
        count      = List.length segments
        (newSeg, newDir) = 
            case (newDirection, direction) of
                -- ignore direction changes in reverse
                (Just Up, Down)    -> ((hdx, hdy-1), Down)
                (Just Down, Up)    -> ((hdx, hdy+1), Up)
                (Just Left, Right) -> ((hdx+1, hdy), Right)
                (Just Right, Left) -> ((hdx-1, hdy), Left)
                -- change direction
                (Just Up,    _)    -> ((hdx, hdy+1), direction)
                (Just Down,  _)    -> ((hdx, hdy-1), direction)
                (Just Left,  _)    -> ((hdx-1, hdy), direction)
                (Just Right, _)    -> ((hdx+1, hdy), direction)
                -- continue current direction
                (Nothing, Down)    -> ((hdx, hdy+1), direction)
                (Nothing, Up)      -> ((hdx, hdy-1), direction)
                (Nothing, Right)   -> ((hdx+1, hdy), direction)
                (Nothing, Left)    -> ((hdx-1, hdy), direction)
        
        newSegments = newSeg::(List.take (count-1) segments)
        newSnake    = { segments=newSegments, direction=newDir }
    in { gameState | snake <- newSnake }

drawBackground (w, h) =
  collage w h <| [ rect (toFloat w) (toFloat h) |> filled (rgb 0 0 0) ]

drawGame : (Int, Int) -> GameState -> Element
drawGame (w, h) gameState =
  collage w h [ txt "game over" ]

display : (Int,Int) -> GameState -> Element
display dim gameState = layers [ drawBackground dim
                               , drawGame dim gameState ]

input : Signal (Maybe Direction)
input = sampleOn (fps 45) (fromArrow <~ Keyboard.arrows)

gameState = foldp stepGame defaultGame input

main = lift2 display Window.dimensions gameState