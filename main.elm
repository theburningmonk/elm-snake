module Snake where

import Keyboard
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
stepGame newDirection gameState = gameState

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