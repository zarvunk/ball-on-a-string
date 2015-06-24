module Ball where

import DragAndDrop as Drag

import Color exposing ( Color )
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Ball = { x : Float
                  , y : Float
                  , color : Color
                  , radius : Float }

toForm : Ball -> Form
toForm ball = circle ball.radius
                      |> filled ball.color
                      |> move (ball.x, ball.y)

update : Maybe Drag.Action -> Ball -> Ball
update maction ball = 
        case maction of
            Nothing -> ball
            Just action ->
                case action of
                    Drag.Lift -> 
                        -- switch from green to purple
                        { ball | color <- Color.complement ball.color }
                    Drag.MoveBy (x, y) ->
                        { ball | x <- ball.x + toFloat x
                               , y <- ball.y - toFloat y }
                        -- we negate y because (apparently) DragAndDrop
                        -- thinks +y is down, whereas Graphics.Collage
                        -- thinks +y is up.
                    Drag.Release -> 
                        -- switch back from purple to green
                        { ball | color <- Color.complement ball.color }

view : (Int, Int) -> Ball -> Element
view (width, height) ball = collage width height [toForm ball]
