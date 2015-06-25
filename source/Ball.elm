module Ball where

import DragAndDrop as Drag

import Color exposing ( Color )
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)


type alias Entity e = { e | x : Float
                          , y : Float }

type alias Ball = { x : Float
                  , y : Float
                  , vx : Float
                  , vy : Float
                  , color : Color
                  , radius : Float }

type alias ForceField = 
            { x : Float
            , y : Float
            , accelAt :  Float -- the distance from an entity to
                               -- the locus of the field;
                      -> Float -- the change in the entity's
                               -- velocity (in the direction of the
                               -- locus of the field) at that
                               -- distance.
            }


toForm : Ball -> Form
toForm ball = circle ball.radius
                      |> filled ball.color
                      |> move (ball.x, ball.y)

update : Maybe Drag.Action -> Entity e -> Entity e
update maction thing = 
        case maction of
            Nothing -> thing
            Just action ->
                case action of
                    Drag.Lift -> 
                        thing
                    Drag.MoveBy (x, y) ->
                        { thing | x <- thing.x + toFloat x
                                , y <- thing.y - toFloat y }
                        -- we negate y because (apparently) DragAndDrop
                        -- thinks +y is down, whereas Graphics.Collage
                        -- thinks +y is up.
                    Drag.Release -> 
                        thing

view : (Int, Int) -> Ball -> Element
view (width, height) ball = collage width height [toForm ball]
