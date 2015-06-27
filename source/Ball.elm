module Ball where

import DragAndDrop as Drag

import Color exposing ( Color )
import Graphics.Collage as Form exposing (..)
import Graphics.Element exposing (..)

import Time exposing ( Time )

import Tuple exposing (..)
import Point exposing (..)


type alias Entity e = { e | x : Float
                          , y : Float }

type alias Body b =
             Entity { b | vx : Float
                        , vy : Float }

type alias Ball =
             Body { color : Color
                  , radius : Float }

type alias ForceField = 
     Entity {
              accelAt :  Float -- the distance from an entity to
                               -- the locus of the field;
                      -> Float -- the change in the entity's
                               -- velocity (in the direction of the
                               -- locus of the field) at that
                               -- distance.
            }


toForm : Ball -> Form
toForm ball = circle ball.radius
                      |> filled ball.color
                      |> Form.move (ball.x, ball.y)

drag : Maybe Drag.Action -> Entity e -> Entity e
drag maction thing = 
        case maction of
            Nothing -> thing
            Just action ->
                case action of
                    Drag.Lift -> 
                        thing
                    Drag.MoveBy vector ->
                        move thing <| mapBoth toFloat
                                   <| mapRight negate vector
                        -- we negate y because (apparently) DragAndDrop
                        -- thinks +y is down, whereas Graphics.Collage
                        -- thinks +y is up.
                    Drag.Release -> 
                        thing

move : Entity e -> Point -> Entity e
move thing (x, y) =
                        { thing | x <- thing.x + x
                                , y <- thing.y + y }

step : Time -> Body b -> Body b
step dt body =
               move body <| mapBoth ((*) dt) (body.vx, body.vy)

actOn : ForceField -> Time -> Body b -> Body b
actOn field dt body =
                      let
                          ax = field.accelAt <| field.x - body.x
                          ay = field.accelAt <| field.y - body.y
                          dvx = ax * dt
                          dvy = ay * dt
                       in
                          { body | vx <- body.vx + dvx
                                 , vy <- body.vy + dvy }


view : (Int, Int) -> Ball -> Element
view (width, height) ball = collage width height [toForm ball]
