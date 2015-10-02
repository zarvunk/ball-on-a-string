module Ball where

import DragAndDrop as Drag

import Color exposing ( Color )
import Svg exposing ( Svg, svg, circle )
import Svg.Attributes as Attrs exposing ( id, class, cx, cy, r, color )
import Html exposing ( Html )
import Html.Events exposing ( onMouseEnter, onMouseLeave )

import Signal exposing ( Address )
import Time exposing ( Time )

import Tuple.Map exposing (..)
import Point exposing (..)


type alias Entity e = { e | x : Float
                          , y : Float }

type alias Body b =
             Entity { b | vx : Float
                        , vy : Float }

type alias Ball =
             Body (Circular {})

type alias Circular c =
              { c | color : Color
                  , radius : Float }

type alias Field f = 
     Entity { f |
              accelAt :  Float -- the distance from an entity to
                               -- the locus of the field;
                      -> Float -- the change in the entity's
                               -- velocity (in the direction of the
                               -- locus of the field) at that
                               -- distance.
            }


drag : Maybe Drag.Action -> Entity e -> Entity e
drag maction thing = 
        case maction of
            Nothing -> thing
            Just action ->
                case action of
                    Drag.Lift -> 
                        thing
                    Drag.MoveBy vector ->
                        move (mapBoth toFloat vector) thing
                    Drag.Release -> 
                        thing

move : Point -> Entity e -> Entity e
move (x, y) thing =
                        { thing | x <- thing.x + x
                                , y <- thing.y + y }

step : Time -> Body b -> Body b
step dt body =
               move (mapBoth ((*) dt) (body.vx, body.vy)) body

actOn : Field f -> Time -> Body b -> Body b
actOn field dt body =
                      let
                          ax = field.accelAt <| field.x - body.x
                          ay = field.accelAt <| field.y - body.y
                          dvx = ax * dt
                          dvy = ay * dt
                       in
                          { body | vx <- body.vx + dvx
                                 , vy <- body.vy + dvy }

decelerate : Float -> Float -> Float
decelerate dv v = 
                  let 
                      dv' = min (abs v) dv
                   in 
                      if v >= 0
                         then v - dv'
                         else v + dv'

applyFriction : Float -> Time -> Body b -> Body b
applyFriction decel dt body =
            let
                dv = decel * dt
             in 
                { body | vx <- decelerate dv body.vx
                       , vy <- decelerate dv body.vy }


view : Address Bool -> (Int, Int) -> Entity (Circular c) -> Html
view address (width, height) ball =
        svg [
              Attrs.width  <| toString width
            , Attrs.height <| toString height
            ]
            [
              Svg.circle [
                  cx <| toString ball.x
                , cy <| toString ball.y
                , r  <| toString ball.radius
                , color <| toString ball.color
                , onMouseEnter address True
                , onMouseLeave address False
                ]
                []
            ]
