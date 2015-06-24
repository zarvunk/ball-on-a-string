module Wheeeeeee where

import Signal exposing (..)
import Window
import Mouse

import Task exposing ( Task )

import Graphics.Element exposing ( Element )
import Graphics.Collage as Graphics exposing ( Form )
import Color exposing (..)
import Text exposing ( fromString )

import DragAndDrop as Drag

import Tuple exposing ( mapRight, mapBoth' )

import Debug

import Ball exposing ( Ball )


main : Signal Element
main = map2 Ball.view
            Window.dimensions
            ballState


transmitter : Mailbox Bool
transmitter = mailbox False

    
ballState : Signal Ball
ballState = 
    let ball = { x = 0
               , y = 0
               , color = green
               , radius = 24   }
     in foldp Ball.update ball receiver


type alias Point = (Float, Float)

-- `isWithinRadiusOf` returns True if the (Euclidean) distance
-- between the two points is less than or equal to the given Float,
-- otherwise returns False.
isWithinRadiusOf : Point -> Float -> Point -> Bool
isWithinRadiusOf (x1, y1) radius (x2, y2) =
                         let xsq = (x1 - x2)^2
                             ysq = (y1 - y2)^2
                             distance = sqrt <| xsq + ysq
                          in distance <= radius


-- `hoverable` is intended to do much the same thing as
-- Graphics.Input.hoverable, except it takes a Ball rather than an
-- Element. It detects whether the given point is within the
-- radius of the given Ball, and sends this information to the
-- given Address.
hoverable : Address Bool -> Ball -> Point -> Task x ()
hoverable address ball coords =
                send address <| isWithinRadiusOf (ball.x, ball.y)
                                                     ball.radius
                                                        coords

receiver : Signal (Maybe Drag.Action)
receiver = Drag.track False transmitter.signal

port sender : Signal (Task x ())
port sender = map2
                (hoverable transmitter.address)
                (ballState)
                (map2 relativeTo    -- because we render the Ball
                      mousePosition -- as a Form, its coordinates
                      windowCentre) -- are relative to the centre
                      -- of the window, whereas Mouse.position is
                      -- relative to the top left. This makes the
                      -- mouse position relative to the window
                      -- centre.

-- `relativeTo somewhere origin` calculates what the coordinates
-- `somewhere` would be relative to `origin`, assuming that both
-- `somewhere` and `origin` are relative to (0,0).
relativeTo : (number, number) -> (number, number) -> (number, number)
relativeTo (x2, y2) (x1, y1) = (x2 - x1, y2 - y1)

mousePosition : Signal (Float, Float)
mousePosition =     -- The other thing about Mouse.position is that
                    -- its y-coordinate is the positive distance down
                    -- from the top of the window; but from a Form's
                    -- perspective, down is not positive but
                    -- negative. In other words, we need to flip the
                    -- y-axis relative to the window. Hence
                    -- Window.height.
                let recombobulate (x, y) h = mapBoth' toFloat
                                                <| (x, h - y) 
                 in map2 recombobulate Mouse.position Window.height

windowCentre : Signal (Float, Float)
windowCentre = map (mapBoth' <| divideBy 2 << toFloat) Window.dimensions

divideBy : Float -> Float -> Float
divideBy = flip (/)
