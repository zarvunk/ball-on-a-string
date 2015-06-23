module Wheeeeeeeeeeeeeeee where

import Signal exposing (..)
import Window
import Mouse

import Task exposing ( Task )

import Graphics.Element exposing ( Element )
import Graphics.Collage exposing ( Form, BasicForm(..) )
import Color exposing (..)

import DragAndDrop as Drag

import Tuple exposing ( mapBoth' )
import ListExtras exposing ( elementAt )

import Ball

main : Signal Element
main = let mainSignal = map2 (,)
                            Window.dimensions
                            ballState

        in map (uncurry Ball.view) mainSignal


transmitter : Mailbox Bool
transmitter = mailbox False

    
ballState : Signal Form
ballState = 

    let ball = Ball.ball 21 green       -- when the mouse is  
                                            -- over it.           
                                     
        receive : Signal (Maybe Drag.Action)-- watches whether the
        receive = Drag.track False          -- mouse is over the
                        transmitter.signal  -- ball and whether the
                                            -- mousebutton is down
                                            -- and tells us where
                                            -- the thing is being
                                            -- dragged.

     in foldp Ball.update ball receive


type alias Coordinates = (Float, Float)

isWithinRadiusOf : Coordinates -> Float -> Coordinates -> Bool
isWithinRadiusOf (x1, y1) radius (x2, y2) =
                         let xsq = (x1 - x2)^2
                             ysq = (y1 - y2)^2
                             distance = sqrt <| xsq + ysq
                          in distance <= radius

hoverable : Address Bool -> Form -> Coordinates -> Task x ()
hoverable address aForm coords = send address
                                  <| isWithinRadiusOf
                                       (aForm.x, aForm.y)
                                       (radius aForm)
                                       coords

port sender : Signal (Task x ())
port sender = map2
                (hoverable transmitter.address)
                ballState
                (map (mapBoth' toFloat) Mouse.position)

-- this only works if the form is a circle or an ellipse. If it's an
-- ellipse, radius returns the longest radius.
radius : Form -> Float
radius aForm = case aForm.form of
                 FShape _ theShape ->
                        (fst   <| fromJust (elementAt theShape 0))
                        - (fst <| fromJust (elementAt theShape 25))

fromJust : Maybe a -> a
fromJust a = case a of Just a -> a
