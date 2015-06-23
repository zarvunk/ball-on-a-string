module Wheeeeeeeeeeeeeeee where

import Signal exposing (..)
import Window
import Mouse

import Task exposing ( Task )

import Graphics.Element exposing ( Element )
import Graphics.Collage exposing ( Form )
import Color exposing (..)

import DragAndDrop as Drag

import Tuple exposing ( mapBoth' )

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

    let ball = Ball.ball 21 green
                                 
                                     
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


-- hoverable is intended to do much the same thing as
-- Graphics.Input.hoverable, except it takes a Form rather than an
-- Element. It detects whether the given Coordinates are within the
-- boundaries of the given Form (which is assumed to be a
-- circle[^type]), and sends this information (True or False) to the
-- given Address[^message]. Naturally you'll want to map it over a Signal or
-- two and bind the result to a port.

{- [^message]:
   Graphics.Input.hoverable takes, instead of an Address, a function
   from a Bool to a Message, --- which fulfills the same purpose and
   amounts to the same thing, since a Message (if I understand it
   right) is just a wrapper for the act of sending a thing to some
   address (that is, the address and thing you passed in to
   Signal.message). But in what way are Messages better? The
   documentation suggests that their advantage is that they're an
   alternative to Tasks, only for sending to Addresses, --- which
   means you don't need a port, I guess? A port being what allows
   you to "execute arbitrary Tasks" (I quote the docs for Message
   from memory), which maybe you don't always want. But then how do
   you execute / send a Message? That's not clear to me. Graphics.Input,
   for its part, implements hoverable (and thus this whole Message
   business) via Native.Graphics.Input, which is to say via Javascript,
   using Elm's Javascript-facing API. So maybe there is no way to
   actually send / execute Messages in Elm itself.
-}

hoverable : Address Bool -> Form -> Coordinates -> Task x ()
hoverable address aForm coords = send address
                                  <| isWithinRadiusOf
                                       (aForm.x, aForm.y)
                                       (Ball.radius aForm)
                                       coords

port sender : Signal (Task x ())
port sender =
           let mousePosition =
                      map (mapBoth' toFloat) Mouse.position
               windowCentre  = 
                      map (mapBoth' <| divideBy 2 << toFloat)
                                             Window.dimensions
            in map2
                (hoverable transmitter.address)
                (ballState)
                (map2 Ball.relativeTo
                      mousePosition
                      windowCentre)

divideBy : Float -> Float -> Float
divideBy = flip (/)
