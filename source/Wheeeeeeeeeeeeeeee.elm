module Wheeeeeeeeeeeeeeee where

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
main = let view (width, height) ball hovering = 
               Graphics.collage width height [ Ball.toForm ball
                                             , temperature hovering ]

           temperature hovering = Graphics.text 
                                    <| fromString
                                    <| if hovering
                                        then "Hot!"
                                        else "Cold..."

        in map3 view
            Window.dimensions
            ballState
            transmitter.signal


transmitter : Mailbox Bool
transmitter = mailbox False

    
ballState : Signal Ball
ballState = 

    let ball = { x = 0
               , y = 0
               , color = green
               , radius = 21   }
                 |> Debug.watch "ball"
                                 
                                     
        receive : Signal (Maybe Drag.Action)-- watches whether the
        receive = Drag.track False          -- mouse is over the
                        <| map (Debug.watch "hoverable")
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

{- [^message]: {{{1
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
   actually send / execute Messages in Elm itself. }}}1
-}

hoverable : Address Bool -> Ball -> Coordinates -> Task x ()
hoverable address ball coords = send address
                                  <| isWithinRadiusOf
                                       (ball.x, ball.y)
                                       ball.radius
                                       coords

port sender : Signal (Task x ())
port sender = map2
                (hoverable transmitter.address)
                (ballState)
                (map2 Ball.relativeTo
                      mousePosition
                      windowCentre)

mousePosition : Signal (Float, Float)
mousePosition = let recombobulate (x, y) h =
                                             mapBoth' toFloat
                                                <| (x, h - y) 
                 in map2 recombobulate Mouse.position Window.height

windowCentre : Signal (Float, Float)
windowCentre = map (mapBoth' <| divideBy 2 << toFloat) Window.dimensions

divideBy : Float -> Float -> Float
divideBy = flip (/)
