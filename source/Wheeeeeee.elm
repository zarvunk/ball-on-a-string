module Wheeeeeee where

import Signal exposing (..)
import Window
import Mouse

import Task exposing ( Task )

import Graphics.Element exposing ( Element, show, below, above )
import Graphics.Collage as Graphics exposing ( Form )
import Color exposing (..)
import Text exposing ( fromString )

import DragAndDrop as Drag

import Tuple exposing ( mapRight, mapBoth' )
import Maybe.Extra exposing ( isJust, isNothing )

import Keyboard
import Char

import Macro exposing (..)

import Ball exposing ( Ball, update )

-- the two commented-out lines display most recently recorded macro
-- (if there is one), for debugging.
main : Signal Element
main = -- map2 above
            (map2 Ball.view
                  Window.dimensions
                  ballState)
            -- (map show currentMacro)


-------------------------------------------------------------------
-- # Macro stuff # {{{1

recording : Signal Bool
recording = Keyboard.isDown <| Char.toCode 'R'

replaying : Signal Bool
replaying = Keyboard.isDown <| Char.toCode 'P'


currentMacro : Signal (Macro (Maybe Drag.Action))
currentMacro = record recording receiver


macroTransmitter : Mailbox (Maybe (Drag.Action))
macroTransmitter = mailbox Nothing


port macroSender : Signal (Task x ())
port macroSender = sampleOn
                        (notifyIf replaying)
                        (map (replay macroTransmitter.address)
                             currentMacro)


-- `notifyIf` turns a Signal of Bools into a Signal of nulls
-- that emits an event every time the given Signal Bool emits
-- a True.
notifyIf : Signal Bool -> Signal ()
notifyIf = filterMap
                ( \ whether -> if whether
                                  then Just ()
                                  else Nothing ) ()
-- }}}1

-------------------------------------------------------------------
-- # Ball stuff # {{{1

ballState : Signal Ball
ballState = 
    let ball = { x = 0
               , y = 0
               , color = green
               , radius = 24   }
     in foldp update
              ball
              <| merge receiver
                       macroTransmitter.signal


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
-- }}}1

-------------------------------------------------------------------
-- # Signal graph stuff # {{{1

transmitter : Mailbox Bool
transmitter = mailbox False

receiver : Signal (Maybe Drag.Action)
receiver = let -- we don't care about the Nothings --- and believe
               -- me, there are a *lot* of them. They rather clutter
               -- up the macros. In fact, all those Nothings was
               -- exactly what was slowing down the macro playback
               -- and consuming so much memory. Folding them all up
               -- wouldn't ordinarily be *so* bad --- though, seeing
               -- as they're effectively no-ops, they'd still slow
               -- the fold down, --- but the fold combines them as
               -- *Tasks*, which, insofar as they are basically
               -- promises, side effects reified into data, are
               -- essentially lazy; so that all those Nothings were
               -- not simply kept on the stack and combined as the
               -- stack unwound, but were in fact turned into
               -- closures stored on the heap; thus (a) having lots
               -- of storage overhead, and (b) having to wait to be
               -- garbage collected. Hence the memory usage kept
               -- growing. Plus it always took a while to play back
               -- all those no-op closures. ---Whereas with this
               -- sieve, macros play back instantly and no lag
               -- accumulates.
               sieve = filter isJust Nothing
            in sieve <| Drag.track False transmitter.signal

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
-- }}}1

-------------------------------------------------------------------
-- # Coordinates stuff # {{{1

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

-- }}}1
