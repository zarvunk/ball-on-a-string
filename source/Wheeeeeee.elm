module Wheeeeeee where

import Signal exposing (..)
import Signal.Extra exposing ( mapMany, zip )
import Window
import Mouse
import Time exposing ( Time, fps )

import Color
import List

import Task exposing ( Task )

import Html exposing ( Html )

import DragAndDrop as Drag

import Tuple exposing ( mapRight, mapBoth )
import Maybe.Extra exposing ( isJust, isNothing )

import Keyboard
import Char

import Macro.Timed exposing (..)

import Point exposing (..)
import Ball exposing (..)

{- the commented-out lines display useful info for debugging. -}
main : Signal Html
main = 
            (map2 (Ball.view transmitter.address)
                  Window.dimensions
                  ballState)

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
               , vx = 0
               , vy = 0
               , color = Color.green
               , radius = 24   }

        surface = 0.002

        update (field, dt) body =
             body |> actOn field dt
                  |> applyFriction surface dt
                  |> step dt

     in foldp update ball
           <| zip
                (sampleOn timestream elasticState)
                (timestream)

timestream : Signal Time
timestream = fps 30

elasticState : Signal (Field {})
elasticState = 

    let elastic =
               { x = 78
               , y = 78
               , accelAt d = d * 0.00008
               }

     in foldp drag elastic
           <| merge
                receiver
                macroTransmitter.signal

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

-- }}}1

-------------------------------------------------------------------
-- # Coordinates stuff # {{{1

mousePosition : Signal (Float, Float)
mousePosition =
                    map (mapBoth toFloat) Mouse.position

-- }}}1
