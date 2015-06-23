module Wheeeeeeeeeeeeeeee where

import Signal exposing (..)
import Window

import Graphics.Input exposing ( hoverable )
import Graphics.Element exposing ( Element )
import Graphics.Collage exposing ( Form )
import DragAndDrop as Drag
import Color exposing (..)

import Ball

main : Signal Element
main = let mainSignal = map2 (,)
                            Window.dimensions
                            (ballState transmitter) 

           transmitter = mailbox False

        in map (uncurry Ball.view) mainSignal

    
ballState : Mailbox Bool -> Signal Form
ballState transmitter = 

    let ball = hoverable send               -- a ball that knows  
                <| Ball.ball 21 green       -- when the mouse is  
                                            -- over it.           
                                     
        send : Bool -> Signal.Message
        send = flip Signal.message          -- the Bool represents
                        transmitter.address -- whether or not the
                                            -- mouse is over the
                                            -- ball. The Message
                                            -- just sends the Bool
                                            -- to the Signal which
                                            -- is watched by
                                            -- Drag.track.

        receive : Signal (Maybe Drag.Action)-- watches whether the
        receive = Drag.track False          -- mouse is over the
                        transmitter.signal  -- ball and whether the
                                            -- mousebutton is down
                                            -- and tells us where
                                            -- the thing is being
                                            -- dragged.

     in foldp Ball.update ball receive
