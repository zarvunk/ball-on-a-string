module Wheeeeeeeeeeeeeeee where

import Signal exposing (..)

import Html exposing ( Html )

import Ball

main : Signal Html
main = map Ball.view ballState
    
ballState : Signal Ball
ballState = foldp Ball.update initialBall <| Ball.tracking ballState

update : (Ball.Coordinates -> Ball

initialBall : Ball
initialBall = Ball.cornerBall 12

isTracking : Signal (Ball -> Bool)
isTracking = Signal.map Ball.isOverBall Mouse.position

tracking : Signal Ball -> Signal (Maybe Drag.Action)
tracking theBall = Drag.track False <| isTracking theBall
