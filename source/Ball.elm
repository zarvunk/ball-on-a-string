module Ball where

import DragAndDrop as Drag
import Mouse
import Signal

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Graphics.Input   exposing (..)

type alias Object = { object    : Form
                    , velocityX : Float
                    , velocityY : Float }

ball : Float -> Color -> Form
ball radius color = filled color <| circle radius

type alias Coordinates = (Int, Int)

update : Maybe Drag.Action -> Form -> Form
update maction ball = 
        case maction of
            Nothing -> ball
            Just action ->
                case action of
                    Drag.Lift -> ball    -- do we need to do something here?
                    Drag.MoveBy (x, y) ->
                        move (toFloat x, toFloat y) ball
                    Drag.Release -> ball -- ... or here?

view : (Int, Int) -> Form -> Element
view (width, height) aBall = collage width height [aBall]
