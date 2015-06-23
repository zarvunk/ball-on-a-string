module Ball where

import DragAndDrop as Drag
import Mouse
import Signal

import List exposing ( head )
import MaybeUtils exposing ( fromJust )

import Color exposing ( Color )
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

type alias Object = { object    : Form
                    , velocityX : Float
                    , velocityY : Float }

ball : Float -> Color -> Form
ball radius color = filled color <| circle radius

update : Maybe Drag.Action -> Form -> Form
update maction ball = 
        case maction of
            Nothing -> ball
            Just action ->
                case action of
                    Drag.Lift -> ball    -- do we need to do something here?
                    Drag.MoveBy (x, y) ->
                        move (toFloat x, toFloat -y) ball
                        -- we negate y because (apparently) DragAndDrop
                        -- thinks +y is down, whereas Graphics.Collage
                        -- thinks +y is up.
                    Drag.Release -> ball -- ... or here?

view : (Int, Int) -> Form -> Element
view (width, height) aBall = collage width height [aBall]

-- this is a hack that only returns anything meaningfully like a radius
-- if the Form was created with Graphics.Collage.circle or
-- Graphics.collage.oval. In the latter case it returns the longest
-- radius.
radius : Form -> Float
radius aForm = case aForm.form of
                 FShape _ points ->
                        fst <| fromJust <| head points
                 -- the FShape constructor isn't actually exported
                 -- by Graphics.Collage; --- I had to edit the
                 -- source file. Frankly I'm not completely sure
                 -- what is the point of hiding the internals; ---
                 -- in what way would that API be misused?
                 --
                 -- Anyhow, the other reason this is a hack is that
                 -- it'll raise a runtime error given any Form that
                 -- was not created via a Shape.

relativeTo : (number, number) -> (number, number) -> (number, number)
relativeTo (x2, y2) (x1, y1) = (x2 - x1, y2 - y1)
