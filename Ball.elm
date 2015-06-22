module Ball where

import DragAndDrop as Drag
import Mouse
import Signal

import Html exposing (..)
import Html.Attributes exposing (..)

type alias Ball = { positionX : Float
                  , positionY : Float
                  , velocityX : Float
                  , velocityY : Float 
                  , attatched : Bool
                  , radius    : Int   }

type alias Coordinates = (Int, Int)

isOverBall : Coordinates -> Ball -> Bool
isOverBall (x, y) ball = let xsq = (toFloat x - ball.positionX)^2
                             ysq = (toFloat y - ball.positionY)^2
                             distance = sqrt <| xsq + ysq
                          in distance <= ball.radius

update : Maybe Drag.Action -> Ball -> Ball
update maction ball =
        case maction of
            Nothing -> ball
            Just action ->
                case action of
                        Lift ->    { ball | attached <- True }
                        Release -> { ball | attached <- False }
                        MoveBy (x, y) -> 
                            { ball | positionX <- x
                                   , positionY <- y }

cornerBall : Int -> Ball
cornerBall radius = { positionX = radius
                    , positionY = radius
                    , velocityX = 0
                    , velocityY = 0
                    , attached  = False
                    , radius    = radius }

view : Ball -> Html
view ball = let diameter = toString <| ball.radius * 2
                radius   = toString <| ball.radius
             in main' [] [
                        div [ id "ball"
                            , style [ ("width",  diameter)
                                    , ("height", diameter)
                                    , ("-moz-border-radius", radius)
                                    , ("-webkit-border-radius", radius)
                                    , ("border-radius", radius)
                                    , ("background-color", "green")
                                    , ("position", "absolute")
                                    , ("left", toString ball.positionX)
                                    , ("bottom", toString ball.positionY)
                                    ]
                            ]
                            []
                         ]
