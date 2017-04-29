module App exposing ( main )

import Html exposing ( Html, program )
import Svg exposing ( Svg, svg, circle )
import Svg.Attributes as Attrs exposing ( id, class, cx, cy, r, color )
import Time exposing ( Time )
import Color

import Draggable as Drag exposing ( Delta )
import Mechanics
import AnimationFrame as Frame
import Window

import Ball exposing (..)

main : Program Never State Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions }

type alias State =
    { ball : Ball
    , friction : HasField ()
    , elastic : HasField Entity
    , drag : Drag.State EntityID
    , windowSize : Window.Size
    }

type Msg = DragMoveBy Delta
         | DragMsg (Drag.Msg EntityID)
         | NextFrame Time
         | WindowResized Window.Size

update : Msg -> State -> (State, Cmd Msg)
update msg ({ ball, elastic, friction } as state) =
    case msg of
        DragMoveBy delta_xy ->
            ( { state | elastic =
                { elastic | being = move delta_xy elastic.being } }
            , Cmd.none )
        DragMsg dragMsg ->
            Drag.update dragConfig dragMsg state
        NextFrame delta_t ->
            let
                fields = [ accelOf friction, accelOf elastic ]
            in
                ( { state | ball =
                            ball |> actOn fields delta_t
                  }, Cmd.none )
        WindowResized size ->
            ( { state | windowSize = size }
            , Cmd.none )

view : State -> Html Msg
view { ball, elastic, windowSize } = 
    svg [
          Attrs.width  <| toString windowSize.width
        , Attrs.height <| toString windowSize.height
        ]
        [
          Svg.circle [
              cx <| toString <| x ball
            , cy <| toString <| y ball
            , r  <| toString ball.radius
            , color <| toString ball.color
            , Drag.mouseTrigger elastic.being.id DragMsg
            ]
            []
        ]

init : (State, Cmd Msg)
init = ( { ball = initialBall
         , elastic = initialElastic
         , friction = initialFriction
         , drag = Drag.init
         , windowSize = Window.Size 400 400 -- I don't know what to do here
         }
       , Cmd.none )

initialBall : Ball
initialBall =
    { state = Mechanics.state2 (0, 0) (0, 0)
    , id = 1
    , color = Color.green
    , radius = 24
    }

initialElastic : HasField Entity
initialElastic =
    let
        strength = 0.00008
        being =
            { state = Mechanics.state2 (200, 0) (200, 0)
            , id = 2 }
    in
        elastic strength being

initialFriction : HasField ()
initialFriction =
    friction 0.001

dragConfig : Drag.Config EntityID Msg
dragConfig =
    Drag.basicConfig DragMoveBy

subscriptions : State -> Sub Msg
subscriptions { drag } =
    Sub.batch
        [ Frame.diffs NextFrame
        , Window.resizes WindowResized
        , Drag.subscriptions DragMsg drag ]
