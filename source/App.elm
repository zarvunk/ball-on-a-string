module App exposing ( main )

import Html exposing ( Html, program )
import Svg exposing ( Svg, svg, circle )
import Svg.Attributes as Attrs exposing ( id, class, cx, cy, r, color )

import Time exposing ( Time )
import Color
import Char

import Window
import Keyboard
import AnimationFrame as Frame

import Draggable as Drag exposing ( Delta )
import Mechanics

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
    , paused : Bool
    }

type Msg = DragMoveBy Delta
         | DragMsg (Drag.Msg EntityID)
         | NextFrame Time
         | WindowResized Window.Size
         | SetPaused Bool
         | DoNothing

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
        SetPaused areWePaused ->
            ( { state | paused = areWePaused }
            , Cmd.none )
        DoNothing ->
            ( state
            , Cmd.none )

view : State -> Html Msg
view { ball, elastic, windowSize } = 
    svg [
        Attrs.width  <| toString windowSize.width
      , Attrs.height <| toString windowSize.height
      ]
      [
        Svg.circle [
            cx <| toString (x ball)
          , cy <| toString (y ball)
          , r  <| toString ball.radius
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
         , paused = False
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
subscriptions { drag, paused } =
    Sub.batch <|
        if paused
           then [ Keyboard.presses handleKeyPressPaused
                , Window.resizes WindowResized ]
           else [ Keyboard.presses handleKeyPressPlaying
                , Window.resizes WindowResized
                , Frame.diffs NextFrame
                , Drag.subscriptions DragMsg drag ]

handleKeyPressPaused : Char.KeyCode -> Msg
handleKeyPressPaused code =
    case Char.fromCode code of
        ' ' -> NextFrame 0.1
        'p' -> SetPaused False
        _   -> DoNothing

handleKeyPressPlaying : Char.KeyCode -> Msg
handleKeyPressPlaying code =
    case Char.fromCode code of
        'p' -> SetPaused True
        _   -> DoNothing
