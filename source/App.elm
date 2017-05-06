module App exposing ( main )

import Html exposing ( Html, program, body )
import Svg exposing ( Svg, svg, circle )
import Svg.Attributes as Attrs exposing ( cx, cy, r, fill )

import Time exposing ( Time )
import Color
import Char

import Keyboard
import AnimationFrame as Frame

import Draggable as Drag exposing ( Delta )
import Mechanics
import CssBasics as Css exposing ( CssValue, encodeCssValue )
import CssBasics.Properties exposing ( property )
import Stylesheet exposing (..)

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
    , paused : Bool
    }

type Msg = DragMoveBy Delta
         | DragMsg (Drag.Msg EntityID)
         | NextFrame Time
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
        SetPaused areWePaused ->
            ( { state | paused = areWePaused }
            , Cmd.none )
        DoNothing ->
            ( state
            , Cmd.none )

view : State -> Html Msg
view { ball, elastic } =
    let
        styleRules =
          newRuleSet
            |> withSelectors
              [ Tag "html", Tag "body" ]
            |> withDeclarations
              [ property.width := Css.Str "100%"
              , property.height := Css.Str "100%"
              , property.display := Css.Str "block" ]
        stylesheet =
            toStyleNode <| withRules [styleRules] newStylesheet
     in
        body
        []
        [ stylesheet
        , svg [
            Attrs.width "100%"
          , Attrs.height "100%"
          ]
          [
            Svg.circle [
              cx <| toString (x ball)
            , cy <| toString (y ball)
            , r  <| toString <| ball.radius
            , fill <| encodeCssValue (Css.Col ball.color)
            , Drag.mouseTrigger elastic.being.id DragMsg
            ]
            []
          ]
        ]

init : (State, Cmd Msg)
init = ( { ball = initialBall
         , elastic = initialElastic
         , friction = initialFriction
         , drag = Drag.init
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
           then [ Keyboard.presses handleKeyPressPaused ]
           else [ Keyboard.presses handleKeyPressPlaying
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

(:=) : String -> CssValue -> Css.Declaration
(:=) = (,)
