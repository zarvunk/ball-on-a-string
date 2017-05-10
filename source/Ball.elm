module Ball exposing (..)

import Color exposing (Color)
import Time exposing (Time)
import Function
import Tuple2 exposing (..)
import Draggable as Drag exposing (Delta)
import Mechanics


type alias Is e =
    { e
        | state : Mechanics.State
        , id : EntityID
    }


type alias Entity =
    Is {}


type alias EntityID =
    Int


type alias Circular c =
    { c
        | color : Color
        , radius : Float
    }


type alias Ball =
    Circular Entity


type alias HasField locus =
    { field :
        locus
        -- the state of the locus of the field
        -> Mechanics.State
        -- the state of the thing affected by the
        -- field
        -> List Float

    -- the changes to the thing's velocities
    , being : locus
    }


elastic :
    Float -- the strength of the elastic
    -> Is thing
    -> HasField (Is thing)
elastic strength being =
    let
        field locus body =
            let
                distance_x =
                    x locus - x_ body

                distance_y =
                    y locus - y_ body

                accel_x =
                    distance_x * strength

                accel_y =
                    distance_y * strength
            in
                [ accel_x, accel_y ]
    in
        { being = being
        , field = field
        }


friction :
    Float -- the coefficient of drag
    -> HasField ()
friction drag =
    let
        field _ body =
            let
                velocity_x =
                    vx_ body

                velocity_y =
                    vy_ body

                decel_x =
                    decelerate drag velocity_x

                decel_y =
                    decelerate drag velocity_y
            in
                [ decel_x, decel_y ]
    in
        { being = ()
        , field = field
        }


decelerate : Float -> Float -> Float
decelerate drag velocity =
    let
        acceleration =
            min drag (abs velocity)
    in
        if velocity >= 0 then
            -acceleration
        else
            acceleration


move : Delta -> Is thing -> Is thing
move ( dx, dy ) thing =
    { thing
        | state =
            Mechanics.state2
                -- we assume two-dimensional coordinates
                ( x thing + dx, vx thing )
                ( y thing + dy, vy thing )
    }


x : Is thing -> Float
x =
    x_ << .state


y : Is thing -> Float
y =
    y_ << .state


vx : Is thing -> Float
vx =
    vx_ << .state


vy : Is thing -> Float
vy =
    vy_ << .state


x_ : Mechanics.State -> Float
x_ =
    Mechanics.coordinate 0


y_ : Mechanics.State -> Float
y_ =
    Mechanics.coordinate 1


vx_ : Mechanics.State -> Float
vx_ =
    Mechanics.velocity 0


vy_ : Mechanics.State -> Float
vy_ =
    Mechanics.velocity 1


accelOf : HasField being -> Mechanics.State -> List Float
accelOf { being, field } =
    field being


actOn : List (Mechanics.State -> List Float) -> Time -> Is thing -> Is thing
actOn accels dt ({ state } as body) =
    let
        addAccels =
            Function.map2 (List.map2 (+))

        zeroAccel =
            -- we're working in 2D only
            always [ 0, 0 ]

        totalAccel =
            accels
                |> List.foldr addAccels zeroAccel
                |> Mechanics.acceleration
    in
        { body | state = Mechanics.evolve totalAccel dt state }
