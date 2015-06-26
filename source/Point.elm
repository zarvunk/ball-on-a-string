module Point where


type alias Point = (Float, Float)

type alias Angle = Float

type alias Magnitude = Float


{-| `isWithinRadiusOf` returns True if the distance between
the two points is less than or equal to the given Float,
otherwise returns False. -}
isWithinRadiusOf : Point -> Float -> Point -> Bool
isWithinRadiusOf point1 radius point2 = 
                        distance point1 point2 <= radius


{-| the distance between two points in the Euclidean plane. -}
distance : Point -> Point -> Magnitude
distance (x1, y1) (x2, y2) =
            let xsq = (x1 - x2)^2            
                ysq = (y1 - y2)^2            
             in sqrt <| xsq + ysq 


{-|
p2  +y
 ╲  ↑
  ╲ |
   ╲⤵ 𝛳
    p1──────⟶ +x

angle p1 p2 = 𝜃

The angle ranges from +π to −π. If p2 is below p1, then the
angle will be negative; otherwise it will be positive.
(The arrow of the angle in the diagram should actually point
the other way, but apparently there' no Unicode character
for that. Nor, for that matter, for an up-arrow without
vertical padding.)
-}
angle : Point -> Point -> Angle
angle (x1, y1) (x2, y2) = atan2 (y2 - y1) (x2 - x1)


{-| `vector p1 p2` computes the Cartesian vector from p1 to p2. -}
vector : Point -> Point -> Point
vector p1 p2 = relativeTo p2 p1


{-| `relativeTo somewhere origin` calculates what the coordinates
`somewhere` would be relative to `origin`, assuming that both
`somewhere` and `origin` are relative to (0,0). -}
relativeTo : (number, number) -> (number, number) -> (number, number)
relativeTo (x2, y2) (x1, y1) = (x2 - x1, y2 - y1)
