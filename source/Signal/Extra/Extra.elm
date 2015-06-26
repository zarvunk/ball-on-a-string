module Signal.Extra.Extra where

import Signal exposing (..)
import Signal.Extra exposing (..)


{-| Drops all updates to a signal, keeps only the initial value.
-}
initSignal : Signal a -> Signal a
initSignal s =
  sampleOn (constant ()) s


-- like switchWhen and switchSample, except it samples the *second*
-- Signal but *not* the first.
switchWhenSample :  Signal Bool -> Signal a -> Signal a -> Signal a
switchWhenSample b l r = switchHelper keepWhen sampleWhen b l r

switchHelper :  (Signal Bool -> Maybe a -> Signal (Maybe a) -> Signal (Maybe a))
             -> (Signal Bool -> Maybe a -> Signal (Maybe a) -> Signal (Maybe a))
             ->  Signal Bool -> Signal a -> Signal a -> Signal a
switchHelper lfilter rfilter b l r =
  let
    base =
      (\bi li ri -> Just <| if bi then li else ri)
      <~ initSignal b
       ~ initSignal l
       ~ initSignal r
    
    lAndR =
      merge
        (lfilter b          Nothing (Just <~ l))
        (rfilter (not <~ b) Nothing (Just <~ r))
    
    fromJust (Just a) = a
  in
    fromJust <~ merge base lAndR
