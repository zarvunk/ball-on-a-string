module Macro where

import Signal exposing ( Signal, map, map2, foldp )
import Signal.Extra exposing ( keepThen, keepWhen, sampleWhen, filter )

import Maybe exposing ( Maybe(..) )
import Maybe.Extra exposing ( isJust, isNothing )


type alias Macro action = List action

record :  Signal Bool           -- whether or not we're recording;
       -> Signal action         -- a Signal of the actions that we record;
       -> Signal (Macro action) -- a Signal of recorded macros.

record recording actions =

        let macroActions =
                    keepThen recording Nothing
                            <| map Just actions

            prependMaybe ma lista =
                            case ma of
                                Just a -> a :: lista
                                Nothing -> lista

            macroSignal = foldp prependMaybe [] macroActions

         in keepWhen (map not recording) [] macroSignal


type alias NamedMacro action =
                    { macro : Macro action
                    , name  : String       }

recordNamed :  Signal (Maybe String)      -- whether or not we're recording,
                                          -- and, if so, what is the name of
                                          -- the currently recording macro;
            -> Signal action              -- a Signal of the actions we record;
            -> Signal (NamedMacro action) -- the named macro.

recordNamed mNames actions =
        let recording = map isJust mNames
            macros = record recording actions
            namedMacro macro name = { macro = macro, name = name }
         in map2 namedMacro macros <| filter "" mNames
