module Macro.Timed where

import Signal exposing ( Signal, Address
                       , map, map2, foldp
                       , send, sampleOn, constant )

import Signal.Extra exposing ( filter )

import Task exposing ( Task, andThen )

import Time exposing ( Time, timestamp )

import Task.Extra exposing ( delay )
import Signal.Extra exposing ( foldps' )

import Maybe exposing ( Maybe(..) )
import Maybe.Extra exposing ( isJust, isNothing )

import Macro exposing ( and )



-- A timed macro is just a list of actions with delta timestamps,
-- whatever your action type may be.  The functions in this module all
-- build up macros from right to left, which means right is earlier
-- and left is later.
type alias Macro action = List (Time, action)



record :  Signal Bool           -- whether or not we're recording;
       -> Signal action         -- a Signal of the actions that we record;
       -> Signal (Macro action) -- a Signal whose value is the most recently 
                                -- recorded macro.

record recording actions =
        
    let
        timestampedActions = 
            timestamp actions
                |> foldps' relativeTime commenceRelativeTime

        relativeTime (absTime, action) prevAbsTime =
                    ((absTime - prevAbsTime, action), absTime)

        commenceRelativeTime (absTime, action) =
                    ((0, action), 0)

     in 
        Macro.record recording timestampedActions



replay :  Address action -- the address to which to send the actions;
       -> Macro action   -- the macro whose actions will be replayed in order;
       -> Task x ()      -- a Task that executes the replay.

replay address macro = 

            let appendTimed (timeDelta, action) task =
                            task
                            `and`
                            delay timeDelta ( send address action )

             in List.foldr appendTimed
                           ( Task.succeed () )
                           macro



--------------------------------------------------------------------------

-- ~~~~~~~~~~~~ --
-- Named Macros --
-- ~~~~~~~~~~~~ --

-- {{{1

type alias NamedMacro action =
                            ( String
                            , Macro action )



recordNamed :  Signal (Maybe String)      -- whether or not we're recording,
                                          -- and, if so, what is the name of
                                          -- the currently recording macro;
            -> Signal action              -- a Signal of the actions we record;
            -> Signal (NamedMacro action) -- the named macro.

recordNamed mNames actions =
        let 
            recording = map isJust mNames
            macros = record recording actions
            namedMacro macro name =
                        (name, macro)
         in 
            map2 namedMacro macros <| filter "" mNames

-- }}}1

--------------------------------------------------------------------------
