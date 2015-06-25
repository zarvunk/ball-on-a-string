module Macro where

import Signal exposing ( Signal, Address, map, map2, foldp, send )
import Signal.Extra exposing ( keepThen, keepWhen, sampleWhen, filter )

import Task exposing ( Task, andThen )

import List

import Maybe exposing ( Maybe(..) )
import Maybe.Extra exposing ( isJust, isNothing )


-- A macro is just a list of actions, whatever your action type may be.
-- The functions in this module all build up macros from right to left, 
-- which means right is earlier and left is later.
type alias Macro action = List action

record :  Signal Bool           -- whether or not we're recording;
       -> Signal action         -- a Signal of the actions that we record;
       -> Signal (Macro action) -- a Signal of recorded macros.

record recording actions =

        let 
            macroActions =
                    keepThen recording Nothing
                            <| map Just actions

            prependMaybe ma lista =
                            case ma of
                                Just a -> a :: lista
                                Nothing -> lista

            macroSignal = foldp prependMaybe [] macroActions

         in 
            keepWhen (map not recording) [] macroSignal


type alias NamedMacro action =
                    { macro : Macro action
                    , name  : String       }

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
                        { macro = macro, name = name }
         in 
            map2 namedMacro macros <| filter "" mNames


replay :  Address action -- the address to which to send the actions;
       -> Macro action   -- the macro whose actions will be replayed in order;
       -> Task x ()      -- a Task that executes the replay.

replay address macro = 
            let append action task =
                            task `and` send address action
             in List.foldr append
                           ( Task.succeed () )
                           macro

-- a utility function that simply sequences two Tasks,
-- where the second Task does not depend on the return
-- value of the first.
and : Task x () -> Task x a -> Task x a
and task1 task2 = task1 `andThen` \ () -> task2
