module Macro.Timed ( Macro, NamedMacro
                   , record, recordNamed
                   , replay
                   ) where

{-| A timed macro is that wherewith you record the output of a
Signal over a certain period of time, keeping track of the relative
timings of the things recorded.

# Macros
@docs Macro

# Recording
@docs record

# Replaying
@docs replay

# Named macros
@docs NamedMacro, recordNamed
-}

import Signal exposing ( Signal, Address
                       , map, map2, foldp
                       , send, sampleOn, constant )

import Signal.Extra exposing ( sampleWhen, filter
                             , foldps, foldps' )

import Signal.Extra.Extra exposing ( switchWhenSample )

import Task exposing ( Task, andThen )

import Time exposing ( Time, timestamp )

import Task.Extra exposing ( delay )
import Task.And exposing ( and )

import Maybe exposing ( Maybe(..) )
import Maybe.Extra exposing ( isJust, isNothing )



{-| A timed macro is just a list of actions with delta timestamps,
whatever your action type may be.  The functions in this module all
build up macros from right to left, which means right is earlier
and left is later.
-}
type alias Macro action = List (Time, action)



{-| This does about what you'd expect. The `Signal action` is the
Signal that you're recording from; and the `Signal Bool` controls
when recording happens: When it emits True, recording begins, keeping
track of the timestamps; and the session ends when the signal emits
False.

As soon as a recording session finishes, the resulting `Signal (Macro
action)` emits the timed macro that was just recorded.

To get a Signal that produces a list of all the macros recorded so
for, you might do something like

    foldp (::) [] <| record areWeRecording whatWeWouldRecordFrom
-}
record :  Signal Bool           -- whether or not we're recording;
       -> Signal action         -- a Signal of the actions that we record;
       -> Signal (Macro action) -- a Signal whose value is the most recently 
                                -- recorded macro.
record recording actions =

    let 
        macroActions =

                -- see note [^keepThen] in Macro.elm.
                switchWhenSample

                           -- tells us when a recording session
                           -- starts and ends.
                           (recording)

                           -- timestamp 'em.
                           (map Just <| timestamp actions)

                           -- emitted once right when we finish a
                           -- recording session.
                           (constant Nothing)


        relativeTime (absTime, action) prevAbsTime =
                    ((absTime - prevAbsTime, action), absTime)


        -- see note [^reset] in Macro.elm.
        prependMaybeTimestamped mTaction (prevAbsTime, lista) =

                case (lista, mTaction) of

                    -- if we're just starting a recording
                    -- session, then set the time-delta of
                    -- the (first) action to zero.

                    ([], Just (absTime, action)) ->
                        ([], (absTime, (0, action) :: lista))

                    -- add the given action to the
                    -- currently-recording macro; don't
                    -- pass the macro onward just yet.
                    (_, Just taction) ->
                        let (timedAction, absTime) =
                                relativeTime taction prevAbsTime
                         in ([], (absTime, timedAction :: lista))

                    -- we've just stopped recording;
                    -- return the just-recorded macro and
                    -- start afresh.
                    (_, Nothing) ->
                        (lista, (0, []))


        macroSignal = foldps prependMaybeTimestamped
                             ([], (0, []))
                             macroActions

     in 
        -- as soon as we finish recording, sample the
        -- just-recorded macro; otherwise empty list.
        -- This may in fact be redundant:---see
        -- `prependMaybe`, above. 
        sampleWhen (map not recording) [] macroSignal




{-| Storing a macro (or e.g. a list of macros) and conditionally
propogating it (or one of them) later on is admittedly trivial; but
`replay` is handy if you want to send the actions in the macro back
through the same Signal they were recorded from, and preserve their
relative time-distances. It sends each of the macro's actions in turn
to an address, with the same timing that they had when they were
recorded---or, rather, produces a Task that does exactly that.

    port timestwo =
        Signal.map (replay someMailbox.address) <| record recYesNo someMailbox.signal

-}
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

{-| A `NamedMacro` is a macro with a name. Simple as that.
-}
type alias NamedMacro action =
                            ( String
                            , Macro action )



{-| Same deal as `record`, above,---except signaling `recordNamed` to
start recording amounts to telling it what to call the
currently-recording macro.

It wouldn't be hard to fold the resulting `Signal (NamedMacro action)`
into a `Signal (Dict String (Macro action))`. Something in the vein of

    foldp (uncurry Dict.insert) Dict.empty <| recordNamed nameSig actSig
-}
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
