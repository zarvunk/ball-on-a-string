module Macro ( Macro, NamedMacro
             , record, recordNamed
             , replay
             ) where

{-| A macro is that wherewith you record the output of a
Signal over a certain period of time.

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

import Signal.Extra exposing ( keepThen, keepWhen
                             , sampleWhen, switchWhen
                             , filter, foldps )

import Signal.Extra.Extra exposing ( switchWhenSample )

import Task exposing ( Task, andThen )
import Task.And exposing ( and )

import List

import Maybe exposing ( Maybe(..) )
import Maybe.Extra exposing ( isJust, isNothing )


{-| A macro is just a list of actions, whatever your action type may be.
The functions in this module all build up macros from right to left, 
which means right is earlier and left is later.
-}
type alias Macro action = List action

{-| This does about what you'd expect. The `Signal action` is the
Signal that you're recording from; and the `Signal Bool` controls
when recording happens: When it emits True, a recording session
starts; and the session ends when the signal emits False.

As soon as a recording session finishes, the resulting
`Signal (Macro action)` emits the macro that was just recorded.

To get a Signal that produces a list of all the macros recorded so
for, you would do something like

    foldp (::) [] <| record areWeRecording whatWeWouldRecordFrom
-}
record :  Signal Bool           -- whether or not we're recording;
       -> Signal action         -- a Signal of the actions that we record;
       -> Signal (Macro action) -- a Signal whose value is the most recently 
                                -- recorded macro.

record recording actions =

        let 
            macroActions =

                    -- see note [^keepThen].
                    switchWhenSample

                               -- tells us when a recording session
                               -- starts and ends.
                               (recording)

                               (map Just actions)

                               -- emitted once right when we finish a
                               -- recording session.
                               (constant Nothing)


            -- see note [^reset].
            prependMaybe ma lista =
                            case ma of
                                -- add the given action to the
                                -- currently-recording macro; don't
                                -- pass the macro onward just yet.
                                Just a ->
                                    ([], a :: lista)
                                -- we've just stopped recording;
                                -- return the just-recorded macro and
                                -- start afresh.
                                Nothing ->
                                    (lista, [])

            macroSignal = foldps prependMaybe ([], []) macroActions

         in 
            -- as soon as we finish recording, sample the
            -- just-recorded macro; otherwise empty list.
            -- This may in fact be redundant:---see
            -- `prependMaybe`, above. 
            sampleWhen (map not recording) [] macroSignal


{- [^keepThen]: {{{1

Originally this was defined not with `switchWhenSample`
(from Signal.Extra.Extra) but with keepThen (from
Signal.Extra). What we want to do here, after all, is (a)
when the value of `recording` becomes True, start
listening to `actions`, re-emitting anything it sends; (b)
when `recording` becomes False, emit the value Nothing.
`keepWhen` would have done the trick, except it doesn't
emit Nothing again when `recording` becomes False.
`keepThen`, however, does; it ought to fit the bill.
Except that it doesn't work exactly as advertised: There's
an additional difference between it and `keepWhen`, namely
that it *samples* the given Signal (in this case
`actions`) immediately when it switches to it (when
`recording` emits a True) --- in this respect it behaves
like `sampleWhen`. What you get by sampling a Signal is
the most recent value that the Signal has emitted ---
which will of course have been emitted *before* the act of
sampling. Which is to say that switching to `actions`
(upon `recording` giving us a True) and sampling it right
away actually gives us the most recent action emitted
*before* recording was supposed to begin!  That will not
stand!

Why does `keepThen` behave this way? Well, it turns out that
`keepThen` is actually implemented in terms of switch helper, Which,
naturally enough, is also the basis for `switchWhen` and
`switchSample`. Why in terms of the switch helper?  Well, the point of
`keepThen` is that it should re-emit the default value when the
toggle signal emits False; Which is implemented as switching back
to a constant signal whose single never-emitted value is the
default value that you give to `keepThen`. But switch helper will,
depending on one of its parameters that you give it, either sample
each of its Signals whenever it switches to either of them, or not
sample but only listen.  What we want is to only sample from the
constant signal, because otherwise you don't get anything from it,
since it never emits anything. We need more flexibility than
`switchHelper` allows for. So I wrote my own `switchHelper`
function in the module Signal.Extra.Extra for precisely this
purpose.

}}}1 -}

{- [^reset]: {{{1

Originally I was using not `foldps` but good old `foldp`, with
the fold being a simple List-prepend (`::`). But the problem
with that was that the list we were folding over persisted
over all recording sessions; when the provided Signal of Bool
switched from True to False and then back to True, the `foldp`
would keep prepending to the same list as before. What was
needed was a way to "reset" the folded-over list when
`recording` emits a False, so that the list always
corresponded to the currently-recording macro, rather than a
cumulative list of everything we've ever recorded. It became
clear that the only way to do this was to have the fold
maintain an extra bit of state, which would tell it whether
the incoming action was the next action in the current macro
or the first action in a new macro. But it would have been
silly for this state to be passed downstream, only to be
discarded; it's distinct from what needs to be passed onward
and entirely internal to the fold-----a perfect chance for
`foldps` to shine!

Hopefully the code plus its interleaved comments are clear
enough that you can see how this mechanism works.

}}}1 -}

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



{-| Storing a macro (or e.g. a list of macros) and conditionally
propogating it (or one of them) later on is admittedly trivial; but
`replay` is handy if you want to send the actions in the macro back
through the same Signal they were recorded from. It sends each of the
macro's actions in turn to an address---or, rather, produces a Task
that does exactly that.

    port timestwo =
        Signal.map (replay someMailbox.address) <| record recYesNo someMailbox.signal

-}
replay :  Address action -- the address to which to send the actions;
       -> Macro action   -- the macro whose actions will be replayed in order;
       -> Task x ()      -- a Task that executes the replay.

replay address macro = 
            let append action task =
                            task `and` send address action
             in List.foldr append
                           ( Task.succeed () )
                           macro
