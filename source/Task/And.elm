module Task.And ( and ) where

{-| A handy function for sequencing Tasks.
@docs and
-}

import Task exposing ( Task, andThen )

{-| a utility function that simply sequences two Tasks,
where the second Task does not depend on the first.
-}
and : Task x () -> Task x a -> Task x a
and task1 task2 = task1 `andThen` \ () -> task2
