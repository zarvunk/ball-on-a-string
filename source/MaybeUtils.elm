module MaybeUtils where

fromJust : Maybe a -> a
fromJust a = case a of Just a -> a
