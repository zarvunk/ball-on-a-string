module Tuple where

mapBoth : (a -> a') -> (b -> b') -> (a, b) -> (a', b')
mapBoth f g (a, b) = (f a, g b)

mapBoth' : (a -> a') -> (a, a) -> (a', a')
mapBoth' f = mapBoth f f

mapLeft : (a -> a') -> (a, b) -> (a', b)
mapLeft f = mapBoth f identity

mapRight : (b -> b') -> (a, b) -> (a, b')
mapRight g = mapBoth identity g
