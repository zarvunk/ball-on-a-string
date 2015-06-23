module ListUtils where

import List exposing ( foldl, foldr )
import Maybe exposing ( Maybe(..) )

elementAt : List a -> Int -> Maybe a
elementAt list index =
    let thumb a (i,mr) =
                    (i+1, if i == index
                            then Just a
                            else mr)    
     in snd <| foldl thumb (0, Nothing) list
