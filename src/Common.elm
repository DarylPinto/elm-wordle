module Common exposing (..)

{-| Returns true if `word` has any letters in the same position as "PINTO"
-}
isPintoWord : String -> Bool
isPintoWord word =
    let
        pairs =
            List.map2 Tuple.pair (String.toList "pinto") (String.toList word)
    in
    List.any (\( pintoLetter, wordLetter ) -> pintoLetter == wordLetter) pairs
