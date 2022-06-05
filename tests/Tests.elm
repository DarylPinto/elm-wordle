module Tests exposing (..)

import Expect
import Main exposing (..)
import Test exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "rowFrowGuess"
        [ test "works for correct guesses" <|
            \_ ->
                let
                    guess : List Char
                    guess =
                        "hello" |> String.toList

                    word : List Char
                    word =
                        "hello" |> String.toList
                in
                Expect.equal (rowFromGuess word guess)
                    [ ( 'h', Correct )
                    , ( 'e', Correct )
                    , ( 'l', Correct )
                    , ( 'l', Correct )
                    , ( 'o', Correct )
                    ]
        , test "only shows one 'present' tile if guess has repeated letters" <|
            \_ ->
                let
                    guess : List Char
                    guess =
                        "elite" |> String.toList

                    word : List Char
                    word =
                        "began" |> String.toList
                in
                Expect.equal (rowFromGuess word guess)
                    [ ( 'e', Present )
                    , ( 'l', Absent )
                    , ( 'i', Absent )
                    , ( 't', Absent )
                    , ( 'e', Absent )
                    ]
        , test "shows two 'present' tiles if both the guess and the word have repeated letters" <|
            \_ ->
                let
                    guess : List Char
                    guess =
                        "oomla" |> String.toList

                    word : List Char
                    word =
                        "igloo" |> String.toList
                in
                Expect.equal (rowFromGuess word guess)
                    [ ( 'o', Present )
                    , ( 'o', Present )
                    , ( 'm', Absent )
                    , ( 'l', Present )
                    , ( 'a', Absent )
                    ]
        , test "shows one 'correct' tile and one 'present' for repeated letters in guess where applicable" <|
            \_ ->
                let
                    guess : List Char
                    guess =
                        "aisha" |> String.toList

                    word : List Char
                    word =
                        "atlas" |> String.toList
                in
                Expect.equal (rowFromGuess word guess)
                    [ ( 'a', Correct )
                    , ( 'i', Absent )
                    , ( 's', Present )
                    , ( 'h', Absent )
                    , ( 'a', Present )
                    ]
        , test "doesn't show any 'present' tiles for repeated letters in guess if they're already correct" <|
            \_ ->
                let
                    guess : List Char
                    guess =
                        "momos" |> String.toList

                    word : List Char
                    word =
                        "minor" |> String.toList
                in
                Expect.equal (rowFromGuess word guess)
                    [ ( 'm', Correct )
                    , ( 'o', Absent )
                    , ( 'm', Absent )
                    , ( 'o', Correct )
                    , ( 's', Absent )
                    ]
        ]
