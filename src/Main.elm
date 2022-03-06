module Main exposing (main)

import Basics exposing (..)
import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Set



-- Constants


turnLimit : number
turnLimit =
    5


maxWordLength : number
maxWordLength =
    5



-- Extra list functions


{-| Get the first element that satisfies the test
-}
listFind : (a -> Bool) -> List a -> Maybe a
listFind isNeedle haystack =
    let
        filtered =
            haystack |> List.filter isNeedle
    in
    case filtered of
        [] ->
            Nothing

        _ ->
            List.head filtered


{-| Get all elements in a list except for the last
-}
listInit : List a -> Maybe (List a)
listInit xs =
    case xs of
        [] ->
            Nothing

        _ ->
            Just (xs |> List.take (List.length xs - 1))


{-| Combine two lists into a single list of tuples
-}
listZip : List a -> List b -> List ( a, b )
listZip xs ys =
    List.map2 Tuple.pair xs ys



-- Utility functions


{-| Given a word and a guess, output a row of tiles representing the guess's accuracy
-}
rowFromGuess : List Char -> List Char -> Row
rowFromGuess word guess =
    listZip word guess
        |> List.map
            (\( wordLetter, guessLetter ) ->
                if guessLetter == wordLetter then
                    ( guessLetter, Green )

                else if List.member guessLetter word then
                    ( guessLetter, Gold )

                else
                    ( guessLetter, Black )
            )


{-| Convert the entire board into a single list of unique tiles
-}
toUniqueTiles : Board -> List Tile
toUniqueTiles board =
    let
        letterPositionToInt : LetterPosition -> Int
        letterPositionToInt pos =
            case pos of
                Gold ->
                    1

                Green ->
                    2

                Black ->
                    0

        intToLetterPosition : Int -> LetterPosition
        intToLetterPosition int =
            case int of
                1 ->
                    Gold

                2 ->
                    Green

                _ ->
                    Black
    in
    board
        |> List.concat
        |> List.map (\( char, pos ) -> ( char, letterPositionToInt pos ))
        |> Set.fromList
        |> Set.toList
        |> List.map (\( char, int ) -> ( char, intToLetterPosition int ))



---- TYPES ----


type LetterPosition
    = Green
    | Gold
    | Black


type alias Tile =
    ( Char, LetterPosition )


type alias Row =
    List Tile


type alias Board =
    List Row



---- MODEL ----


type alias Model =
    { word : List Char
    , board : Board
    , inputBuffer : List Char
    }


init : Model
init =
    { word = "daryl" |> String.toList
    , board = []
    , inputBuffer = []
    }



---- UPDATE ----


type Msg
    = Guess Char
    | Backspace
    | Submit


update : Msg -> Model -> Model
update msg model =
    let
        isMaxTurnCountReached =
            List.length model.board >= turnLimit

        isInputBufferFull =
            List.length model.inputBuffer == maxWordLength
    in
    case msg of
        Guess letter ->
            if not isInputBufferFull && not isMaxTurnCountReached then
                { model | inputBuffer = List.append model.inputBuffer [ letter ] }

            else
                model

        Backspace ->
            { model
                | inputBuffer =
                    case listInit model.inputBuffer of
                        Just chars ->
                            chars

                        Nothing ->
                            []
            }

        Submit ->
            let
                newRow =
                    rowFromGuess model.word model.inputBuffer
            in
            if isInputBufferFull && not isMaxTurnCountReached then
                { model
                    | board = List.append model.board [ newRow ]
                    , inputBuffer = []
                }

            else
                model



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        shownInputBuffer =
            model.inputBuffer
                |> List.map String.fromChar
                |> String.concat

        -- Convert LetterPosition to CSS color
        letterPosToColor : LetterPosition -> String
        letterPosToColor pos =
            case pos of
                Green ->
                    "green"

                Gold ->
                    "yellow"

                Black ->
                    "gray"

        -- List of 'letter' buttons to guess with
        buttonList : String -> List (Html Msg)
        buttonList str =
            let
                uniqueTiles =
                    model.board |> toUniqueTiles
            in
            str
                |> String.toList
                |> List.map
                    (\letter ->
                        let
                            tileForThisLetter =
                                uniqueTiles
                                    |> listFind (\t -> (t |> Tuple.first) == letter)

                            bgColor =
                                case tileForThisLetter of
                                    Just tile ->
                                        tile
                                            |> Tuple.second
                                            |> letterPosToColor

                                    Nothing ->
                                        "white"
                        in
                        button
                            [ onClick (Guess letter), style "background-color" bgColor ]
                            [ text (String.fromChar letter) ]
                    )

        -- Row of Tiles HTML
        shownRow : Row -> Html Msg
        shownRow row =
            tr []
                (row
                    |> List.map
                        (\tile ->
                            td
                                [ style "background-color" (tile |> Tuple.second |> letterPosToColor) ]
                                [ text (tile |> Tuple.first |> String.fromChar) ]
                        )
                )
    in
    div []
        [ table [] (model.board |> List.map shownRow)
        , div [] (buttonList "qwertyuiop")
        , div [] (buttonList "asdfghjkl")
        , div [] (buttonList "zxcvbnm")
        , p [] [ text shownInputBuffer ]
        , button [ onClick Backspace ] [ text "←" ]
        , button [ onClick Submit ] [ text "Submit" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
