module Main exposing (main)

import Basics exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Set



-- Constants


turnLimit : number
turnLimit =
    6


maxWordLength : number
maxWordLength =
    5



-- Extra list functions


{-| Get the first element that satisfies the test
-}
listFind : (a -> Bool) -> List a -> Maybe a
listFind isNeedle haystack =
    haystack |> List.filter isNeedle |> List.head


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
    let
        missingLetterLength =
            maxWordLength - List.length guess

        emptyTiles =
            List.repeat missingLetterLength ( ' ', Absent )

        guessLetterTiles =
            listZip word guess
                |> List.map
                    (\( wordLetter, guessLetter ) ->
                        if guessLetter == wordLetter then
                            ( guessLetter, Correct )

                        else if List.member guessLetter word then
                            ( guessLetter, Present )

                        else
                            ( guessLetter, Absent )
                    )
    in
    List.append guessLetterTiles emptyTiles


{-| Convert the entire board into a single list of unique tiles
-}
toUniqueTiles : Board -> List Tile
toUniqueTiles board =
    let
        letterPositionToInt : LetterPosition -> Int
        letterPositionToInt pos =
            case pos of
                Present ->
                    1

                Correct ->
                    2

                Absent ->
                    3

                Unknown ->
                    4

        intToLetterPosition : Int -> LetterPosition
        intToLetterPosition int =
            case int of
                1 ->
                    Present

                2 ->
                    Correct

                3 ->
                    Absent

                _ ->
                    Unknown
    in
    board
        |> List.concat
        |> List.map (\( char, pos ) -> ( char, letterPositionToInt pos ))
        |> Set.fromList
        |> Set.toList
        |> List.map (\( char, int ) -> ( char, intToLetterPosition int ))



---- TYPES ----


type LetterPosition
    = Correct
    | Present
    | Absent
    | Unknown


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
    { word = "today" |> String.toList
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
        -- Convert LetterPosition to CSS color
        letterPosToClassName : LetterPosition -> String
        letterPosToClassName pos =
            case pos of
                Correct ->
                    "correct"

                Present ->
                    "present"

                Absent ->
                    "absent"

                Unknown ->
                    ""

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

                            tileClassName =
                                case tileForThisLetter of
                                    Just tile ->
                                        tile |> Tuple.second |> letterPosToClassName

                                    Nothing ->
                                        ""
                        in
                        button
                            [ onClick (Guess letter), class tileClassName ]
                            [ text (String.fromChar letter) ]
                    )

        -- Submitted guesses represented as rows of tiles
        rowToHtml : Row -> Html Msg
        rowToHtml row =
            div [ class "row" ]
                (row
                    |> List.map
                        (\tile ->
                            div
                                [ class (tile |> Tuple.second |> letterPosToClassName |> String.append "tile ") ]
                                [ text (tile |> Tuple.first |> String.fromChar) ]
                        )
                )

        -- Current guess represented as a row of tiles
        inputBufferTileRow : Html Msg
        inputBufferTileRow =
            rowFromGuess (List.repeat maxWordLength ' ') model.inputBuffer
                |> List.map (\t -> ( Tuple.first t, Unknown ))
                |> rowToHtml

        -- Rows of empty tiles representing remaining turns
        remainingRows : List (Html Msg)
        remainingRows =
            let
                remainingTurnCount : Int
                remainingTurnCount =
                    turnLimit - List.length model.board

                emptyTile : Html Msg
                emptyTile =
                    div [ class "tile" ] [ text " " ]
            in
            div [ class "row" ] (List.repeat maxWordLength emptyTile)
                |> List.repeat (remainingTurnCount - 1)
    in
    div [ class "game" ]
        [ header [] [ h1 [] [ text "Wordle" ] ]
        , div [ class "board" ]
            (remainingRows
                |> List.append [ inputBufferTileRow ]
                |> List.append (List.map rowToHtml model.board)
            )
        , div [ class "keyboard" ]
            [ div [] (buttonList "qwertyuiop")
            , div [] (buttonList "asdfghjkl")
            , div []
                (List.concat
                    [ [ button [ onClick Submit ] [ text "Enter" ] ]
                    , buttonList "zxcvbnm"
                    , [ button [ onClick Backspace, class "backspace" ] [ text "⌫" ] ]
                    ]
                )
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
