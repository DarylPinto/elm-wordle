module Main exposing (main)

import Array
import Basics exposing (..)
import Browser
import Browser.Events
import Dictionary exposing (answers, guessableWords)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Process
import Random
import Set
import Task


turnLimit : Int
turnLimit =
    6


maxWordLength : Int
maxWordLength =
    5


alphabet : List Char
alphabet =
    "abcdefghijklmnopqrstuvwxyz" |> String.toList


defaultWord : String
defaultWord =
    "hello"



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


{-| Create a new list with no duplicates
-}
listUnique : List comparable -> List comparable
listUnique xs =
    xs |> Set.fromList |> Set.toList



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
                Correct ->
                    1

                Present ->
                    2

                Absent ->
                    3

                Unknown ->
                    4

        intToLetterPosition : Int -> LetterPosition
        intToLetterPosition int =
            case int of
                1 ->
                    Correct

                2 ->
                    Present

                3 ->
                    Absent

                _ ->
                    Unknown
    in
    board
        |> List.concat
        |> List.map (\( char, pos ) -> ( char, letterPositionToInt pos ))
        |> listUnique
        |> List.map (\( char, int ) -> ( char, intToLetterPosition int ))


{-| Used for conditional rendering
-}
htmlIf : Bool -> Html Msg -> Html Msg
htmlIf condition html =
    if condition then
        html

    else
        text ""



---- SUBSCRIPTIONS ----


{-| Keyboard event listener
<https://stackoverflow.com/a/53800798/7003127>
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        toKey : String -> Msg
        toKey string =
            let
                isKeyInAplhabet : Bool
                isKeyInAplhabet =
                    List.member string (alphabet |> List.map String.fromChar)
            in
            case string of
                "Enter" ->
                    Submit

                "Backspace" ->
                    Backspace

                _ ->
                    if isKeyInAplhabet then
                        case string |> String.toList |> List.head of
                            Just char ->
                                Guess char

                            Nothing ->
                                Noop

                    else
                        Noop

        keyDecoder : Decode.Decoder Msg
        keyDecoder =
            Decode.map toKey (Decode.field "key" Decode.string)
    in
    Browser.Events.onKeyDown keyDecoder



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


type GameState
    = Win
    | Loss
    | Playing



---- MODEL ----


type alias Model =
    { word : List Char
    , board : Board
    , inputBuffer : List Char
    , gameState : GameState
    , toastMessage : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        index : Random.Generator Int
        index =
            Random.int 0 ((answers |> Array.length) - 1)
    in
    ( { word = defaultWord |> String.toList
      , board = []
      , inputBuffer = []
      , gameState = Playing
      , toastMessage = Nothing
      }
    , Random.generate SetWord index
    )



---- UPDATE ----


type Msg
    = Noop
    | SetWord Int
    | Guess Char
    | Backspace
    | Submit
    | ShowToast String
    | HideToast


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        isFinalTurn =
            List.length model.board == turnLimit - 1

        isMaxTurnCountReached =
            List.length model.board >= turnLimit

        isInputBufferFull =
            List.length model.inputBuffer == maxWordLength
    in
    case msg of
        Noop ->
            ( model, Cmd.none )

        SetWord index ->
            ( { model
                | word =
                    answers
                        |> Array.get index
                        |> Maybe.withDefault defaultWord
                        |> String.toList
              }
            , Cmd.none
            )

        Guess letter ->
            case model.gameState of
                Playing ->
                    if not isInputBufferFull && not isMaxTurnCountReached then
                        ( { model | inputBuffer = List.append model.inputBuffer [ letter ] }, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Backspace ->
            ( { model
                | inputBuffer =
                    case listInit model.inputBuffer of
                        Just chars ->
                            chars

                        Nothing ->
                            []
              }
            , Cmd.none
            )

        ShowToast message ->
            ( { model | toastMessage = Just message }
            , Task.perform (\_ -> HideToast) (Process.sleep 2000)
            )

        HideToast ->
            ( { model | toastMessage = Nothing }, Cmd.none )

        Submit ->
            let
                newRow =
                    rowFromGuess model.word model.inputBuffer

                newGameState =
                    if model.inputBuffer == model.word then
                        Win

                    else if isFinalTurn then
                        Loss

                    else
                        Playing

                isInputBufferAGuessableWord =
                    List.member (String.fromList model.inputBuffer) guessableWords
            in
            if isInputBufferFull && not isMaxTurnCountReached && isInputBufferAGuessableWord then
                ( { model
                    | board = List.append model.board [ newRow ]
                    , inputBuffer = []
                    , gameState = newGameState
                  }
                , Cmd.none
                )

            else if isInputBufferFull && not isInputBufferAGuessableWord then
                update (ShowToast "Not in word list") model

            else
                ( model, Cmd.none )



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
                            let
                                -- if the character in this tile is empty (' '),
                                -- then don't put any children within the tile
                                -- HTML. This allows us to style empty tiles with
                                -- CSS very easily
                                innerText : List (Html Msg)
                                innerText =
                                    if (tile |> Tuple.first) == ' ' then
                                        []

                                    else
                                        [ text (tile |> Tuple.first |> String.fromChar) ]
                            in
                            div
                                [ class (tile |> Tuple.second |> letterPosToClassName |> String.append "tile ") ]
                                innerText
                        )
                )

        remainingTurnCount : Int
        remainingTurnCount =
            turnLimit - List.length model.board

        -- Current guess represented as a row of tiles
        inputBufferTileRow : Html Msg
        inputBufferTileRow =
            rowFromGuess (List.repeat maxWordLength ' ') model.inputBuffer
                |> List.map (\t -> ( Tuple.first t, Unknown ))
                |> rowToHtml
                |> htmlIf (remainingTurnCount > 0)

        -- Rows of empty tiles representing remaining turns
        remainingRows : List (Html Msg)
        remainingRows =
            div [ class "row" ] (List.repeat maxWordLength (div [ class "tile" ] []))
                |> List.repeat (remainingTurnCount - 1)

        gameStateText : Html Msg
        gameStateText =
            case model.gameState of
                Win ->
                    h2 [] [ text "Congratulations!" ]

                Loss ->
                    div []
                        [ h2 [] [ text "Maybe next time" ]
                        , p [ class "loss-text" ] [ text ("The word was \"" ++ String.fromList model.word ++ "\"") ]
                        ]

                _ ->
                    text ""

        toastContents : List (Html Msg)
        toastContents =
            case model.toastMessage of
                Just message ->
                    [ text message ]

                Nothing ->
                    []
    in
    div [ class "game" ]
        [ header [] [ h1 [] [ text "elm wordle" ] ]
        , div [ class "board" ]
            (remainingRows
                |> List.append [ inputBufferTileRow ]
                |> List.append (List.map rowToHtml model.board)
            )
        , htmlIf (model.gameState /= Playing) gameStateText
        , div [ class "keyboard" ]
            [ div [] (buttonList "qwertyuiop")
            , div [] (buttonList "asdfghjkl")
            , div []
                (List.concat
                    [ [ button [ onClick Submit, class "enter" ] [ text "Enter" ] ]
                    , buttonList "zxcvbnm"
                    , [ button [ onClick Backspace, class "backspace" ] [ text "⌫" ] ]
                    ]
                )
            ]
        , div [ class "toast" ] toastContents
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
