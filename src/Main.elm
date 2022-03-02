module Main exposing (main)

import Basics exposing (..)
import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Set exposing (Set)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- Data Model (State)


type alias Model =
    { word : String
    , guesses : Set Char
    , inputBuffer : Set Char
    }


init : Model
init =
    { word = "daryl"
    , guesses = Set.empty
    , inputBuffer = Set.empty
    }



-- Messages (like redux actions)


type Msg
    = Guess Char
    | Backspace
    | Submit



-- Update (like redux reducers)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Guess letter ->
            { model | inputBuffer = Set.insert letter model.inputBuffer }

        Backspace ->
            model

        Submit ->
            { model | inputBuffer = Set.empty }



-- View (render function)


view : Model -> Html Msg
view model =
    let
        shownInputBuffer =
            model.inputBuffer
                |> Set.toList
                |> List.map String.fromChar
                |> String.concat

        -- List of 'letter' buttons to guess with
        buttonList : String -> List (Html Msg)
        buttonList str =
            str
                |> String.toList
                |> List.map
                    (\letter ->
                        button [ onClick (Guess letter) ] [ text (String.fromChar letter) ]
                    )
    in
    div []
        [ div [] (buttonList "qwertyuiop")
        , div [] (buttonList "asdfghjkl")
        , div [] (buttonList "zxcvbnm")
        , p [] [ text shownInputBuffer ]
        ]
