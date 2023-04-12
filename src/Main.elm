module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias ToDo =
    { id : Int
    , message : String
    , done : Bool
    }


type alias Model =
    { message : String
    , todos : List ToDo
    }


init : Model
init =
    { message = ""
    , todos = []
    }


type Msg
    = Add String
    | Remove Int
    | ChangeMessage String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add message ->
            if message == "" then
                model

            else
                { model | todos = List.append model.todos [ { id = List.length model.todos, message = message, done = False } ] }

        Remove id ->
            { model | todos = List.filter (\m -> m.id /= id) model.todos }

        ChangeMessage message ->
            { model | message = message }


todoItem : ToDo -> Html Msg
todoItem todo =
    div []
        [ div []
            [ text todo.message ]
        , div []
            [ text
                (if todo.done == True then
                    "Done"

                 else
                    "Not Done"
                )
            ]
        , button [ onClick (Remove todo.id) ] [ text "Remove" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Todo Message", onInput ChangeMessage, value model.message ] []
        , button [ onClick (Add model.message) ]
            [ text "Add" ]
        , div []
            (List.map todoItem model.todos)
        ]
