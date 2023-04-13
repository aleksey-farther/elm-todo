module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode


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
    | Done Int
    | ChangeMessage String


encodeToDo : ToDo -> Encode.Value
encodeToDo todo =
    Encode.object
        [ ( "id", Encode.int todo.id )
        , ( "message", Encode.string todo.message )
        , ( "done", Encode.bool todo.done )
        ]


encodeToDos : List ToDo -> String
encodeToDos todos =
    Encode.encode 0 (encodeToDo todos)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add message ->
            if message == "" then
                model

            else
                { model | todos = List.append model.todos [ { id = List.length model.todos, message = message, done = False } ], message = "" }

        Remove id ->
            { model | todos = List.filter (\m -> m.id /= id) model.todos }

        Done id ->
            { model
                | todos =
                    List.map
                        (\m ->
                            if m.id == id then
                                { m | done = True }

                            else
                                m
                        )
                        model.todos
            }

        ChangeMessage message ->
            { model | message = message }


todoItem : ToDo -> Html Msg
todoItem todo =
    div [ style "margin-top" "15px" ]
        [ div []
            [ text todo.message ]
        , div [ style "display" "flex", style "gap" "5px" ]
            [ div []
                [ text
                    (if todo.done == True then
                        "Done"

                     else
                        "Not Done"
                    )
                ]
            , button [ onClick (Done todo.id) ] [ text "Mark as done" ]
            , button [ onClick (Remove todo.id) ] [ text "Remove" ]
            ]
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
