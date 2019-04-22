module Main exposing (Model)

import Browser
import DateFormat exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, at, field, int, map2, string)
import Task exposing (perform)
import Time exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { currentEntry : TextEntry
    , rows : Int
    , scratch : Int
    }


type alias TextEntry =
    { body : String
    , date : Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (TextEntry "" (millisToPosix 0)) 10 0
    , Task.perform GetDate Time.now
    )



-- UPDATE


type Msg
    = UpdateBody String Int
    | NoOp
    | GetDate Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateBody str newRows ->
            -- We have to change the view if the textBody is too long
            ( { model
                | rows = resizeRows newRows
                , scratch = newRows
                , currentEntry = updateEntryBody model.currentEntry str
              }
            , Cmd.none
            )

        GetDate posix ->
            ( { model | currentEntry = updateEntryDay model.currentEntry posix }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


updateEntryBody : TextEntry -> String -> TextEntry
updateEntryBody entry str =
    { entry | body = str }


updateEntryDay : TextEntry -> Posix -> TextEntry
updateEntryDay entry secs =
    { entry | date = secs }



-- This should use some configurable settings or something - not magic numbers


resizeRows : Int -> Int
resizeRows scrollHeight =
    ((toFloat scrollHeight - 1 * 10) / 15)
        |> ceiling
        |> clamp 10 1000



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ p [] [ viewDate model.currentEntry.date ]
        , textarea
            [ class "main-text-input"
            , cols 50
            , rows model.rows
            , value model.currentEntry.body
            , on "input" inputDecoder
            , autofocus True
            ]
            []
        ]


inputDecoder : Decoder Msg
inputDecoder =
    Json.Decode.map2 UpdateBody
        (at [ "target", "value" ] string)
        (at [ "target", "scrollHeight" ] int)


viewDate : Posix -> Html Msg
viewDate secs =
    Html.text (format displayDate utc secs)


displayDate : List Token
displayDate =
    [ dayOfMonthNumber
    , DateFormat.text "/"
    , monthNumber
    , DateFormat.text "/"
    , yearNumberLastTwo
    ]



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
