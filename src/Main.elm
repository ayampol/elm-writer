module Main exposing (Model)

import Browser
import Browser.Dom
import DateFormat exposing (..)
import Element exposing (..)
import Element.Input exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (id)
import Html.Events exposing (on)
import Json.Decode as Json
import Task exposing (perform)
import Time exposing (..)



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = \model -> Browser.Document "Elm Writer" [ view model ]
        }



-- MODEL


type alias Model =
    { currentEntry : TextEntry
    , rows : Float
    , pad : Float
    }


type alias TextEntry =
    { body : String
    , date : Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (TextEntry "" (millisToPosix 0)) 100 20
    , Task.perform GetDate Time.now
    )


getScrollHeight : String -> Cmd Msg
getScrollHeight str =
    Task.attempt UpdateHeight (Browser.Dom.getViewportOf str)



-- UPDATE


type Msg
    = UpdateBody String
    | NoOp
    | UpdateHeight (Result Browser.Dom.Error Browser.Dom.Viewport)
    | GetNewHeight String String
    | GetDate Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateBody str ->
            -- We have to change the view if the textBody is too long
            ( { model
                | currentEntry = updateEntryBody model.currentEntry str
              }
            , Cmd.none
            )

        GetDate posix ->
            ( { model | currentEntry = updateEntryDay model.currentEntry posix }
            , Cmd.none
            )

        UpdateHeight scrollheight ->
            let
                a =
                    Debug.log "new scrollheight" scrollheight
            in
            ( { model | rows = parseViewportResult scrollheight }, Cmd.none )

        GetNewHeight id _ ->
            ( model, getScrollHeight id )

        NoOp ->
            ( model, Cmd.none )



-- This should actually handle the error


parseViewportResult : Result Browser.Dom.Error Browser.Dom.Viewport -> Float
parseViewportResult result =
    case result of
        Err _ ->
            10000

        Ok info ->
            info.scene.height


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
    Element.layout [] <|
        viewEditor model


viewEditor : Model -> Element Msg
viewEditor model =
    Element.column
        [ centerX
        , spacing 20
        ]
        [ Element.row [ centerX ]
            [ viewDate model.currentEntry.date ]
        , Element.Input.multiline
            [ width fill
            , htmlAttribute <| Html.Attributes.style "height" (String.fromFloat model.rows ++ "px")
            , htmlAttribute <| id "bigfield"
            ]
            { onChange = GetNewHeight "bigfield"
            , text = model.currentEntry.body
            , placeholder = Nothing
            , label = Element.Input.labelHidden "Write here"
            , spellcheck = False
            }
        ]



-- Use Browser.Dom to getviewportof the node
-- This requires using a task
-- Then using the output of that taskviewDate : Posix -> Element Msg


viewDate secs =
    Element.text (format displayDate utc secs)


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
