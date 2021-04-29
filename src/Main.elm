module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)



-- elements

container : List (Attribute msg) -> List (Html msg) -> Html msg
container =
    styled div
        [ margin auto
        , padding (px 25)
        ]


app =
    styled div
        [ width (vw 100)
        , displayFlex
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )

        Decrement ->
            ( model - 1, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        body =
            app []
                [ container []
                    [ button [ onClick Decrement ] [ text "-" ]
                    , div [] [ text (String.fromInt model) ]
                    , button [ onClick Increment ] [ text "+" ]
                    ]
                ]
    in
    { body = [ toUnstyled body ]
    , title = "Guess The Word"
    }
