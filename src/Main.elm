module Main exposing (..)

import Browser
import Css exposing (auto, displayFlex, margin, padding, px, vw, width)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (name, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, field, list, string)
import List exposing (filter, sort)
import Random
import Random.List



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias GameState =
    { words : List String
    , enteredWords : List String
    , magicWord : String
    , guess : String
    }


type Model
    = Loaded GameState
    | Loading
    | Error
    | Won GameState


defaultModel : Model
defaultModel =
    Loaded (GameState [] [] "" "")


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getWords )



-- UPDATE


type Msg
    = GotWords (Result Http.Error (List String))
    | GotMagicWord ( Maybe String, List String )
    | Guess String
    | CheckGuess
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWords result ->
            case result of
                Ok words ->
                    ( Loaded (GameState (List.map String.toLower words) [] "" ""), randomWord words )

                Err _ ->
                    ( defaultModel, Cmd.none )

        GotMagicWord ( mWord, _ ) ->
            case mWord of
                Just word ->
                    case model of
                        Loaded oldState ->
                            ( Loaded { oldState | magicWord = word }, Cmd.none )

                        Won oldState ->
                            ( Won oldState, Cmd.none )

                        _ ->
                            ( defaultModel, Cmd.none )

                Nothing ->
                    ( defaultModel, Cmd.none )

        Guess g ->
            case model of
                Loaded oldState ->
                    ( Loaded { oldState | guess = g }, Cmd.none )

                Won oldState ->
                    ( Won oldState, Cmd.none )

                _ ->
                    ( defaultModel, Cmd.none )

        CheckGuess ->
            case model of
                Loaded oldState ->
                    if String.toLower oldState.magicWord == String.toLower oldState.guess then
                        ( Won oldState, Cmd.none )

                    else if List.member (String.toLower oldState.guess) oldState.words && not (List.member (String.toLower oldState.guess) oldState.enteredWords) then
                        ( Loaded { oldState | enteredWords = oldState.enteredWords ++ [ String.toLower oldState.guess ], guess = "" }, Cmd.none )

                    else
                        ( Loaded oldState, Cmd.none )

                Won oldState ->
                    ( Won oldState, Cmd.none )

                _ ->
                    ( defaultModel, Cmd.none )

        Reset ->
            case model of
                Loaded oldState ->
                    ( Loaded oldState, Cmd.none )

                Won oldState ->
                    ( Loaded { oldState | guess = "" }, randomWord oldState.words )

                _ ->
                    ( defaultModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


container : List (Attribute msg) -> List (Html msg) -> Html msg
container =
    styled div
        [ margin auto
        , padding (px 25)
        ]


app : List (Attribute msg) -> List (Html msg) -> Html msg
app =
    styled div
        [ width (vw 100)
        , displayFlex
        ]


renderGuess : String -> Html msg
renderGuess w =
    p [] [ text w ]


view : Model -> Html.Html Msg
view model =
    let
        body =
            case model of
                Loaded state ->
                    app []
                        [ container []
                            [ h1 [] [ text "Guess the word" ]
                            , div [] (List.map renderGuess (beforeWords state))
                            , form [ name "guessword", onSubmit CheckGuess ]
                                [ input [ onInput Guess, value state.guess ] []
                                , input [ type_ "submit" ] []
                                ]
                            , div [] (List.map renderGuess (afterWords state))
                            , button [ onClick Reset ] [ text "Restart" ]

                            -- , p []  [text state.magicWord]
                            ]
                        ]

                Loading ->
                    app []
                        [ container [] [ h2 [] [ text "Loading" ] ] ]

                Error ->
                    app []
                        [ container [] [ h2 [] [ text "Error Loading! Try refreshing the page" ] ] ]

                Won _ ->
                    app []
                        [ container []
                            [ div []
                                [ h2 [] [ text "You Win!" ]
                                , img [ src "https://source.unsplash.com/random", Html.Styled.Attributes.width 300, Html.Styled.Attributes.height 300 ] []
                                , div [] [ button [ onClick Reset ] [ text "Restart" ] ]
                                ]
                            ]
                        ]
    in
    toUnstyled body



--- HELPERS


wordsDecoder : Decoder (List String)
wordsDecoder =
    field "all_words" (list string)


beforeWords : GameState -> List String
beforeWords m =
    filter (\n -> String.toLower n < String.toLower m.magicWord) m.enteredWords
        |> sort


afterWords : GameState -> List String
afterWords m =
    filter (\n -> String.toLower n > String.toLower m.magicWord) m.enteredWords
        |> sort


getWords : Cmd Msg
getWords =
    Http.get
        { url = "/src/words.json"
        , expect = Http.expectJson GotWords wordsDecoder
        }


randomWord : List String -> Cmd Msg
randomWord words =
    Random.generate GotMagicWord (Random.List.choose words)
