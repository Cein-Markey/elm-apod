module Main exposing (..)

import Html exposing (div, h1, img, label, text, program)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Http exposing (..)
import Json.Decode as Decode


type alias Model =
    { response : Response
    , error : Maybe String
    }


type alias Response =
    { title : String
    , hdurl : String
    , explanation : String
    }


type Msg
    = LoadPhoto (Result Http.Error Response)


initialModel : Model
initialModel =
    { response =
        { title = ""
        , hdurl = ""
        , explanation = ""
        }
    , error = Nothing
    }


getPhoto : Cmd Msg
getPhoto =
    let
        url =
            "https://api.nasa.gov/planetary/apod?api_key=PD22f7nhvJTDOh1IvhfWPxtXj1D0AMiM4Zr65uqj"

        request =
            Http.get url decodePhotoUrl
    in
        Http.send LoadPhoto request


decodePhotoUrl : Decode.Decoder Response
decodePhotoUrl =
    Decode.map3 Response
        (Decode.field "title" Decode.string)
        (Decode.field "hdurl" Decode.string)
        (Decode.field "explanation" Decode.string)


styleConstraints : List ( String, String )
styleConstraints =
    [ ( "max-width", "800px" ), ( "width", "100%" ), ( "display", "block" ) ]


renderError : Model -> Html.Html Msg
renderError model =
    case model.error of
        Nothing ->
            text ""

        Just err ->
            label [] [ text err ]


renderTitle : Model -> Html.Html Msg
renderTitle model =
    h1 [ style styleConstraints ] [ text model.response.title ]


renderExplanation : Model -> Html.Html Msg
renderExplanation model =
    label [ style styleConstraints ] [ text model.response.explanation ]


renderImage : Model -> Html.Html Msg
renderImage model =
    img [ style styleConstraints, src model.response.hdurl ] []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadPhoto (Ok data) ->
            let
                { title, hdurl, explanation } =
                    data

                oldResponse =
                    model.response

                newResponse =
                    { oldResponse
                        | title = title
                        , hdurl = hdurl
                        , explanation = explanation
                    }
            in
                ( { model | response = newResponse }, Cmd.none )

        LoadPhoto (Err error) ->
            ( { model | error = Just (toString error) }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    div []
        [ renderTitle model
        , renderExplanation model
        , renderImage model
        , renderError model
        ]


main =
    program
        { init = ( initialModel, getPhoto )
        , update = update
        , view = view
        , subscriptions = \model -> Sub.none
        }
