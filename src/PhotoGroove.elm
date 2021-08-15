-- Declare a new module
-- Now other modules in the project can import this one like so:
-- import PhotoGroove exposing (main)
-- We'e only exposing `main` for use in other modules
-- This means other modules can't import `model` from here, for example
-- In general, it's best to expose as little as possible!


module PhotoGroove exposing (main)

-- Import other modules
-- We're exposing *all* exports from `Html` into the global scope
-- This is how we can call functions like `div` without needing to prepend
-- it with `Html.`
-- In this case, exposing everything from Html and Html.Atributes is fine,
-- but it's best to try to be as specific as possible with your imports to
-- prevent name ambiguity and confusion

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"



-- View function takes a model and returns a list of Html nodes
-- User events such as clicks are translated into message values


view : { photos : List Photo, selectedUrl : String } -> Html Msg
view model =
    let
        photos =
            model.photos

        selected =
            model.selectedUrl
    in
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map (viewThumbnail selected) photos)
        , img [ class "large", src (urlPrefix ++ "large/" ++ selected) ] []
        ]



-- When the user clicks a photo, a record with fields `description`
-- and `data` are sent to the update function.


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    let
        url =
            thumb.url
    in
    img
        [ src (urlPrefix ++ url)
        , classList [ ( "selected", url == selectedUrl ) ]
        , onClick { description = "ClickedPhoto", data = url }
        ]
        []



-- Model represents app state


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo, selectedUrl : String }


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos



-- Messages are fed into the update function to produce new models
-- After an update, the new model is sent to the view function to determine
-- the new DOM


type alias Msg =
    { description : String, data : String }


update : Msg -> Model -> Model
update msg model =
    if msg.description == "ClickedPhoto" then
        { model | selectedUrl = msg.data }

    else
        model



-- Browser.sandbox wires together your state, how the page should look
-- depending on the state, and how changes can be made to the state


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, view = view, update = update }
