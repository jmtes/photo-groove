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

import Html exposing (..)
import Html.Attributes exposing (..)


urlPrefix =
    "http://elm-in-action.com/"


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


viewThumbnail selectedUrl thumb =
    let
        url =
            thumb.url
    in
    img
        [ src (urlPrefix ++ url)
        , classList [ ( "selected", url == selectedUrl ) ]
        ]
        []


initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


update msg model =
    if msg.description == "ClickedPhoto" then
        { model | selectedUrl = msg.data }

    else
        model


main =
    view initialModel
