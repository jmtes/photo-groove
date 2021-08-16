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
import Html.Events exposing (onCheck, onClick)
import Random


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"



-- View function takes a model and returns a list of Html nodes
-- User events such as clicks are translated into message values


view : Model -> Html Msg
view model =
    let
        photos =
            model.photos

        selected =
            model.selectedUrl

        size =
            model.chosenSize
    in
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbail Size" ]
        , div
            [ id "choose-size" ]
            (List.map (viewSizeChooser size) [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToClass size) ]
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
        , onClick (ClickedPhoto url)
        ]
        []


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser selectedSize size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , id "size"
            , onCheck (\_ -> ClickedSize size)
            , checked (size == selectedSize)
            ]
            []
        , text (sizeToString size)
        ]



-- Below, Elm's compiler knows we've covered every possibility for `size`,
-- so we don't need a default branch!


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


sizeToClass : ThumbnailSize -> String
sizeToClass size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"



-- Model represents app state


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo, selectedUrl : String, chosenSize : ThumbnailSize }


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Medium
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)


type ThumbnailSize
    = Small
    | Medium
    | Large



-- Below, `Array.get` returns a `Maybe a`, which is essentially a container
-- that can hold at most one value
-- Maybes are used to represent the possible absence of a value without "null"
-- or "undefined"
-- Nothing is a Maybe *value*
-- Just is a function that *returns* a Maybe value and is an example of a
-- custom type variant, which means it can be destructured like a tuple!
-- Below we destructure Just and name its contained value `photo`
-- In English, the Just branch says the following: "This branch matches a Maybe
-- value that was created using the Just variant. Extract that value that was
-- passed to Just and name it photo"
-- This case expression also doesn't need a default branch because Nothing and
-- Just are the only ways to obtain a Maybe


getPhotoUrlAtIndex : Int -> String
getPhotoUrlAtIndex index =
    case Array.get index photoArray of
        Just photo ->
            photo.url

        Nothing ->
            ""



-- Messages are fed into the update function to produce new models
-- After an update, the new model is sent to the view function to determine
-- the new DOM


type Msg
    = ClickedPhoto String
    | ClickedSurpriseMe
    | ClickedSize ThumbnailSize


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPhoto photo ->
            { model | selectedUrl = photo }

        ClickedSurpriseMe ->
            { model | selectedUrl = "2.jpeg" }

        ClickedSize size ->
            { model | chosenSize = size }



-- Browser.sandbox wires together your state, how the page should look
-- depending on the state, and how changes can be made to the state


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, view = view, update = update }
