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
        status =
            model.status

        size =
            model.chosenSize
    in
    div [ class "content" ]
        (case status of
            Loaded photos selected ->
                viewLoaded photos selected size

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]
        )


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selected size =
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


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status, chosenSize : ThumbnailSize }


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos



-- The type annotation below essentially means "a random generator that
-- returns ints"


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
-- Msg is a custom type with four variants
-- All of the variants are containers that hold different types of values
-- Essentially, they're functions that return instances of Msg
-- For example, ClickedPhoto is of type `String -> Msg`
-- That is, except for ClickedSurpriseMe
-- It simply *is* an instance of Msg


type Msg
    = ClickedPhoto String
    | ClickedSurpriseMe
    | ClickedSize ThumbnailSize
    | GotSelectedIndex Int



-- Below, Random.generate is a Cmd
-- It takes as its first arg the Msg to pass to update
-- It takes as its second arg a function that returns the value to pass to
-- the Msg (in this case the Int supplied to GotSelectedIndex) given that the
-- Msg should hold a value (see comments right above)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto photo ->
            ( { model | selectedUrl = photo }, Cmd.none )

        ClickedSurpriseMe ->
            ( model, Random.generate GotSelectedIndex randomPhotoPicker )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotSelectedIndex index ->
            ( { model | selectedUrl = getPhotoUrlAtIndex index }, Cmd.none )



-- Browser.element wires together your state, how the page should look
-- depending on the state, and how changes can be made to the state
-- The init function takes flags as an arg and returns a tuple containing
-- 1) the initial state and 2) a command to run when the app loads
-- Since we don't need to do anything when the app loads, we passed Cmd.none
-- The () in the type annotation is called a unit
-- It contains no info whatsoever, but it's both a value and a type
-- The () type can only be satisfied witht the () value
-- As such, a function that takes *only* () only accepts one input and
-- only returns one output
-- Below, the () represents our flags type, which we aren't using right now
-- Using () for our flags type indicates that we don't accept any flags!
-- The `Model` means the type of our model is our custom type `Model`
-- The `Msg` means that the type of message our update and view functions will
-- use is our custom type `Msg`
-- Putting all these together, we can  read the below type annotation as "an
-- Elm program with no flags, whose model type is Model and whose message type
-- is Msg"
-- It's kinda like making an arrow function that takes no args in JS?
-- () => {...}


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
