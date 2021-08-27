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

import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onCheck, onClick)
import Http
import Json.Decode as Decode exposing (Decoder, at, int, string, succeed)
import Json.Decode.Pipeline as DecodePipeline exposing (optional)
import Json.Encode as Encode
import Random


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"



-- View function takes a model and returns a list of Html nodes
-- User events such as clicks are translated into message values
-- For reference, the <| acts like an opening parenthesis except it doesn't
-- need a closer
-- Both these lines do the same thing:
-- String.toUpper (String.reverse "hello")
-- String.toUpper <| String.reverse "hello"
-- It's just that the latter looks a lot nicer


view : Model -> Html Msg
view model =
    let
        status =
            model.status

        size =
            model.chosenSize
    in
    div [ class "content" ] <|
        case status of
            Loaded photos selected ->
                viewLoaded photos selected size

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selected size =
    [ h1 [] [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ class "filters" ]
        [ viewFilter "Hue" 0
        , viewFilter "Ripple" 0
        , viewFilter "Noise" 0
        ]
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

        photoTitle =
            thumb.title

        size =
            thumb.size
    in
    img
        [ src (urlPrefix ++ url)
        , title (photoTitle ++ " [" ++ String.fromInt size ++ " kb]")
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



-- Contrary to what the name suggests, we're not actually using Encode to
-- create a JSON string
-- Rather, we're encoding a JS Value for the Html.Attributes.property function
-- This sets a JS *property* on this <range-slider> node so that our custom
-- element can read it later
-- The property will be named `val` and it will be set to the value of
-- `magnitude`
-- Encode.int specifies what the property's type will be on the JS side,
-- in this case an int
-- If we had used Encode.string (String.fromInt magnitude) instead, the
-- property would have been set to a string on the JS side
-- Note: Elm applies special handling to the `value` property, so it's best
-- not to use that name for custom properties


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
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
    { url : String
    , size : Int
    , title : String
    }



-- Below, `succeed Photo` begins the pipeline
-- It says our decoder will decode the arguments to Photo one by one
-- Ultimately the whole decoder will succeed unless any steps in the pipeline
-- fails
-- Because Photo accepts 3 arguments, we'll need 3 pipeline steps or else the
-- compiler will give us an error
-- `required "url" string` says that what we're decoding has to be JSON object
-- with a field "url" containing a string
-- Decoding could fail at this step either because the "url" field is missing
-- or it's present but not a string value
-- `optional "title" string "Untitled"` says that if the "title" field is
-- missing or null, we'll default to it being "Untitled" when we build the
-- Photo
-- The decoder type returned from each step in the pipeline must match the
-- corresponding arg in the succeed function
-- In this example, because Photo takes a String as its first arg, the first
-- step in the pipeline has to return a decoder containing a String


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> DecodePipeline.required "url" string
        |> DecodePipeline.required "size" int
        |> optional "title" string "Untitled"



-- Our data model now represents three distinct states
-- Since we store `photos` and `selectedUrl` inside the Loaded variant, there
-- is no way to access those values from the Loading or Errored states
-- This prevents "impossible states"!


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


type ThumbnailSize
    = Small
    | Medium
    | Large



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
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))



-- Below, Random.generate is a Cmd
-- It takes as its first arg the Msg to pass to update
-- It takes as its second arg a function that returns the value to pass to
-- the Msg (in this case the Int supplied to GotSelectedIndex) given that the
-- Msg should hold a value (see comments right above)
-- In the (firstPhoto :: otherPhotos) expression below, we're essentialy
-- saying "Give me the first photo and then a list of the other photos"
-- So the type of firstPhoto would be Photo, while that of otherPhotos would
-- be List Photo
-- Whenever you encounter a function like Random.uniform that takes a non
-- empty list, using the :: pattern in a case expression is often a good way
-- to obtain the arguments it needs
-- When using pipelines, the order of the functions from top-down is pretty
-- much the reverse of what it'd be with parentheses
-- In the GotPhotos branch, what `(firstUrl :: _) as urls` means is:
-- "Give the name `urls` to the entire List, while also subdividing it into
-- its first element, which we will name `firstUrl`, and its remaining
-- elements, which we will decline to name because we won't use them"
-- `as` can also be used to destructure function arguments, i.e:
-- doSomethingWithTuple ((first, second) as tuple) = ...
-- doSomethingWithRecord ({username, password} as user) = ...


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored _ ->
                    ( model, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }
            , Cmd.none
            )

        GotPhotos (Ok photos) ->
            case photos of
                first :: rest ->
                    ( { model | status = Loaded photos first.url }
                    , Cmd.none
                    )

                [] ->
                    ( { model | status = Errored "No photos found" }
                    , Cmd.none
                    )

        GotPhotos (Err httpError) ->
            ( { model | status = Errored "Server error" }, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored _ ->
            status


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (Decode.list photoDecoder)
        }



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
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children



-- detailUserSlidTo decodes the integer located at event.detail.userSlidTo
-- msgDecoder converts that integer to a message using toMsg


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    at [ "detail", "userSlidTo" ] int
        |> Decode.map toMsg
        |> on "slide"
