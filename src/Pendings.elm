--
-- View image
--

module Main exposing (..)

import Browser
import Bulma.CDN exposing (..)
--import Bulma.Modifiers exposing (..)
--import Bulma.Modifiers.Typography exposing (textCentered)
--import Bulma.Form exposing (..)
--import Bulma.Elements exposing (..)
--import Bulma.Components exposing (..)
--import Bulma.Columns as Columns exposing (..)
--import Bulma.Layout exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode exposing (..)

-- CONSTANTS
serverUrl : String
serverUrl = "http://192.168.11.50:4567/"

imageNoUrl : String
imageNoUrl = serverUrl ++ "imageno/"

pendingsUrl : String
pendingsUrl = serverUrl ++ "pendings/"

discardUrl : String
discardUrl = serverUrl ++ "discardpd/"

reviveUrl : String
reviveUrl = serverUrl ++ "revivepd/"

confirmUrl : String
confirmUrl = serverUrl ++ "confirm/"

refreshUrl : String
refreshUrl = serverUrl ++ "refresh/"

nimageParLine : Int
nimageParLine = 12

ndisp : Int
ndisp = nimageParLine * 3

-- TYPES

type ImageStatus =
  Filed | Discarded | Duplicated | Deleted | Inferior | Pending

type Image =
  Image Int String Int Int Int ImageStatus

type alias Model =
  { start : Int
  , nimage : Int
  , images : List (Int, List Image)
  , message : String
  }

type Msg
  = Click
  | First
  | Prev Int
  | Next Int
  | Last
  | Confirm
  | Refresh
  | Discard Int
  | Revive Int
  | GotImages (Result Http.Error String)

-- MAIN

main : Program () Model Msg
main =
  Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = \_ -> Sub.none
  }

-- INIT & UPDATE

init : () -> (Model, Cmd Msg)
init _ =
  ( { start = 0
    , nimage = 0
    , images = []
    , message = ""
    }
  , getImageInfo ndisp 0
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    im = List.reverse model.images
    img1 = Image 11183 "eaepc-3e76b86d13460a83517ba7d364d903c9d9699356.jpg" 960 1168 208658 Filed
    img2 = Image 16150 "eaepc-7d1e03ad8a848b5b5ca852a42e06b9cf4d9492b9.jpg" 960 1168 191886 Pending
  in
    case msg of
    Click ->
      ( model
      , getImageInfo ndisp (model.start + ndisp)
      )
    First ->
      ( { model
          | start = 0
        }
      , getImageInfo ndisp 0      
      )
    Prev pg ->
      ( { model
          | start = if model.start - ndisp * pg >= 0
              then model.start - ndisp * pg
              else 0
        }     
      , getImageInfo ndisp (model.start - ndisp * pg)
      )
    Next pg ->
      ( { model
          | start = if model.start + ndisp * pg > model.nimage
              then model.start
              else model.start + ndisp * pg
        }     
      , getImageInfo ndisp (model.start + ndisp * pg)
      )
    Last ->
      ( { model
          | start = (model.nimage // ndisp) * ndisp
        }     
      , getImageInfo ndisp ((model.nimage // ndisp) * ndisp)
      )
    Confirm ->
      ( model
      , confirmImageStatus ndisp model.start
      )
    Refresh ->
      ( { model
          | start = 0
        }
      , refreshImages ndisp
      )
    Discard imageno->
      ( model
      , discardImage imageno ndisp model.start
      )
    Revive imageno->
      ( model
      , reviveImage imageno ndisp model.start
      )
    GotImages (Ok json) ->
      ( updateModel model json
      , Cmd.none
      )
    GotImages (Err error) ->
      ( { model | start = 0, images = [(1, [img1, img2])], message = Debug.toString error}
      , Cmd.none
      )

discardImage : Int -> Int -> Int -> Cmd Msg
discardImage imageno len start =
  Http.get
  { url = discardUrl ++ (String.fromInt imageno) ++ "/" ++
     (String.fromInt len) ++ "/" ++ (String.fromInt start)
  , expect = Http.expectString GotImages
  }

reviveImage : Int -> Int -> Int -> Cmd Msg
reviveImage imageno len start =
  Http.get
  { url = reviveUrl ++ (String.fromInt imageno) ++ "/" ++
     (String.fromInt len) ++ "/" ++ (String.fromInt start)
  , expect = Http.expectString GotImages
  }

confirmImageStatus : Int -> Int -> Cmd Msg
confirmImageStatus len start =
  Http.get
  { url = confirmUrl ++ (String.fromInt len) ++ "/" ++ (String.fromInt start)
  , expect = Http.expectString GotImages
  }

refreshImages : Int -> Cmd Msg
refreshImages len =
  Http.get
  { url = refreshUrl ++ (String.fromInt len)
  , expect = Http.expectString GotImages
  }

getImageInfo : Int -> Int -> Cmd Msg
getImageInfo len start =
  Http.get
  { url = pendingsUrl ++ (String.fromInt len) ++ "/" ++ (String.fromInt start)
  , expect = Http.expectString GotImages
  }

updateModel : Model -> String -> Model
updateModel model json =
  let
    img1 = Image 11183 "eaepc-3e76b86d13460a83517ba7d364d903c9d9699356.jpg" 960 1168 208658 Filed
    img2 = Image 16150 "eaepc-7d1e03ad8a848b5b5ca852a42e06b9cf4d9492b9.jpg" 960 1168 191886 Pending
    imgs = case (decodeString decoder json) of
      Ok list -> list
      Err err -> [img1, img2]
    imglist = split nimageParLine imgs
    nums = List.range model.start (List.length imglist + model.start - 1)
    nimg = case (decodeString (Json.Decode.field "nimage" int) json) of
      Ok i -> i
      Err err -> 0
  in
    { model
    | nimage = nimg
    , images = List.map2 Tuple.pair nums imglist
    , message = "OK"
    }

decoder : Decoder (List Image)
decoder = Json.Decode.field "images" decoderListImage

decoderListImage : Decoder (List Image)
decoderListImage = Json.Decode.list decoderImage

decoderImage : Decoder Image
decoderImage =
  Json.Decode.map6 Image
    (field "id" int)
    (field "filename" string)
    (field "xreso" int)
    (field "yreso" int)
    (field "filesize" int)
    (field "status" string |> andThen decoderStatus)

decoderStatus : String -> Decoder ImageStatus
decoderStatus status =
  case status of
      "filed"      -> succeed Filed
      "discarded"  -> succeed Discarded
      "duplicated" -> succeed Duplicated
      "deleted"    -> succeed Deleted
      "inferior"   -> succeed Inferior
      "pending"    -> succeed Pending
      _            -> succeed Pending

-- VIEW

view : Model -> Html Msg
view model =
  main_ []
  [ stylesheet
  , bulmaCDN
  , fontAwesomeCDN
  , header model
--  , buttons model
  , imageList model.images
  , buttons model
  ]

fontAwesomeCDN =
  Html.node "link"
  [ rel "stylesheet"
  , href "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
  ]
  []

bulmaCDN =
  Html.node "link"
  [ rel "stylesheet"
  , href "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.1.2/css/bulma.css"
  ]
  []

header : Model -> Html Msg
header model =
  section [ class "hero is-warning is-bold"]
  [ div [ class "hero-body" ]
    [ div [ class "container" ]
      [ h1 [ class "title" ]
        [ text "pending image: results"]
      , h2 [ class "subtitle" ]
        [ text ("Page No :" ++ (String.fromInt (model.start // ndisp + 1)) ++
                " / " ++ (String.fromInt (model.nimage // ndisp + 1))
               )
        ]
      ]
    ]
  , buttons model
  ]

buttons : Model -> Html Msg
buttons model =
  section [ class "navbar" ]
  [
    div [ class "container"]
    [ p [ class "control" ]
      [
        a [ class "button is-warning is-inverted", onClick Refresh]
        [ span [ class "icon is-small" ]
          [ i [ class ("fa fa-sync")] [] ]
        , span [] [ text "Refresh"]
        ]
      , a [ class "button is-link is-outlined", onClick First]
        [ span [ class "icon is-small" ]
          [ i [ class ("fa fa-fast-backward")] [] ]
        , span [] [ text "First"]
        ]
      , a [ class "button is-link is-outlined", onClick (Prev 100) ]
        [ span [ class "icon is-small" ]
          [ i [ class "fa fa-arrow-circle-left" ] [] ]
        , span [] [ text "Prev 100" ]
        ]
      , a [ class "button is-link is-outlined", onClick (Prev 1) ]
        [ span [ class "icon is-small" ]
          [ i [ class "fa fa-arrow-circle-left" ] [] ]
        , span [] [ text "Prev" ]
        ]
      , a [ class "button is-link is-outlined", onClick (Next 1) ]
        [ span [ class "icon is-small" ]
          [ i [ class ("fa fa-arrow-circle-right")] [] ]
        , span [] [ text "Next" ]
        ]
      , a [ class "button is-link is-outlined", onClick (Next 100) ]
        [ span [ class "icon is-small" ]
          [ i [ class ("fa fa-arrow-circle-right")] [] ]
        , span [] [ text "Next 100" ]
        ]
      , a [ class "button is-link is-outlined", onClick Last ]
        [ span [ class "icon is-small" ]
          [ i [ class ("fa fa-fast-forward")] [] ]
        , span [] [ text "Last" ]
        ]
      , a [ class "button is-danger is-outlined", onClick Confirm ]
        [ span [ class "icon is-small" ]
          [ i [ class "fa fa-check-circle" ] [] ]
        , span [] [ text "Confirm" ]
        ]
      ]
    ]
  ]

imageList : List (Int, List Image) -> Html Msg
imageList lists =
  section [ class "section" ]
  [ div [ class "container is-fluid" ]
    (List.map imageBox lists)
  ]

imageBox : (Int, List Image) -> Html Msg
imageBox (num, imgs) =
  let
    ma = List.maximum (List.map calcArea imgs)
    mf = List.maximum (List.map getFilesize imgs)
    maxarea = case ma of
      Just(a) -> a
      Nothing -> 0
    maxfsz  = case mf of
      Just(a) -> a
      Nothing -> 0
  in
    div [class "box"]
    [ div [ class "columns is-mobile is-multiline" ]
      (List.map (imageColum maxarea maxfsz) imgs)
    ]

calcArea : Image -> Int
calcArea (Image _ _ x y _ _) = x * y

getFilesize : Image -> Int
getFilesize (Image _ _ _ _ sz _) = sz

statusToString : ImageStatus -> String
statusToString st =
  case st of
  Filed      -> "filed"
  Discarded  -> "discarded"
  Duplicated -> "duplicated"
  Deleted    -> "deleted"
  Pending    -> "pending"
  Inferior   -> "inferior"

imageColum : Int -> Int -> Image -> Html Msg
imageColum maxarea maxfsz (Image id fname x y fsize st) =
  let
    area = x * y
    asz = text (String.fromInt x ++ " x " ++ String.fromInt y)
    fsz = numComma (fsize // 1000)
    icon = case st of
      Pending -> "question-circle"
      Filed   -> "folder-open"
      Discarded -> "trash"
      Duplicated -> "clone"
      Deleted    -> "times"
      Inferior -> "trash"
    gray = if st == Filed || st == Pending then [] else weakened
    cmd = if st == Discarded then Revive id else Discard id
  in
    div [ class "column is-3-desktop is-12-fullhd is-marginless" ]
    [ span [ class "icon" ]
      [ i [ class ("fa fa-" ++ icon)] []
      ]
    , a [ class "button is-link"
        , href (imageNoUrl ++ String.fromInt id)
        , target "_blank"
        ]
      [ span [ class "icon" ]
        [ i [ class "fa fa-search-plus"] [] ]
      ]
--    , text (String.fromInt id ++ ": ")
    , asz
    , a [ onClick cmd
        , title ("id:" ++ String.fromInt id ++ ", size:" ++ fsz ++ "KB")
        ]
        [ figure [ class "image" ]
          [ img ([ src (imageNoUrl ++ String.fromInt id)
                 , alt ("id: " ++ String.fromInt id)
                 ] ++ gray) []
          ]
        ]
    ]

numComma : Int -> String
numComma num =
  let
    numstr = List.reverse (String.toList (String.fromInt num))
    revnum = String.join "," (List.map String.fromList (splitList 3 numstr))
  in
    String.fromList (List.reverse (String.toList revnum))

splitList : Int -> List Char -> List (List Char)
splitList digit numstr =
  case numstr of
  [] ->
    []
  list ->
    [List.take 3 numstr] ++ splitList digit (List.drop digit numstr)

-- Stylesheet

grayscaling : List (Attribute msg)
grayscaling =
  [ style "-webkit-filter" "grayscale(100%)"
  , style "-moz-filter" "grayscale(100%)"
  , style "-ms-filter" "grayscale(100%)"
  , style "-o-filter" "grayscale(100%)"
  , style "filter" "grayscale(100%)"
  ]

weakened : List (Attribute msg)
weakened =
  [ style "background-color" "white"
  , style "opacity" "0.4"
  , style "display" "block"
  ]

split : Int -> List a -> List (List a)
split i list =
  case List.take i list of
    [] -> []
    listHead -> listHead :: split i (List.drop i list)
