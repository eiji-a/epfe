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

findsameUrl : String
findsameUrl = serverUrl ++ "findsame/"

discardUrl : String
discardUrl = serverUrl ++ "discardfs/"

reviveUrl : String
reviveUrl = serverUrl ++ "revivefs/"

ndisp : Int
ndisp = 10

sampleJson : String
sampleJson = """
{
  "images": [
    [
      {
        "id": 822,
        "filename": "eaepc-1ab3570397deb2faf1b922c993f95f1994d4abaf.jpg",
        "xreso": 683,
        "yreso": 1024,
        "filesize": 144154,
        "status": "filed"
      },
      {
        "id": 2461,
        "filename": "eaepc-56474d4b1e47e7fbf2bb814e98aa6f4f6608b063.jpg",
        "xreso": 683,
        "yreso": 924,
        "filesize": 203860,
        "status": "filed"
      }
    ],
    [
      {
        "id": 836,
        "filename": "eaepc-1aeeac2142deff88f6993663a8d3e1b5fa096f37.jpg",
        "xreso": 800,
        "yreso": 1150,
        "filesize": 181192,
        "status": "filed"
      },
      {
        "id": 9564,
        "filename": "eaepc-27797deeeed11616ac98005301a305d8710452ba.jpg",
        "xreso": 775,
        "yreso": 1131,
        "filesize": 203955,
        "status": "filed"
      }
    ]
  ]
}
"""

sampleJson2 : String
sampleJson2 = """
{
        "id": 822,
        "filename": "eaepc-1ab3570397deb2faf1b922c993f95f1994d4abaf.jpg",
        "xreso": 683,
        "yreso": 1024,
        "filesize": 144154,
        "status": "filed"
}
"""


-- TYPES

type ImageStatus =
  Filed | Discarded | Duplicated | Inferior | Pending

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
  | Prev
  | Next
  | Last
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
  let
    num0 = 0
    img1 = Image 11183 "eaepc-3e76b86d13460a83517ba7d364d903c9d9699356.jpg" 960 1168 208658 Filed
    img2 = Image 16150 "eaepc-7d1e03ad8a848b5b5ca852a42e06b9cf4d9492b9.jpg" 960 1168 191886 Pending
    img3 = Image 17126 "eaepc-89cdd819c21809b7748b4b4cacabe2436b9ec16b.jpg" 910 1297 173291 Filed
    img4 = Image 28984 "eaepc-ac2e10c1b69c4eeb71dde609ebb6ba58b3005319.jpg" 910 1258 168734 Filed
    img5 = Image 26831 "eaepc-f8c57e33609d12ed5bcc38a3246d2d394fea3d3d.jpg" 910 1258 167845 Filed
    img6 = Image 17963 "eaepc-93df4ef42fbbf2ef3e14e1d3467fe4f52a3d6be6.jpg" 713 1024 172513 Filed
    img7 = Image 27942 "eaepc-bed8b43c97b512a2f3050b59a0c9a342b8a1058a.jpg" 1067 1540 195449 Filed
    --imglist = [[img1, img2], [img3, img4, img5], [img6, img7]]
    imglist = [[img6, img7], [img1, img2], [img3, img4, img5]]
    nums = List.range num0 (List.length imglist + num0 - 1)
  in
    ( { start = num0
      , nimage = List.length imglist
      , images = []
      , message = ""
      }
    , getImageInfo ndisp num0
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
    Prev ->
      ( { model
          | start = if model.start - ndisp >= 0
              then model.start - ndisp
              else 0
        }     
      , getImageInfo ndisp (model.start - ndisp)      
      )
    Next ->
      ( { model
          | start = if model.start + ndisp > model.nimage
              then model.start
              else model.start + ndisp
        }     
      , getImageInfo ndisp (model.start + ndisp)
      )
    Last ->
      ( { model
          | start = (model.nimage // ndisp) * ndisp
        }     
      , getImageInfo ndisp ((model.nimage // ndisp) * ndisp)
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

getImageInfo : Int -> Int -> Cmd Msg
getImageInfo len start =
  Http.get
  { url = findsameUrl ++ (String.fromInt len) ++ "/" ++ (String.fromInt start)
  , expect = Http.expectString GotImages
  }

updateModel : Model -> String -> Model
updateModel model json =
  let
    img1 = Image 11183 "eaepc-3e76b86d13460a83517ba7d364d903c9d9699356.jpg" 960 1168 208658 Filed
    img2 = Image 16150 "eaepc-7d1e03ad8a848b5b5ca852a42e06b9cf4d9492b9.jpg" 960 1168 191886 Pending
    imglist = case (decodeString decoder json) of
    --imglist = case (decodeString decoderImage json) of
      Ok list -> list
      Err err -> [[img1, img2]]
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

decoder : Decoder (List (List Image))
decoder = Json.Decode.field "images" decoderListListImage

decoderListListImage : Decoder (List (List Image))
decoderListListImage = Json.Decode.list decoderListImage

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
  , buttons model
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
  section [ class "hero is-primary is-bold"]
  [ div [ class "hero-body" ]
    [ div [ class "container" ]
      [ h1 [ class "title" ]
        [ text "findsame: results"]
      , h2 [ class "subtitle" ]
        [ text ("Image No. :" ++ (String.fromInt (model.start+1)) ++ " - " ++
                (String.fromInt (List.length model.images + model.start)) ++
                " / " ++ (String.fromInt model.nimage)
               )
        ]
      ]
    ]
  ]

buttons : Model -> Html Msg
buttons model =
  section [ class "section" ]
  [ div [ class "container is-fluid"]
    [ a [ class "button is-link is-outlined", onClick First]
      [ text "First"]
    , a [ class "button is-link is-outlined", onClick Prev ]
      [ text "Prev" ]
    , a [ class "button is-link is-outlined", onClick Next ]
      [ text "Next" ]
    , a [ class "button is-link is-outlined", onClick Last ]
      [ text "Last" ]
    , text model.message
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
    [ text ("No." ++ String.fromInt (num + 1))
    , div [ class "columns is-gapless" ]
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
  Pending    -> "pending"
  Inferior   -> "inferior"

imageColum : Int -> Int -> Image -> Html Msg
imageColum maxarea maxfsz (Image id fname x y fsize st) =
  let
    area = x * y
    asz = text (String.fromInt x ++ " x " ++ String.fromInt y)
    fsz = text (numComma fsize)
    gray = if st == Filed || st == Pending then [] else weakened
    cmd = if st == Discarded then Revive id else Discard id
  in
    div [ class "column is-3 is-marginless" ]
    [ a [ class "button is-link", onClick cmd ]
      [ span [ class "icon" ]
        [ i [ class "fa fa-trash"] [] ]
      ]
    , text fname
    , br [] []
    , text ("(" ++ String.fromInt id ++ ") ")
    , if area < maxarea then asz else strong [] [asz]
    , text " / "
    , if fsize < maxfsz then fsz else strong [] [fsz]
    , text (" bytes [" ++ (statusToString st) ++ "]")
    , a [ href (imageNoUrl ++ String.fromInt id) ]
        [ figure [ class "image" ]
          [ img ([ src (imageNoUrl ++ String.fromInt id) ] ++ gray) []
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


