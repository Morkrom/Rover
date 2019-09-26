-- foundation

module Main exposing (..)
import Browser

-- FE

import Html exposing (Html, h1, h2, h3, h4, h5, img, br, div, text)
import Html.Attributes exposing (src)
import List exposing (..)
import Dict exposing (Dict, diff)
import String exposing (..) 

-- network

import Http
import Json.Decode as Json
import Json.Decode exposing (Decoder, field, map2, map3, string, list, dict, int, at)

import Url exposing (..)

type alias Photo = {cameraKey: String, photoUrl: String}
type alias Camera = {name: String, abbr: String}
type alias Rover = {name: String, cameras: (List Camera), maxSol: Int}
type alias DataModel = {rover: Rover, currentSol: Int, album: PhotoAlbum}

type alias Sol = Int
type alias CameraKey = String
type alias PhotoRoll = Dict CameraKey (List Photo)
type alias PhotoAlbum = Dict Sol PhotoRoll

type alias FetchRoverResult = {rover: Rover, currentSol: Int}
type alias FetchSolAlbumResult = List Photo
--store: 
-- << 'currentSol' did change
-- sol : [camera_key : [CameraData]]
-- > get sol: n, camera: camera_key  << "photos" : [{img_src: string}]

type Model = Empty
           | Error String
           | Loaded DataModel
 
type Msg = FetchRover (Result Http.Error FetchRoverResult)
         | FetchCameras (Result Http.Error FetchSolAlbumResult)
         | FetchError Http.Error

subscriptions: Model -> Sub msg
subscriptions m = Sub.none

bootUrl: String
bootUrl = "https://mars-photos.herokuapp.com/api/v1/rovers/curiosity"

-- By Mars Sol
camerasUrl: Int -> String 
camerasUrl sol = "https://mars-photos.herokuapp.com/api/v1/rovers/curiosity/photos?sol=" ++ (fromInt sol) 

init: (Model, Cmd Msg)
init = ( Empty 
       , fetchCuriosity )

main = Browser.element{ init = \() -> init, view = view, update = update, subscriptions = subscriptions }

fetchCuriosity : Cmd Msg
fetchCuriosity =
   Http.get { expect = Http.expectJson FetchRover roverEndpointDecoder, url = bootUrl }

fetchCameras: Int -> Cmd Msg
fetchCameras sol =
  Http.get { expect = Http.expectJson FetchCameras camerasEndpointDecoder, url = camerasUrl sol}

photoDecoder: Json.Decoder Photo 
photoDecoder = 
  map2 Photo (at ["camera", "name"] string) (field "img_src" string)

camerasEndpointDecoder: Json.Decoder FetchSolAlbumResult
camerasEndpointDecoder =
  at ["photos"] (list photoDecoder)
  --  

roverEndpointDecoder: Json.Decoder FetchRoverResult
roverEndpointDecoder = 
  map2 FetchRoverResult (map3 Rover (at ["rover", "name"] string) (at ["rover", "cameras"] (list cameraDecoder)) (at ["rover", "max_sol"] int)) (at ["rover", "max_sol"] int)

cameraDecoder: Json.Decoder Camera
cameraDecoder = 
  map2 Camera (field "full_name" string) (field "name" string) 

camKeyMap: (List Photo) -> (List (String, Photo))
camKeyMap list = 
   List.map (\n -> (n.cameraKey, n)) list

deTupleAndFlatten: String -> Photo -> Dict String (List Photo) -> Dict String (List Photo)
deTupleAndFlatten key photo streamHead = 
  Dict.update key (\_ -> Just ((Maybe.withDefault [] (Dict.get key streamHead)) ++ [photo])) streamHead

photoRoll: (List Photo) -> PhotoRoll
photoRoll list = 
  Dict.foldl deTupleAndFlatten Dict.empty (Dict.fromList (camKeyMap list))

photoAlbum: DataModel -> (List Photo) -> PhotoAlbum
photoAlbum dataModel newPhotos =
  Dict.update dataModel.currentSol (\_ -> Just (Dict.diff (Maybe.withDefault Dict.empty (Dict.get dataModel.currentSol dataModel.album)) (photoRoll newPhotos))) dataModel.album

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      FetchRover result ->
        case result of
           Ok m ->
             (Loaded {rover = m.rover, currentSol = m.currentSol, album = Dict.empty}, fetchCameras m.currentSol) 
           Err _ ->
             (Error "Big one", Cmd.none)   
      FetchCameras result ->
        case result of
          Ok m ->
            case model of 
              Loaded current ->
                (Loaded { current | album = (photoAlbum current m)}, Cmd.none)
              _ ->
                (Error "Expected model to be loaded by now, only trying to modify an existing model's photo album", Cmd.none)
          Err _ ->
            (Error "--", Cmd.none)
      FetchError error -> 
        case error of
          Http.BadBody string ->
            (Error string, Cmd.none)
          _ -> 
            (Error "Unknown error", Cmd.none)

roverCameraImg: String -> Html msg
roverCameraImg key = img [src "http://mars.jpl.nasa.gov/msl-raw-images/proj/msl/redops/ods/surface/sol/00000/opgs/edr/fcam/FRA_397502305EDR_D0010000AUT_04096M_.JPG"] []

roverCameraHtml: Camera -> Html msg
roverCameraHtml c = div [] [h3 [] [text c.abbr], h5 [] [text c.name], roverCameraImg c.abbr, br [] []]

 -- load the image w given url

mappedRoverCameraHtml: (List Camera) -> (List (Html msg))
mappedRoverCameraHtml cameras = List.map roverCameraHtml cameras

roverToHaytcheTeeEmEll: (List Camera) -> Html msg
roverToHaytcheTeeEmEll cameras = div [] (mappedRoverCameraHtml cameras)

roverHaytcheTeeEmElls: Rover -> Html msg
roverHaytcheTeeEmElls rover = roverToHaytcheTeeEmEll rover.cameras 

title: DataModel -> String
title a = 
  a.rover.name

view: Model -> Html Msg
view m = 
  case m of 
    Empty ->
      text "Fetching.."
    Error description -> 
      text description
    Loaded dataModel -> 
      div [] [h1 [] [text dataModel.rover.name], (roverHaytcheTeeEmElls dataModel.rover)]
