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
import Json.Decode exposing (Decoder, field, map2, map3, string, list, dict, int, at, succeed)

import Url exposing (..)

type alias Photo = {cameraKey: String, photoUrl: String}
type alias Camera = {name: String, abbr: String}
type alias Rover = {name: String, cameras: (List Camera), maxSol: Int}
type alias DataModel = {rover: Rover, currentSol: Int, album: PhotoAlbum, cams: (List Photo)}

type alias Sol = Int
type alias CameraKey = String
type alias PhotoRoll = Dict CameraKey (List Photo)
type alias PhotoAlbum = Dict Sol PhotoRoll

type alias FetchRoverResult = {rover: Rover, currentSol: Int}

-- (List String) is camera keys
type alias FetchSolAlbumResult = { photos: (List Photo), remainingKeys: (List String) }

type Model = Empty
           | Error String
           | LoadedRover DataModel
           | LoadedCamera DataModel
 
type Msg = FetchRover (Result Http.Error FetchRoverResult)
         | FetchCameras (Result Http.Error FetchSolAlbumResult)
         | FetchError Http.Error

subscriptions: Model -> Sub msg
subscriptions m = Sub.none

bootUrl: String
bootUrl = "https://mars-photos.herokuapp.com/api/v1/rovers/curiosity"

-- By Mars Sol
camerasUrl: String -> Int -> String 
camerasUrl key sol = "https://mars-photos.herokuapp.com/api/v1/rovers/curiosity/photos?sol=" ++ (fromInt sol) ++ "&camera=" ++ key 

init: (Model, Cmd Msg)
init = ( Empty 
       , fetchCuriosity )

main = Browser.element { init = \() -> init, view = view, update = update, subscriptions = subscriptions }

fetchCuriosity : Cmd Msg
fetchCuriosity =
   Http.get { expect = Http.expectJson FetchRover roverEndpointDecoder, url = bootUrl }

fetchCameras: (List String) -> Int -> Cmd Msg
fetchCameras cameraKeys sol =
  case (List.head cameraKeys) of
    Just h ->
      Http.get { expect = Http.expectJson FetchCameras (camerasEndpointDecoder cameraKeys), url = camerasUrl h sol }
    Nothing ->
      Cmd.none  

photoDecoder: Json.Decoder Photo 
photoDecoder = 
  Json.map2 Photo (at ["camera", "name"] string) (field "img_src" string)

camerasEndpointDecoder: (List String) -> Json.Decoder FetchSolAlbumResult
camerasEndpointDecoder cameraKeys =
  Json.map2 FetchSolAlbumResult (at ["photos"] (list photoDecoder)) (succeed cameraKeys) 

roverEndpointDecoder: Json.Decoder FetchRoverResult
roverEndpointDecoder = 
  Json.map2 FetchRoverResult (Json.map3 Rover (at ["rover", "name"] string) (at ["rover", "cameras"] (list cameraDecoder)) (at ["rover", "max_sol"] int)) (at ["rover", "max_sol"] int)

cameraDecoder: Json.Decoder Camera
cameraDecoder = 
  Json.map2 Camera (field "full_name" string) (field "name" string) 

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
  Dict.insert dataModel.currentSol (Dict.union (photoRoll newPhotos) (Maybe.withDefault Dict.empty (Dict.get dataModel.currentSol dataModel.album))) dataModel.album

loadCamera: Model -> (List String) -> Int -> ( Model, Cmd Msg)
loadCamera model list sol =   
  case list of
    f::first ->
      (model, fetchCameras (List.drop 1 list) sol)
    _ ->
      (model, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      FetchRover result ->
        case result of
           Ok m ->
             loadCamera (LoadedRover {rover = m.rover, currentSol = m.currentSol, album = Dict.empty, cams = []}) ("-"::(List.map (\camera -> camera.abbr) m.rover.cameras)) m.currentSol 
           Err _ ->
             (Error "Big one", Cmd.none)   
      FetchCameras result ->
        case result of
          Ok m ->
            case model of
              LoadedCamera current ->
                (loadCamera (LoadedCamera { current | album = photoAlbum current m.photos, cams = m.photos}) m.remainingKeys current.currentSol)
              LoadedRover initial ->
                (loadCamera (LoadedCamera { initial | album = photoAlbum initial m.photos, cams = m.photos}) m.remainingKeys initial.currentSol)
              _ ->
                (Error "Error loading camera", Cmd.none)
          Err _ ->
            (Error "--", Cmd.none)
      FetchError error -> 
        case error of
          Http.BadBody string ->
            (Error string, Cmd.none)
          _ -> 
            (Error "Unknown error", Cmd.none)

roverCameraImg: String -> Html msg
roverCameraImg key = img [src key] []

roverCameraHtml: (Camera, (List Photo)) -> Html msg
roverCameraHtml (c, l) = 
  case List.head (List.reverse l) of
    Just p ->
      div [] [h3 [] [text c.abbr], h5 [] [text c.name], roverCameraImg p.photoUrl, br [] []]
    Nothing ->
      div [] [text ("where's the list?  " ++ "count: " ++ (fromInt (List.length l)))]

mappedRoverCameraHtml: (List (Camera, (List Photo))) -> (List (Html msg))
mappedRoverCameraHtml cameras = List.map roverCameraHtml cameras

roverCamerasStack: (List (Camera, (List Photo))) -> Html msg
roverCamerasStack cameras = div [] (mappedRoverCameraHtml cameras)

photosStack: Rover -> PhotoRoll -> Html msg
photosStack rover roll = 
  div [] [(text (fromInt (Dict.size roll))), roverCamerasStack (List.map (\c -> (c, Maybe.withDefault [] (Dict.get c.abbr roll))) rover.cameras)]
-- cameras |> list of (Camera, (List Photo)) 
-- roverCamerasStack rover.cameras 

photosStackz: DataModel -> Html msg
photosStackz dm = 
  div [] [(text (fromInt (List.length dm.cams))), roverCamerasStack (List.map (\c -> (c, dm.cams)) dm.rover.cameras)]
-- cameras |> list of (Camera, (List Photo)) 
-- roverCamerasStack rover.cameras 

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
    LoadedRover dataModel ->
      case Dict.get dataModel.currentSol dataModel.album of
        Just booklet ->
          div [] [h1 [] [text dataModel.rover.name], h4 [] [text (fromInt dataModel.currentSol)], (text "its just an empty booklet")]
        Nothing ->
          div [] [h1 [] [text dataModel.rover.name], h4 [] [text (fromInt dataModel.currentSol)], br [] [], (text "We're still loading albums")]
    LoadedCamera dataModel ->
       case Dict.get dataModel.currentSol dataModel.album of
        Just booklet ->
          div [] [h1 [] [text dataModel.rover.name], h4 [] [text (fromInt dataModel.currentSol)], (photosStack dataModel.rover booklet)]
        Nothing ->
          div [] [h1 [] [text dataModel.rover.name], h4 [] [text (fromInt dataModel.currentSol)], br [] [], (text "There was trouble loading an album for Sol")]     
