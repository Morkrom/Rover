-- foundation

module Main exposing (..)
import Browser

-- FE

import Html exposing (Html, h1, h2, h3, h4, h5, img, br, div, text)
import Html.Attributes exposing (src)
import List exposing (..)

-- network

import Http
import Json.Decode as Json
import Json.Decode exposing (Decoder, field, map2, map3, string, list, dict, int, at)

import Url exposing (..)

type alias Camera = {name: String, abbr: String}
type alias Rover = {name: String, cameras: (List Camera), maxSol: Int}
type alias DataModel = {rover: Rover, currentSol: Int}

type Model = Empty
           | Error String
           | Loaded DataModel
 
type Msg = FetchRover (Result Http.Error DataModel)
         | FetchError Http.Error

subscriptions: Model -> Sub msg
subscriptions m = Sub.none

bootUrl: String
bootUrl = "https://mars-photos.herokuapp.com/api/v1/rovers/curiosity"

init: (Model, Cmd Msg)
init = ( Empty 
       , fetchCuriosity )

main = Browser.element{ init = \() -> init, view = view, update = update, subscriptions = subscriptions }

fetchCuriosity : Cmd Msg 
fetchCuriosity =
   Http.get { expect = Http.expectJson FetchRover roverEndpointDecoder, url = bootUrl }

-- > {} Loaded DataModel
-- > DataModel {rover: Rover, currentSol: Int, latestSol: Int}

roverEndpointDecoder: Json.Decoder DataModel
roverEndpointDecoder = 
  map2 DataModel (map3 Rover (at ["rover", "name"] string) (at ["rover", "cameras"] (list cameraDecoder)) (at ["rover", "max_sol"] int)) (at ["rover", "max_sol"] int)

-- "cameras"

cameraDecoder: Json.Decoder Camera
cameraDecoder = 
  map2 Camera (field "full_name" string) (field "name" string) 
  
-- "rover": object
-- max_sol: int
-- 

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
      FetchRover result -> 
        case result of
          Ok m ->
            (Loaded m, Cmd.none)
          
          Err _ ->
            (Error "Big one", Cmd.none)
  
      FetchError error ->
        case error of
          Http.BadBody string ->
            (Error string, Cmd.none)
          _ -> 
            (Error "Unknown error", Cmd.none)

-- div for the camera image title, camera image subtitle, camera image
-- https://mars-photos.herokuapp.com/api/v1/rovers/curiosity/latest_photos
-- image url 

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
