-- foundation

module Main exposing (..)
import Browser

-- FE

import Html exposing (Html, h1, h2, h3, h4, h5, img, br, div, text)
import Html.Attributes exposing (src)
import List

-- network

import Http
import Task exposing (Task)
import Json.Decode as Decode


main = Browser.element{ init = \() -> init, view = view, update = update, subscriptions = subscriptions }

type alias Rover = {name: String, imageName: String, cameras: (List Camera)}
type alias Camera = {name: String, abbr: String}

type alias Model = {rover: Rover, currentSol: Int, latestSol: Int}
type Msg = Inc | Dec 

subscriptions: Model -> Sub msg
subscriptions m = Sub.none
  
--, Cmd Msg 

--Msg (Msg (Model, Cmd Msg))

type2Cameras: (List Camera)
type2Cameras = [ {name = "Front Hazard Avoidance Camera", abbr = "FHAZ"}
               , {name = "Rear Hazard Avoidance Camera", abbr = "RHAZ"}
               , {name = "Mast Camera", abbr = "MAST"}
               , {name = "Chemistry and Camera Complex", abbr = "CHEMCAM"}
               , {name = "Mars Hand Lens Imager", abbr = "MARDI"}
               , {name = "Navigation Camera", abbr = "NAVCAM"}               
               ] 

init: (Model, Cmd Msg)
init = ({rover = {name = "Curiosity", imageName = "curiosity", cameras = type2Cameras}, currentSol = 0, latestSol = 0}
       , Cmd.none
       )

update: Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    (model, Cmd.none)
--    case msg of 
--        Update (mo, ms) -> 
--            (mo, Cmd.none)

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

title: Model -> String
title a = 
  a.rover.name

view: Model -> Html Msg
view model =
  div [] [h1 [] [text (title model)], div[] [(roverHaytcheTeeEmElls model.rover)]]
--  case (List.head model.rovers) of
--    Just rover -> div [] [ text rover.cameras ] -- pure hayche tee em el here --
--    Nothing -> div [] [ text "----" ]

--Maybe a

--But I need a record with a name

-- page 1: 

-- show rover images and rover names
-- navigate to page 2:

-- page 2:
-- show rover images for the current date
-- display in this order: 
-- Name of current rover
-- Date time picker -- mm-dd-yyyy with + and - for m, d, y
-- Update images based on mm-dd-yyy with "today" as the current one

