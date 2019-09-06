module Main exposing (..)
import Browser
import Html exposing (Html, h1, h2, h3, h4, h5, img, br, div, text)
import Html.Attributes exposing (src)
import List
main = Browser.sandbox { init = init, view = view, update = update }

type alias Rover = {name: String, imageName: String, cameras: (List Camera)}
type alias Camera = {name: String, abbr: String}

type2Cameras: (List Camera)
type2Cameras = [ {name = "Front Hazard Avoidance Camera", abbr = "FHAZ"}
               , {name = "Rear Hazard Avoidance Camera", abbr = "RHAZ"}
               , {name = "Mast Camera", abbr = "MAST"}
               , {name = "Chemistry and Camera Complex", abbr = "CHEMCAM"}
               , {name = "Mars Hand Lens Imager", abbr = "MARDI"}
               , {name = "Navigation Camera", abbr = "NAVCAM"}               
               ] 

type1Cameras: (List Camera)
type1Cameras = [ {name = "Front Hazard Avoidance Camera", abbr = "FHAZ"}
               , {name = "Rear Hazard Avoidance Camera", abbr = "RHAZ"} 
               , {name = "Mast Camera", abbr = "MAST"}
               , {name = "Navigation Camera", abbr = "NAVCAM"}
               , {name = "Panoramic Camera", abbr = "PANCAM"}
               , {name = "Miniature Thermal Emission Spectrometer", abbr = "MINITES"} 
               ]

init = { rovers = [ {name = "Curiosity", imageName = "curiosity", cameras = type2Cameras}
                  ]
       }

type alias Model = {rovers: (List Rover)}
type Msg = Update Model

update msg model = 
    case Debug.log "msg" msg of 
        Update new -> 
            new

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

roverHaytcheTeeEmElls: (List Rover) -> (List (Html msg))
roverHaytcheTeeEmElls rovers = List.map roverToHaytcheTeeEmEll (List.map (\r -> r.cameras) rovers) 

title: Model -> String
title a = 
  case (List.head a.rovers) of 
    Just rover -> rover.name
    Nothing -> "-"

view model =
  div [] [h1 [] [text (title model)], div[] (roverHaytcheTeeEmElls model.rovers)]
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

