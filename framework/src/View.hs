{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry

import Model

-- | Drawing

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution world@(World{..})
    = Pictures ([Blank] ++ drawStars backdrop ++ [setToPos location heading ship])

ship :: Picture
ship = Color red (Polygon [(0, 20), (-10, 0), (0, 2), (10, 0)])

drawStars stars = map drawStar stars

drawStar star@(Particle {..}) = setToPos loc head (Color color (starShape size))

starShape size = Circle size

setToPos :: Location -> Float -> Picture -> Picture
setToPos (x, y) r p = Translate x y (Rotate r p)