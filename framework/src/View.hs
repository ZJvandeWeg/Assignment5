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
    = Pictures [Blank, setToPos location heading ship]

ship :: Picture
ship = Color red (Polygon [(0, 20), (-10, 0), (0, 2), (10, 0)])

setToPos :: Location -> Float -> Picture -> Picture
setToPos (x, y) r p = Translate x y (Rotate r p)