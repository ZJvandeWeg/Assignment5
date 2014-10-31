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
    = Pictures ([Blank] ++ drawStars backdrop ++ drawStars bullets ++ drawDebris debris ++ drawAsteroids asteroids ++ [setToPos location heading ship] ++ [drawScore score horizontalResolution verticalResolution])


ship :: Picture
ship = Color red (Polygon [(0, 20), (-10, 0), (0, 2), (10, 0)])

drawStars stars = map drawStar stars

drawStar star@(Particle {..}) = setToPos loc head (Color color (starShape size))

starShape size = circleSolid size

setToPos :: Location -> Float -> Picture -> Picture
setToPos (x, y) r p = Translate x y (Rotate r p)

--Asteroid Drawing
drawAsteroids = map drawAsteroid

drawAsteroid :: Asteroid -> Picture
drawAsteroid a@(Asteroid {..}) = setToPos aLocation aHeading $ Color green (circleSolid 8)

drawScore :: Int -> Float -> Float -> Picture
drawScore i x y = Translate (x / 2 - 150) (y / 2 - 50) (Scale 0.2 0.2 (Color white(Text ("Score: " ++ (show i)))))

--Debris drawings
drawDebris :: [(Location, Float)] -> [Picture]
drawDebris = map drawDebris'
  where drawDebris' (l,h) = setToPos l h debrisShape

debrisShape :: Picture
debrisShape = Color red (Circle 1)
