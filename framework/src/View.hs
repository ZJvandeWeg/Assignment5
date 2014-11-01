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
    = Pictures ([setToPos location heading ship] 
    			++ [drawScore score horizontalResolution verticalResolution]
    			++ [drawMulti multiplier]
    			++ drawParticles gemShape gems
    			++ drawParticles bulletShape bullets
    			++ drawDebris debris 
    			++ drawParticles starShape backdrop 
    			++ drawAsteroids asteroids)

setToPos :: Location -> Float -> Picture -> Picture
setToPos (x, y) r p = Translate x y (Rotate r p)

ship :: Picture
ship = Color red (Polygon [(0, 20), (-10, 0), (0, 2), (10, 0)])

drawParticle shape p@(Particle {..}) = setToPos loc head (Color color (shape size))
drawParticles shape = map (drawParticle shape)

gemShape = circle
starShape = circleSolid
bulletShape = circleSolid

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

drawMulti :: Int -> Picture
drawMulti m = setToPos (0,0) 0 (Scale 0.2 0.2 (Color white(Text (show m ++ "X"))))