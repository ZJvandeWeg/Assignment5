{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>))
import Data.List
import System.Random
import Graphics.Gloss
import Model

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {..}) = world { currentTime = time,
                                              heading = newHeading heading rotateAction,
                                              location = newLocation location movementSpeed heading,
											  backdrop = moveStars (addStar backdrop rndGen),
											  rndGen   = snd (next rndGen) }

newHeading :: Float -> RotateAction -> Float
newHeading r RotateLeft  = r - rotateSpeed
newHeading r RotateRight = r + rotateSpeed
newHeading r _           = r

newLocation :: Location -> Float -> Float -> Location --location, speed, heading
newLocation (x,y) s r = ((sin rad) * s + x, (cos rad) * s + y)
    where rad = r / 360  * (2 * pi)

addStar :: [Particle] -> StdGen -> [Particle]
addStar x rnd = (Particle white spd (-90) spd (1000, ypos)) : x
    where ypos = fst (randomR (-1000, 1000) rnd)
          spd  = fst (randomR (1, 5) (snd (next rnd)))

moveStars :: [Particle] -> [Particle]
moveStars x = map moveStar x
    where moveStar part@(Particle {..}) = part { loc = newLocation loc speed head }

rotateSpeed :: Float
rotateSpeed = 2

movementSpeed :: Float
movementSpeed = 2


{--
	Asteroid handling
--}
asteroidMovementSpeed :: Float
asteroidMovementSpeed = 2.1

asteroidRotationSpeed :: Float
asteroidRotationSpeed = 1.4

moveAsteroid :: World -> Asteroid -> Asteroid
moveAsteroid world@(World{..}) astr@(Asteroid{..}) = astr { aLocation = newLocation aLocation asteroidMovementSpeed heading'}
	where heading' = asteroidHeading location astr

asteroidHeading :: Location -> Asteroid -> Float
asteroidHeading ship ast@(Asteroid {..}) = atan2 diffX diffY
	where 
		diffX = fst ship - fst aLocation
		diffY = snd ship - snd aLocation

addAsteroid :: World -> [Asteroid] -> [Asteroid]
addAsteroid w@(World{..}) x = newAsteroid rndGen : x

--The init heading is 0, updating a frame later
newAsteroid :: StdGen -> Asteroid
newAsteroid rnd = Asteroid { aHeading = 0, aLocation = (initX, initY) }
	where
		--TODO tweak the values
		initX = fst $ randomR (-500, 500) rnd
		initY = fst $ randomR (-500, 500) (snd (next rnd))

{--
	Using a bounding sphere collision detection. Our bounding sphere has a radius of 10
	The asteroids have 8, therefor if our centers are < 18 appart we have a collision.
	Not <= 18 to correct, just a little, the fact that we are in fact not a sphere/circle
--}
detectColision :: World -> [Asteroid] -> Maybe Location
detectColision w@(World{..}) x = foldr (<||>) Nothing (boundSphere location x)

boundSphere :: Location -> [Asteroid] -> Maybe Asteroid
boundSphere loc (x:xs) = 
	--Calculate the center of both objects
	shipX = fst loc
	shipY = (+) 10 $ snd loc
	astrX = 
