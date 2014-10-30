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
											  backdrop = moveStars (addStar backdrop rndGen)}

newHeading :: Float -> RotateAction -> Float
newHeading r RotateLeft  = r - rotateSpeed
newHeading r RotateRight = r + rotateSpeed
newHeading r _           = r

newLocation :: Location -> Float -> Float -> Location --location, speed, heading
newLocation (x,y) s r = ((sin rad) * s + x, (cos rad) * s + y)
    where rad = r / 360  * (2 * pi)

addStar :: [Particle] -> StdGen -> [Particle]
addStar x rnd = (Particle white 10 90 2 (100, ypos)) : x
    where ypos = fst (randomR (-100, 100) rnd)

moveStars :: [Particle] -> [Particle]
moveStars x = map moveStar x
    where moveStar part@(Particle {..}) = part { loc = newLocation loc speed head }

rotateSpeed :: Float
rotateSpeed = 2

movementSpeed :: Float
movementSpeed = 2