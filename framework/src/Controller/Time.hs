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
											  backdrop = cleanStars   (moveParticles (addStar backdrop rndGen)),
											  rndGen   = snd (next rndGen),
											  bullets  = cleanBullets (moveParticles (addBullet bullets shootAction heading location))
 }

{- Rotates the ship according to the rotateaction -}
newHeading :: Float -> RotateAction -> Float
newHeading r RotateLeft  = r - rotateSpeed
newHeading r RotateRight = r + rotateSpeed
newHeading r _           = r

{- Uses speed and heading to calculate an object's new location -}
newLocation :: Location -> Float -> Float -> Location --location, speed, heading
newLocation (x,y) s r = ((sin rad) * s + x, (cos rad) * s + y)
    where rad = r / 360  * (2 * pi)

{- Add a semi random particle to the list of stars -}
addStar :: [Particle] -> StdGen -> [Particle]
addStar x rnd = (Particle white spd (-90) spd (1000, ypos)) : x
    where ypos = fst (randomR (-1000, 1000) rnd)
          spd  = fst (randomR (1, 5) (snd (next rnd)))

{- Moves all particles in a particle list, can be applied to all types of particles -}
moveParticles :: [Particle] -> [Particle]
moveParticles x = map moveParticle x
    where moveParticle part@(Particle {..}) = part { loc = newLocation loc speed head }

{- Remove stars that have gone out of scene -}
cleanStars :: [Particle] -> [Particle]
cleanStars x = filter clean x
    where clean (Particle {..}) = fst loc > (-1000)

{- Add a bullet at the position of the ship -}
addBullet :: [Particle] -> ShootAction -> Float -> Location -> [Particle]
addBullet x Shoot head loc = (Particle yellow 3 head (3 * movementSpeed) loc) : x
addBullet x _ _ _          = x

{- Remove bullets that have gone out of scene -}
cleanBullets :: [Particle] -> [Particle]
cleanBullets x = filter clean x
    where clean (Particle {..}) = (fst loc > (-1000) && fst loc < (1000)) && (snd loc > (-1000) && snd loc < 1000)

rotateSpeed :: Float
rotateSpeed = 2

movementSpeed :: Float
movementSpeed = 2