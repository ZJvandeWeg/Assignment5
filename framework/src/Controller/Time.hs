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
import Data.Maybe

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {..}) |collisionLoc == Nothing = 
												world { currentTime = time,
                                              	heading = newHeading heading rotateAction,
                                              	location = newLocation location movementSpeed heading,
											  	backdrop = cleanStars   (moveParticles (addStar backdrop rndGen)),
											  	rndGen   = snd (next rndGen),
											  	bullets  = newBullets,
											  	asteroids = newAsteroids,
											  	debris = cleanDebris $ moveDebris debris
 												}
 									| otherwise = initAfterImpact rndGen (fromJust collisionLoc) time
 									where 
 										collisionLoc = detectCollision world
 										--First move both the bullets and asteroids, then calculate hits etc
 										moveAndCleanBullets = cleanBullets (moveParticles (addBullet bullets shootAction heading location))
 										moveAndCleanAsteroids = moveAsteroids world (addAsteroid world)
 										bulletsVsAsteroids  = bulletIteration moveAndCleanBullets moveAndCleanAsteroids
 										-- After hit detection etc 
 										newBullets = fst bulletsVsAsteroids
 										newAsteroids = snd bulletsVsAsteroids

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
rotateSpeed = 4

movementSpeed :: Float
movementSpeed = 3


{--
	Asteroid handling
--}

asteroidMovementSpeed :: Float
asteroidMovementSpeed = 2.1

asteroidRotationSpeed :: Float
asteroidRotationSpeed = 1.4

--Does this work?
spawnRate :: Int
spawnRate = 30041

moveAsteroids :: World -> [Asteroid] -> [Asteroid]
moveAsteroids world@(World{..}) = map moveAsteroid 
	where moveAsteroid astr@(Asteroid{..}) = astr { aLocation = newLocation aLocation asteroidMovementSpeed heading'}
		where heading' = asteroidHeading location astr

asteroidHeading :: Location -> Asteroid -> Float
asteroidHeading ship ast@(Asteroid {..}) | angleInPi > 0 = angleInPi *360 / (2*pi)
										 | otherwise = (2*pi + angleInPi) * 360 / (2*pi)
	where 
		diffX = fst ship - fst aLocation
		diffY = snd ship - snd aLocation
		angleInPi = atan2 diffX diffY --atan2 gives 0 <= value < pi

--Adds an asteroid per 3 seconds
addAsteroid :: World  -> [Asteroid]
addAsteroid w@(World{..})   | (chance rndGen) == 1 = (newAsteroid rndGen) : asteroids
                            | otherwise             = asteroids
    where chance rnd = fst (randomR (0 :: Int, 20 :: Int) rnd)

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

detectCollision :: World -> Maybe Location
detectCollision w@(World{..}) = foldr (<||>) Nothing (map (boundSphere location) asteroids)

--For lazy eval
(<||>) :: Maybe a -> Maybe a -> Maybe a
Nothing <||> x = x
x 		<||> _ = x

--If output is Just Location it needs to be translated from the ship
boundSphere :: Location -> Asteroid -> Maybe Location
boundSphere loc astr@(Asteroid{..}) | c >= 15 = Nothing
									| otherwise = Just (explosionLoc)
	where
		diffX = fst loc - fst aLocation
		diffY = (-) (10 + snd loc) $ snd aLocation
		c 	  = sqrt(diffX^2 + diffY^2) -- c^2 = a^2 + b^2
		explosionLoc = ((fst loc) + (diffX / 2), (snd loc) + (diffY / 2)) --About where the collision is

moveDebris :: [(Location, Float)] -> [(Location, Float)]
moveDebris = map moveDebris'
	where moveDebris' (l,h) = (newLocation l debrisSpeed h, h)

-- Not an optimal solution, though real fast. And in the beginning of the game, not much asteroids are there anyway
cleanDebris :: [(Location, Float)] -> [(Location, Float)]
cleanDebris d@(x:_) | (snd $ fst x) > 1000 	= []
					| otherwise 			= d
cleanDebris _ 								= []

debrisSpeed :: Float
debrisSpeed = 5

{--
	Main idea for the section below;
	Per bullet, iterate over all asteroids to remove hit asteroids. 
	Unlike the executable provided, we only remove one asteroid instead of all it hit? 
	(At least it seems, you guys did this.)
	Worked with tuples, to save an extra couple off fitlers, which saves an o(n) operation.
--}

-- If bulletVsAsteroid tells us we've hit an object, we remove the bullet and the asteroid
-- else, we check for the next bullet
bulletIteration :: [Particle] -> [Asteroid] -> ([Particle],[Asteroid])
bulletIteration [] 		 x 		= ([], x)
bulletIteration x 		 [] 	= (x, [])
bulletIteration (x:xs) 	 a 		| bulletHitObject = hitRecursiveCase
								| otherwise 	  = normalRecursiveCase
	where
		hitRecursiveCase = (fst hitIteration, snd oneBulletCheck)
		normalRecursiveCase = (x: (fst noHitIteration), snd noHitIteration)
		
		bulletHitObject = fst oneBulletCheck
		oneBulletCheck 	= bulletVsAsteroids x (False,a)

		hitIteration = bulletIteration xs (snd oneBulletCheck) 
		noHitIteration = bulletIteration xs a


--For one bullet, iterate over all asteroids. In case of a hit, we remove the asteroid
-- and return the tail from that moment and concat it with the previous asteroids
-- else everything is returned as the way it was. 
-- Hit? -> (True, filtered list) -- Otherwise, we had to filter it, another O(n) operation
bulletVsAsteroids :: Particle -> (Bool,[Asteroid]) -> (Bool, [Asteroid])
bulletVsAsteroids _ (False,[])	= (False, [])
bulletVsAsteroids b (_,x:xs) 	| distanceBulletAsteroid b x < 11 	= (True, xs)
								| otherwise = (fst recursiveCall, x : (snd recursiveCall))
			where 
				recursiveCall = bulletVsAsteroids b (False,xs)

distanceBulletAsteroid :: Particle -> Asteroid -> Float
distanceBulletAsteroid p@(Particle{..}) a@(Asteroid{..}) = sqrt(diffX^2 + diffY^2)
	where
		diffX = fst loc - fst aLocation
		diffY = snd loc - snd aLocation