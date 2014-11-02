{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss


-- | Game state

data World = World {
        -- Random generator
        rndGen              :: StdGen,
        -- Event queue
        rotateAction        :: RotateAction,
        movementAction      :: MovementAction,
        shootAction         :: ShootAction,

        -- Keep both times for score calculation.
        startTime           :: Float,
        currentTime         :: Float,
		score               :: Int,
		multiplier			:: Int, 
		gems 				:: [Particle],

        -- SpaceShip book keeping
        location            :: Location,
        heading             :: Float,
        
        -- Particles and stuff
        backdrop            :: [Particle],
        bullets             :: [Particle],

		asteroids 			:: [Asteroid],

		--Impact Debris book keeping
		debris 		 		:: [Debris],
		trail               :: [Debris]
    }

    
data RotateAction   = NoRotation | RotateLeft | RotateRight
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot

type Location       = (Float, Float)
data Particle       = Particle {
        color      :: Color,
        size       :: Float,
        head       :: Float,
        speed      :: Float,
        loc        :: Location
    }

data Asteroid 		= Asteroid {
		aHeading   :: Float, 
		aLocation  :: Location
}

data Debris         = Debris {
        dLocation  :: Location,
        dHeading   :: Float,
        dAge       :: Int
    }

-- Time is the seed, aswell as the startTime and currentTIme. 
-- Not really accurate, but for now good enough
initial :: Int -> World
initial seed = World (mkStdGen seed) NoRotation NoMovement DontShoot floatTime floatTime 0 1 [](0,0) 0 [] [] [] [] []
    where floatTime = fromIntegral seed :: Float

{--
	the random generator
	location of impact, for debris drawing
	Impact Time. 
--}
initAfterImpact :: StdGen -> Location -> Float -> Int -> [Particle] -> World
initAfterImpact gen impactLoc time score stars
			= World gen NoRotation NoMovement DontShoot time time score 1 [] (0,0) 0 stars [][] (createDebris impactLoc) []

createDebris :: Location -> [Debris]
createDebris loc = [(Debris loc h 0) | h <- [0..359]]