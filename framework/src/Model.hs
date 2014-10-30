{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random

-- | Game state

data World = World {
        -- Random generator
        rndGen           	:: StdGen,
        -- Event queue
        rotateAction     	:: RotateAction,
        movementAction   	:: MovementAction,
        shootAction 		:: ShootAction,
        -- TODO: add more fields here!

        -- Keep both times for score calculation.
        startTime 			:: Float,
        currentTime 		:: Float,

        -- SpaceShip book keeping
		location 			:: Location,
		heading             :: Float
    }

    
data RotateAction   = NoRotation | RotateLeft | RotateRight
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot

type Location 		= (Float, Float)

-- Time is the seed, aswell as the startTime and currentTIme. 
-- Not really accurate, but for now good enough
initial :: Int -> World
initial seed = World (mkStdGen seed) NoRotation Thrust DontShoot floatTime floatTime (0,0) 0
	where floatTime = fromIntegral seed :: Float