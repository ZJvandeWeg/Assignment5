{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>))

import Data.List

import System.Random

import Model

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {..}) = world { currentTime = time, heading = newHeading heading rotateAction, location = newLocation location heading}
		
newHeading :: Float -> RotateAction -> Float
newHeading r RotateLeft  = r - rotateSpeed
newHeading r RotateRight = r + rotateSpeed
newHeading r _           = r

newLocation :: Location -> Float -> Location
newLocation (x,y) r = ((sin rad) * movementSpeed + x, (cos rad) * movementSpeed + y)
    where rad = r / 360  * (2 * pi)

rotateSpeed :: Float
rotateSpeed = 2

movementSpeed :: Float
movementSpeed = 2