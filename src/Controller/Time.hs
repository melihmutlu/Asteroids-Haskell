{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>))

import Data.List

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import System.Random

import Model

-- | Time handling

playerSpeed, astroidSpeed :: Float
playerSpeed = 3
astroidSpeed = 1.5

timeHandler :: Float -> World -> World
timeHandler time world = world{
						player = (Point newX newY, Deg (newAngle d))
						}
						where
							(Point x y , Deg d) = player world
							newX = x + cos (degToRad d)*playerSpeed
							newY = y - sin (degToRad d)*playerSpeed
							newAngle :: Float -> Float
							newAngle d
								| (rotateAction world) == RotateLeft =  if d-5 < 0 then 365-d else d-5
								| (rotateAction world) == RotateRight = if d+5 > 380 then d-355 else d+5
								| otherwise = d

						