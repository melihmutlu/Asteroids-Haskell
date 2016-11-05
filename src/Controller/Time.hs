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
import Config

-- | Time handling

playerSpeed, astroidSpeed , height, width:: Float
playerSpeed = 3
astroidSpeed = 1.5
height = defaultVerticalResolution/2
width = defaultHorizontalResolution/2

timeHandler :: Float -> World -> World
timeHandler time world = world{
						rndGen = mkStdGen $ round (gameTime world),
						player = (Point newX newY, Deg (newAngle d)),
						asteroids = getAsteroidList,
						gameTime = (gameTime world) + time
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

							getAsteroidList :: [(Position,Angle)]
							getAsteroidList 
								| (gameTime world) > (lastEnemy world) + 0.5
									= (asteroids world) ++ [ createAsteroid world ]
								| otherwise = asteroids world


createAsteroid :: World -> (Position,Angle)
createAsteroid world = (Point x y, Deg ang)
						where
							(rnd1,rnd2) = split (rndGen world) 
							(xGen,yGen) = split rnd1
							(x:_) = randomRs ((-1)*width, width) xGen
							(y:_) = randomRs ((-1)*height, height) yGen
							(ang:_) = randomRs (0,360) rnd2