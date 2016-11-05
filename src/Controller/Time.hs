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
astroidSpeed = 1.8
height = defaultVerticalResolution/2
width = defaultHorizontalResolution/2

timeHandler :: Float -> World -> World
timeHandler time world = world{
						rndGen = fst $ split $ (rndGen world) ,
						player = (Point newX newY, Deg (newAngle d)),
						asteroids = moveAsteroids (player world) getAsteroidList,
						lastEnemy = newLastEnemy,
						gameTime = (gameTime world) + time
						}
						where
							(Point x y , Deg d) = player world
							newX = x + cos (degToRad d)*playerSpeed
							newY = y - sin (degToRad d)*playerSpeed
							
							newLastEnemy  = if (gameTime world) > (lastEnemy world) + 0.5 
								then (gameTime world) + time
								else lastEnemy world

							newAngle :: Float -> Float
							newAngle d
								| (rotateAction world) == RotateLeft =  if d-5 < 0 then 365-d else d-5
								| (rotateAction world) == RotateRight = if d+5 > 380 then d-355 else d+5
								| otherwise = d

							getAsteroidList :: [(Position,Angle)]
							getAsteroidList 
								| (gameTime world) > (lastEnemy world) + 0.5
									=  asteroids world ++ [ createAsteroid world ]
								| otherwise = asteroids world


createAsteroid :: World -> (Position,Angle)
createAsteroid world = (Point x y, Deg ang)
						where
							(rnd1,rnd2) = split (rndGen world) 
							(xGen,yGen) = split rnd1
							(x:_) = randomRs ((-1)*width, width) xGen
							(y:_) = randomRs ((-1)*height, height) yGen
							(ang:_) = randomRs (0,360) rnd2

moveAsteroids :: (Position,Angle) -> [(Position,Angle)] -> [(Position,Angle)]
moveAsteroids _ [] = []
moveAsteroids player@(Point px py , _) (a:as) = 
								[(Point (x+vx) (y+vy), Deg d)] ++ moveAsteroids player as
							where
								(Point x y , Deg d) = a
								h = sqrt $ (px-x)*(px-x) + (py-y)*(py-y)
								vx = ((px-x)/h) * astroidSpeed
								vy = ((py-y)/h) * astroidSpeed


gameStatusCheck :: World -> GameStatus 
gameStatusCheck world
	| isCollision (player world) (asteroids world) = Over
	| otherwise = On


isCollision :: (Position,Angle) -> [(Position,Angle)] -> Bool
isCollision _ [] = True
isCollision player@(Point px py, _) ((Point ax ay ,_):as) = 
		checkIntersection player enemy && isCollision player as
	where
		player = playerPath px py
		enemy = enemyPath ax ay

checkIntersection :: Path -> Path -> Bool
checkIntersection [p] [a] = False
checkIntersection (p1:ps) (a1: as) =
		if  intersectSegSeg p1 p2 a1 a2 == Nothing then
			checkIntersection ps as
		else
			True
	where 
		(p2:_) = ps
		(a2:_) = as

playerPath :: Float -> Float -> Path
playerPath x y = [(x,y),(x+10,y+15),(x,y+15+35),(x-10,y+15)]

enemyPath :: Point -> Float -> Path
enemyPath x y= [(x,y), (x-18,y-7), (x-18,y-7-12), (x,y-2*7-12), (x+18,y-7-12), (x+18,y-7)]
