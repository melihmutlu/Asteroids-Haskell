{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>))

import Data.List

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Point

import System.Random

import Model
import Config

-- | Time handling

playerSpeed, astroidSpeed , height, width :: Float
playerSpeed = 3
astroidSpeed = 1.5
shootSpeed = 10
height = defaultVerticalResolution/2
width = defaultHorizontalResolution/2

timeHandler :: Float -> World -> World
timeHandler time world 
	| gameStatus world == Over = initial $ round (gameTime world) 
	| otherwise 			   = 
					world{
						rndGen = fst $ split $ (rndGen world) ,
						player = updatePosition world,
						asteroids = isEnemyShot world $ moveAsteroids (player world) getAsteroidList,
						lastEnemy = newLastEnemy,
						shoots = shootMovement world,
						gameTime = (gameTime world) + time,
						gameStatus = gameStatusCheck (player world) (asteroids world),
						backgroundLayers = updateLayers 4 (backgroundLayers world)
						}
						where							
							newLastEnemy  = if (gameTime world) > (lastEnemy world) + 0.5 
								then (gameTime world) + time
								else lastEnemy world

							getAsteroidList :: [(Position,Angle)]
							getAsteroidList 
								| (gameTime world) > (lastEnemy world) + 0.5
									=  asteroids world ++ [ createAsteroid world ]
								| otherwise = asteroids world


updatePosition :: World -> (Position,Angle)
updatePosition world 	 
					| movementAction world == Thrust = (Pos (newX 2.5) (newY 2.5), Deg (newAngle d))
					| otherwise = (Pos (newX 1) (newY 1), Deg (newAngle d))
					where
						(Pos x y, Deg d) = player world
						newX factor	
							| x + cos (degToRad d)*playerSpeed*factor >= width = x
							| x + cos (degToRad d)*playerSpeed*factor <= (-1)*width = x
							|otherwise = x + cos (degToRad d)*playerSpeed*factor
						newY factor	
							| y - sin (degToRad d)*playerSpeed*factor >= height = y
							| y - sin (degToRad d)*playerSpeed*factor <= (-1)*height = y
							|otherwise = y - sin (degToRad d)*playerSpeed*factor
						newAngle :: Float -> Float
						newAngle d
							| (rotateAction world) == RotateLeft =  if d-5 < 0 then 365-d else d-5
							| (rotateAction world) == RotateRight = if d+5 > 380 then d-355 else d+5
							| otherwise = d

createAsteroid :: World -> (Position,Angle)
createAsteroid world = (Pos x y, Deg ang)
						where
							(rnd1,rnd2) = split (rndGen world) 
							(xGen,yGen) = split rnd1
							(x:_) = randomRs ((-1)*width, width) xGen
							(y:_) = randomRs ((-1)*height, height) yGen
							(ang:_) = randomRs (0,360) rnd2

moveAsteroids :: (Position,Angle) -> [(Position,Angle)] -> [(Position,Angle)]
moveAsteroids _ [] = []
moveAsteroids player@(Pos px py , _) (a:as) = 
								[(Pos (x+vx) (y+vy), Deg (d+18))] ++ moveAsteroids player as
							where
								(Pos x y , Deg d) = a
								h = sqrt $ (px-x)*(px-x) + (py-y)*(py-y)
								vx = ((px-x)/h) * astroidSpeed
								vy = ((py-y)/h) * astroidSpeed


gameStatusCheck :: (Position,Angle) -> [(Position,Angle)] -> GameStatus 
gameStatusCheck _ [] = On
gameStatusCheck player (a:as)
	| (distance player a) <= 17 = Over
	| otherwise = gameStatusCheck player as


distance :: (Position,Angle) -> (Position,Angle) -> Float
distance (Pos px py, _) (Pos ax ay,_) = 
		sqrt ((px-ax)*(px-ax) + (py-ay)*(py-ay)) 

shootMovement :: World -> [(Position,Angle)]
shootMovement world 
	| action == Shoot  = move firelist ++ [(Pos px py, Deg d)]
	| otherwise = move firelist
	where
		(Pos px py , Deg d) = player world
		action = shootAction world
		firelist = shoots world 
		move :: [(Position,Angle)] -> [(Position,Angle)]
		move [] = []
		move ((Pos x y, Deg d):xs)
			| newX > width || newX < (-1)*width =  move xs
			| newY > height || newY < (-1)*height = move xs
			| otherwise =  [(Pos newX newY , Deg d)] ++ move xs
			where 
				newX = x + cos (degToRad d)*shootSpeed
				newY = y - sin (degToRad d)*shootSpeed

updateLayers :: Float -> [[(Float,Float)]] -> [[(Float,Float)]]
updateLayers _ [] = []
updateLayers n (l:ls) = [update n l] ++ updateLayers (n-1) ls
	where
		update:: Float -> [(Float,Float)] -> [(Float,Float)]
		update _  [] = []
		update n ((x,y):xs) = [(newX,y)] ++ (update n xs)  
			where
				newX 
					| x+2*n > width = x+2*n-2*width
					| otherwise = x+2*n
	

isEnemyShot :: World -> [(Position,Angle)] -> [(Position,Angle)]
isEnemyShot _ [] = []
isEnemyShot world (a:as) 
	| checkCondition (shoots world) a = isEnemyShot world as
	| otherwise = [a] ++ isEnemyShot world as
	where
		checkCondition :: [(Position,Angle)] -> (Position,Angle) -> Bool
		checkCondition [] _ = False
		checkCondition (f:fs) asteroid 
			| (distance f asteroid) <= 17 = True
			| otherwise = checkCondition fs asteroid