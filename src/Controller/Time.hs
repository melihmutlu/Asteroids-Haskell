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

import System.randomRs

import Model
import Config

-- | Time handling

playerSpeed, astroidSpeed , height, width :: Float
-- Player speed
playerSpeed = 3
-- Asteroids speed
astroidSpeed = 1.5
-- A shoot speed
shootSpeed = 10
--Screen configurations
height = defaultVerticalResolution/2
width = defaultHorizontalResolution/2

timeHandler :: Float -> World -> World
timeHandler time world 
	| gameStatus world == Over = initial $ round (gameTime world)  -- If game is over, start again
	| otherwise 			   = 
					world{
						-- New random generator from old generator which is initially created by using time as seed
						rndGen = fst $ split $ (rndGen world) ,
						player = updatePosition world,
						asteroids = newAsteroidList,
						-- Calculation of new score 
						score  = (score world) 
							+ ((length $ movedAsteroidList) - (length newAsteroidList))*(multiplier world),
						lastEnemy = newLastEnemy,
						lastMultiplier = newLastMult,
						multiplier = newMultiplier,
						multiplierParticles = isMultShot world $ newMultList,
						shoots = shootMovement world,
						gameTime = (gameTime world) + time,
						gameStatus = gameStatusCheck (player world) (asteroids world),
						backgroundLayers = updateLayers 4 (backgroundLayers world)
						}
						where	
							-- The time that last asteroid was created						
							newLastEnemy  
								|(gameTime world) > (lastEnemy world) + 0.5 =
									(gameTime world) + time
								| otherwise = lastEnemy world

							-- The time that last multiplier particle was created
							newLastMult  
								| (gameTime world) > (lastMultiplier world) + 1 
									= (gameTime world) + time
								| otherwise = lastMultiplier world
							
							-- Asteroid list after movement and addition of new one
							movedAsteroidList = moveAsteroids (player world) getAsteroidList
							-- Asteroid list after checking if any of them is shot by player
							newAsteroidList = isEnemyShot world $ movedAsteroidList

							-- New multiplier particle list
							newMultList = multiplierCheck (player world) getMultiplierList
							
							-- New multiplier values. Checks how many particle is taken by player
							newMultiplier = (multiplier world) + (length getMultiplierList)
											- (length newMultList)

							-- Returns new multiplier list if it is the time
							getMultiplierList :: [(Position)]
							getMultiplierList
								| (gameTime world) > (lastMultiplier world) + 1 -- create a partile in every second
									=  createMultiplier world
								| otherwise = multiplierParticles world

							-- Returns new asteroid list if it is the time
							getAsteroidList :: [(Position,Angle)]
							getAsteroidList 
								| (gameTime world) > (lastEnemy world) + 0.5 	-- create an asteroid in every 0.5 second
									=  asteroids world ++ [ createAsteroid world ]
								| otherwise = asteroids world

-- Updates the position of player ship
updatePosition :: World -> (Position,Angle)
updatePosition world 	 
					| movementAction world == Thrust = (Pos (newX 2.5) (newY 2.5), Deg (newAngle d)) 	-- Move faster
					| otherwise = (Pos (newX 1) (newY 1), Deg (newAngle d))
					where
						(Pos x y, Deg d) = player world 	-- Current position
						-- New x coordinate
						newX factor	
							-- Can not move if player hits the edges
							| x + cos (degToRad d)*playerSpeed*factor >= width = x
							| x + cos (degToRad d)*playerSpeed*factor <= (-1)*width = x
							|otherwise = x + cos (degToRad d)*playerSpeed*factor
						-- New y coordinate
						newY factor	
							-- Can not move if player hits the edges
							| y - sin (degToRad d)*playerSpeed*factor >= height = y
							| y - sin (degToRad d)*playerSpeed*factor <= (-1)*height = y
							|otherwise = y - sin (degToRad d)*playerSpeed*factor
						-- New angle
						newAngle :: Float -> Float
						newAngle d
							-- Rotates left
							| (rotateAction world) == RotateLeft =  if d-5 < 0 then 365-d else d-5
							-- Rotates right
							| (rotateAction world) == RotateRight = if d+5 > 380 then d-355 else d+5
							| otherwise = d


-- Creates new multiplies particle in a random position
createMultiplier :: World -> [(Position)]
createMultiplier world =(multiplierParticles world) ++ [(Pos x y)]
						where
							(rnd1,rnd2) = split (rndGen world) 
							-- Random coordinates
							(x:_) = randomRs ((-1)*width, width) rnd1
							(y:_) = randomRs ((-1)*height, height) rnd2

-- Creates new multiplies particle in a random position
createAsteroid :: World -> (Position,Angle)
createAsteroid world = (Pos x y, Deg ang)
						where
							(rnd1,rnd2) = split (rndGen world) 
							(xGen,yGen) = split rnd1
							-- Random coordinates and angle
							(x:_) = randomRs ((-1)*width, width) xGen
							(y:_) = randomRs ((-1)*height, height) yGen
							(ang:_) = randomRs (0,360) rnd2

-- Move each asteroid particle to a new position according to its angle and current position
moveAsteroids :: (Position,Angle) -> [(Position,Angle)] -> [(Position,Angle)]
moveAsteroids _ [] = []
moveAsteroids player@(Pos px py , _) (a:as) = 
								[(Pos (x+vx) (y+vy), Deg (d+18))] ++ moveAsteroids player as
							where
								(Pos x y , Deg d) = a
								h = sqrt $ (px-x)*(px-x) + (py-y)*(py-y)
								vx = ((px-x)/h) * astroidSpeed
								vy = ((py-y)/h) * astroidSpeed

-- Determines if player gain a multiplier
multiplierCheck :: (Position,Angle) -> [Position] -> [Position]
multiplierCheck _ [] = []
multiplierCheck player (m:ms) 
	-- The distance between player and an particle is too small, there is a collision
	| distance player (m, Deg 0) <= 10 = multiplierCheck player ms
	| otherwise = [m] ++ multiplierCheck player ms

-- Determines new game status
gameStatusCheck :: (Position,Angle) -> [(Position,Angle)] -> GameStatus 
gameStatusCheck _ [] = On
gameStatusCheck player (a:as)
	-- The distance between player and an asteroid is too small, there is a collision
	| (distance player a) <= 17 = Over
	| otherwise = gameStatusCheck player as

-- Determines new positions for shoot particles
shootMovement :: World -> [(Position,Angle)]
shootMovement world 
	| action == Shoot  = move firelist ++ [(Pos px py, Deg d)] -- Create new shoot, move older ones
	| otherwise = move firelist
	where
		(Pos px py , Deg d) = player world -- player ship coordinates
		action = shootAction world 	-- Shoot Action
		firelist = shoots world -- Shoots list
		-- Calculate new positions for each shoot
		move :: [(Position,Angle)] -> [(Position,Angle)]
		move [] = []
		move ((Pos x y, Deg d):xs)
			-- If it is off the screen, remove it
			| newX > width || newX < (-1)*width =  move xs 	
			| newY > height || newY < (-1)*height = move xs
			| otherwise =  [(Pos newX newY , Deg d)] ++ move xs
			where 
				newX = x + cos (degToRad d)*shootSpeed
				newY = y - sin (degToRad d)*shootSpeed

-- Moves particles in the background layers from left to right
updateLayers :: Float -> [[(Float,Float)]] -> [[(Float,Float)]]
updateLayers _ [] = []
updateLayers n (l:ls) = [update n l] ++ updateLayers (n-1) ls
	where
		-- Updates positions
		update:: Float -> [(Float,Float)] -> [(Float,Float)]
		update _  [] = []
		update n ((x,y):xs) = [(newX,y)] ++ (update n xs)  
			where
				-- Moves by speed of 2*n (n is layer number)
				newX 
					| x+2*n > width = x+2*n-2*width -- If it reaches to end, send back it to start 
					| otherwise = x+2*n

-- Checks if a multiplier particle is shot
isMultShot :: World -> [(Position)] -> [(Position)]
isMultShot _ [] = []
isMultShot world (a:as) 
	| checkCondition (shoots world) a = isMultShot world as 	-- If it is shot, remove it from the list
	| otherwise = [a] ++ isMultShot world as 	-- Otherwise keep it in the list
	where
		-- Checks for a particular particle
		checkCondition :: [(Position,Angle)] -> (Position) -> Bool
		checkCondition [] _ = False
		checkCondition (f:fs) mult 
			| (distance f (mult, Deg 0)) <= 15 = True	
			| otherwise = checkCondition fs mult	

-- Checks if an asteroid is shot
isEnemyShot :: World -> [(Position,Angle)] -> [(Position,Angle)]
isEnemyShot _ [] = []
isEnemyShot world (a:as) 
	| checkCondition (shoots world) a = isEnemyShot world as 	-- If asteroid is shot, remove it from the list
	| otherwise = [a] ++ isEnemyShot world as 	-- otherwise keep it in the list
	where
		-- Checks for a particular asteroid
		checkCondition :: [(Position,Angle)] -> (Position,Angle) -> Bool
		checkCondition [] _ = False
		checkCondition (f:fs) asteroid 
			| (distance f asteroid) <= 17 = True 	-- Given asteroid is shot
			| otherwise = checkCondition fs asteroid

-- Returns distance between two position
distance :: (Position,Angle) -> (Position,Angle) -> Float
distance (Pos px py, _) (Pos ax ay,_) = 
		sqrt ((px-ax)*(px-ax) + (py-ay)*(py-ay)) 