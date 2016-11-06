{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Model

-- Draws the current world
draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution 
		world@(World{rndGen,
					rotateAction,
					movementAction,
					shootAction,
					player,
					asteroids,
					score,
					multiplier,
					multiplierParticles,
					shoots,
					lastEnemy,
					lastMultiplier,
					gameTime,
					gameStatus,
					backgroundLayers})
    = let p1@(Pos x y, Deg d) = player  in
    	pictures $ 
    			backgroundEffect 4 backgroundLayers	-- Parallal Srolling
    			++ tailEffect player movementAction -- Thrust Effect
    			++[ 
    			color green $ rectangleWire horizontalResolution verticalResolution, -- Screen limits
    			translate (x) (y) $ rotate d $ color red $ polygon playerShape, -- Player ship
    			translate ((-1)*horizontalResolution/2+20) (verticalResolution/2-40) -- Score and Multiplier
    				$ scale 0.2 0.2
    				$ color white $ text $ "Score : " ++ show score
    					++ "     Multiplier : x" ++ show multiplier
    			]
    			++ drawAsteroids asteroids 	-- Asteroids
    			++ showMultipliers multiplierParticles	-- Multipliers
    			++ shootEffect shoots	-- Shoots


-- Base player ship coordinates
playerShape :: Path
playerShape  = [(0,0),((-15),5),((-15),(-5))]

-- Base asteroid coordinates
enemyShape :: Path
enemyShape = [((-10),10),(10,10),(10,(-10)),((-10),(-10))]

-- Takes asteroid coordinate list, returns Picture list which contains asteroid shapes
drawAsteroids :: [(Position,Angle)] -> [Picture]
drawAsteroids [] = []
drawAsteroids (a:as) = let (Pos x y , Deg ang)=a in
		[translate x y $ rotate ang $ color (dim cyan) $ polygon enemyShape]++ drawAsteroids as

-- Takes shoots coordinate list, returns Picture list 
shootEffect :: [(Position,Angle)] -> [Picture]
shootEffect [] = []
shootEffect ((Pos x y ,_):xs) = [translate x y $ color yellow $ circleSolid 2] ++ shootEffect xs 

-- Takes layer number and layer coordinates, returns Pictue list which contains layers
backgroundEffect :: Float -> [[(Float,Float)]] -> [Picture]
backgroundEffect _ [] = []
backgroundEffect n (l:ls) = showEffect n l ++ backgroundEffect (n-1) ls
	where
		-- Creates a Picture list for layer number n
		showEffect :: Float -> [(Float,Float)] -> [Picture]
		showEffect _ [] = []
		showEffect n ((x,y):xs) = 
			[translate x y $ color white $ circleSolid (n/3)] ++ showEffect n xs

-- If MovementAction is Thrust, returns tail
tailEffect :: (Position,Angle) -> MovementAction -> [Picture]
tailEffect (Pos px py, Deg d) action 
	| action == Thrust = let 
							x = px - cos (degToRad d)*20 -- x coordinate for starting point of the tail
							y = py + sin (degToRad d)*20 -- y coordinate fot starting point of the tail
						in
						-- Tail Shapes
						 [ translate x y $ color orange $ circleSolid 5,
							translate (x+1) (y+1) $ color blue $ circleSolid 5,
							translate (x+1.5) (y+1.5) $ color chartreuse $ circleSolid 5,
							translate (x-1) (y-1) $ color rose $ circleSolid 5,
							translate (x-1.5) (y-1.5) $ color aquamarine $ circleSolid 5]
						
	| otherwise = []

-- Takes multiplier list and returns Picture list which contains multiplier shapes
showMultipliers :: [Position] -> [Picture]
showMultipliers [] = []
showMultipliers ((Pos x y):ms) = 
	[ translate x y $ color blue $ circleSolid 10] ++ showMultipliers ms