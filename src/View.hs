{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Model

-- | Drawing

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution 
		world@(World{rndGen,
					rotateAction,
					movementAction,
					shootAction,
					player,
					asteroids,
					score,
					shoots,
					lastEnemy,
					gameTime,
					gameStatus,
					backgroundLayers})
    = let p1@(Pos x y, Deg d) = player  in
    	pictures $ 
    			backgroundEffect 4 backgroundLayers
    			++ tailEffect player movementAction
    			++[ 
    			color green $ rectangleWire horizontalResolution verticalResolution,
    			translate (x) (y) $ rotate d $ color red $ polygon playerShape
    			text $ show $ score world
    			] 
    			++ drawAsteroids asteroids
    			++ shootEffect shoots


playerShape :: Path
playerShape  = [(0,0),((-15),5),((-15),(-5))]

enemyShape :: Path
enemyShape = [((-10),10),(10,10),(10,(-10)),((-10),(-10))]

drawAsteroids :: [(Position,Angle)] -> [Picture]
drawAsteroids [] = []
drawAsteroids (a:as) = let (Pos x y , Deg ang)=a in
		[translate x y $ rotate ang $ color (dim cyan) $ polygon enemyShape]++ drawAsteroids as

shootEffect :: [(Position,Angle)] -> [Picture]
shootEffect [] = []
shootEffect ((Pos x y ,_):xs) = [translate x y $ color yellow $ circleSolid 2] ++ shootEffect xs 

backgroundEffect :: Float -> [[(Float,Float)]] -> [Picture]
backgroundEffect _ [] = []
backgroundEffect n (l:ls) = showEffect n l ++ backgroundEffect (n-1) ls
	where
		showEffect :: Float -> [(Float,Float)] -> [Picture]
		showEffect _ [] = []
		showEffect n ((x,y):xs) = 
			[translate x y $ color white $ circleSolid (n/3)] ++ showEffect n xs

tailEffect :: (Position,Angle) -> MovementAction -> [Picture]
tailEffect (Pos px py, Deg d) action 
	| action == Thrust = let 
							x = px - cos (degToRad d)*20
							y = py + sin (degToRad d)*20
						in
						 [ translate x y $ color orange $ circleSolid 5,
							translate (x+1) (y+1) $ color blue $ circleSolid 5,
							translate (x+1.5) (y+1.5) $ color chartreuse $ circleSolid 5,
							translate (x-1) (y-1) $ color rose $ circleSolid 5,
							translate (x-1.5) (y-1.5) $ color aquamarine $ circleSolid 5]
						
	| otherwise = []