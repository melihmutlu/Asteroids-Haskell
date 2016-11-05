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
    			++[ 
    			color green $ rectangleWire horizontalResolution verticalResolution,
    			translate (x) (y) $ rotate d $ color red $ polygon playerShape
    			] 
    			++ drawAsteroids asteroids
    			++ shootEffect shoots


playerShape :: Path
playerShape  = [(0,0),((-17),7),((-17),(-7))]

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