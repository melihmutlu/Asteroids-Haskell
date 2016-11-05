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
					lastEnemy,
					gameTime,
					gameStatus})
    = let p1@(Point x y, Deg d) = player  in
    	pictures $ [ 
    			translate (x) (y) $ rotate d $ color red $ polygon playerShape,
    			color white $ text $ show gameTime] ++ drawAsteroids asteroids


playerShape :: Path
playerShape  = [(-15,0),(0,10),(35,0),(0,(-10))]

enemyShape :: Path
enemyShape = [(0,0), (-18,-7), (-18,-19), (0,-26), (18,-19), (18,-7)]

drawAsteroids :: [(Position,Angle)] -> [Picture]
drawAsteroids [] = []
drawAsteroids (a:as) = let (Point x y , Deg ang)=a in
		[translate x y $ rotate ang $ color  (greyN 0.3) $ polygon enemyShape]++ drawAsteroids as