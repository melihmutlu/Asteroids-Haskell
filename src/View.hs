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
					gameStatus})
    = let p1@(Pos x y, Deg d) = player  in
    	pictures $ [ 
    			translate (x) (y) $ rotate d $ color red $ polygon playerShape,
    			color white $ text $ show $ length asteroids] 
    			++ drawAsteroids asteroids
    			++ shootEffect shootAction


playerShape :: Path
playerShape  = [(17,0),(0,7),(0,(-7))]

enemyShape :: Path
enemyShape = [((-10),10),(10,10),(10,(-10)),((-10),(-10))]

drawAsteroids :: [(Position,Angle)] -> [Picture]
drawAsteroids [] = []
drawAsteroids (a:as) = let (Pos x y , Deg ang)=a in
		[translate x y $ rotate ang $ color  (greyN 0.3) $ polygon enemyShape]++ drawAsteroids as

shootAction :: [(Position,Angle)] -> [Picture]
shootAction [] = []
shootAction ((Pos x y ,_):xs) = translate x y $ color yellow $ circle 3 ++ shootAction xs 
