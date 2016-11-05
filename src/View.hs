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
					gameStatus})
    = let p1@(Point x y, Deg d) = player in
    	pictures[ translate (x) (y) $ rotate d $ color red $ polygon playerShape]


playerShape :: Path
playerShape  = [(-15,0),(0,10),(35,0),(0,(-10))]