{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random

-- | Game state

data World = World {
        -- Random generator
        rndGen           :: StdGen,
        -- Event queue
        rotateAction     :: RotateAction,
        movementAction   :: MovementAction,
        shootAction      :: ShootAction
        -- TODO: add more fields here!
        -- Coordinates
        player  :: (PlayerLoc, Angle)
        enemies :: [(EnemyLoc, Angle)]
        score :: Int 
        gameStatus :: GameStatus
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot
data Angle = Int
data Point = (Float,Float)
data PlayerLoc = Player (Point, Point, Point, Point) 
data EnemyLoc = Enemy (Point,Point,Point,Point)
data GameStatus = On | Over

initial :: Int -> World
initial seed = error "implement Model.initial!"

