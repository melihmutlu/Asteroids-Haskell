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
        shootAction      :: ShootAction,
        -- TODO: add more fields here!
        -- Coordinates
        player  :: (Position,Angle),
        asteroids :: [(Position,Angle)],
        score :: Float ,
        lastEnemy :: Float,
        gameTime :: Float,
        gameStatus :: GameStatus
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight deriving Eq
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot
data Angle = Deg Float
data Position = Point Float Float
data GameStatus = On | Over

initial :: Int -> World
initial seed =
         World{
            rndGen = mkStdGen seed,
            rotateAction = NoRotation,
            movementAction = NoMovement,
            shootAction = DontShoot,
            player = (Point 0 0, Deg 0),
            asteroids = [],
            score = 0,
            lastEnemy = 0,
            gameTime = 0,
            gameStatus = On
        }
