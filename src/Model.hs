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
        shoots :: (Position,Angle)
        lastEnemy :: Float,
        gameTime :: Float,
        gameStatus :: GameStatus
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight deriving Eq
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot deriving Eq
data Angle = Deg Float
data Position = Pos Float Float
data GameStatus = On | Over deriving Eq

initial :: Int -> World
initial seed =
         World{
            rndGen = mkStdGen seed,
            rotateAction = NoRotation,
            movementAction = NoMovement,
            shootAction = DontShoot,
            player = (Pos 0 0, Deg 0),
            asteroids = [],
            score = 0,
            shoots = [],
            lastEnemy = 0,
            gameTime = 0,
            gameStatus = On
        }
