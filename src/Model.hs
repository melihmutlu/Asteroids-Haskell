{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Config

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
        score :: Int ,
        shoots :: [(Position,Angle)],
        lastEnemy :: Float,
        multiplier :: Int,
        multiplierParticles :: [Position],
        gameTime :: Float,
        gameStatus :: GameStatus,
        backgroundLayers :: [[(Float,Float)]]
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight deriving Eq
data MovementAction = NoMovement | Thrust deriving Eq
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
            player = newPlayer $ mkStdGen seed,
            asteroids = [],
            score = 0,
            multiplier = 1,
            multiplierParticles = [],
            shoots = [],
            lastEnemy = 0,
            gameTime = 0,
            gameStatus = On,
            backgroundLayers = getLayers seed
        }

newPlayer :: StdGen -> (Position,Angle)
newPlayer gen = (Pos x y, Deg ang)
        where
            (rnd1,rnd2) = split  gen
            (xGen,yGen) = split rnd1
            (y:_) = randomRs ((-1)*defaultVerticalResolution/2, defaultVerticalResolution/2) xGen
            (x:_) = randomRs ((-1)*defaultHorizontalResolution/2, defaultHorizontalResolution/2) yGen
            (ang:_) = randomRs (0,360) rnd2

getLayers :: Int -> [[(Float,Float)]]
getLayers seed = [(mkLayer g1),(mkLayer g2),(mkLayer g3),(mkLayer g4)]
        where
            gen = mkStdGen seed
            (g1,g2) = split gen
            (g3,g4) = split g1

mkLayer :: StdGen -> [(Float,Float)]
mkLayer gen = zip xList yList
    where
        xList = take 20 $ randomRs ((-1)*defaultHorizontalResolution,defaultHorizontalResolution) g1 
        yList = take 20 $ randomRs ((-1)* defaultVerticalResolution/2, defaultVerticalResolution/2) g2 
        (g1,g2) = split gen