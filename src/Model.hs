{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Config

-- | Game state

data World = World {
        -- Random generator
        rndGen           :: StdGen, -- Random Generator
        -- Event queue
        rotateAction     :: RotateAction, -- Rotate Event
        movementAction   :: MovementAction, -- Movement Event
        shootAction      :: ShootAction,    -- Shoot Event
        -- TODO: add more fields here!
        player  :: (Position,Angle),    -- Player ship coordinates and angle
        asteroids :: [(Position,Angle)], -- List of asteroids' coordinates and angles
        score :: Int , -- Score
        shoots :: [(Position,Angle)], -- List of shoots' coordinates and angles
        lastEnemy :: Float, -- The time that last asteroid was created in
        lastMultiplier :: Float,    -- The time that last multiplier was created in
        multiplier :: Int,  -- Multiplier value
        multiplierParticles :: [Position],  -- Multiplier coordinates
        gameTime :: Float,  -- Total game time
        gameStatus :: GameStatus,   -- Indicates if the game is over or not
        backgroundLayers :: [[(Float,Float)]]   -- Background layer list
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight deriving Eq
data MovementAction = NoMovement | Thrust deriving Eq
data ShootAction    = Shoot      | DontShoot deriving Eq
-- Angle type in Degrees
data Angle = Deg Float
-- Position tyoe contains x and y values
data Position = Pos Float Float
-- Game status
data GameStatus = On | Over deriving Eq

initial :: Int -> World
initial seed =
         World{
            rndGen = mkStdGen seed,
            rotateAction = NoRotation,
            movementAction = NoMovement,
            shootAction = DontShoot,
            player = newPlayer $ mkStdGen seed, -- Initiate in a randım position
            asteroids = [],
            score = 0,
            multiplier = 1,
            multiplierParticles = [],
            shoots = [],
            lastEnemy = 0,
            lastMultiplier=0,
            gameTime = 0,
            gameStatus = On,
            backgroundLayers = getLayers seed   -- Random background layers
        }

-- Returns a random position for player ship
newPlayer :: StdGen -> (Position,Angle)
newPlayer gen = (Pos x y, Deg ang)
        where
            (rnd1,rnd2) = split  gen
            (xGen,yGen) = split rnd1
            -- y coordinate in screen size limits
            (y:_) = randomRs ((-1)*defaultVerticalResolution/2, defaultVerticalResolution/2) xGen 
            -- x coordinate in screen size limits
            (x:_) = randomRs ((-1)*defaultHorizontalResolution/2, defaultHorizontalResolution/2) yGen
            -- Angle betwwen 0-360
            (ang:_) = randomRs (0,360) rnd2

-- Create 4 backgorund layers randomly
getLayers :: Int -> [[(Float,Float)]]
getLayers seed = [(mkLayer g1),(mkLayer g2),(mkLayer g3),(mkLayer g4)]
        where
            -- Random generators for each layer
            gen = mkStdGen seed
            (g1,g2) = split gen
            (g3,g4) = split g1

-- Creates each layer with given random generator
mkLayer :: StdGen -> [(Float,Float)]
mkLayer gen = zip xList yList
    where
        -- Create 20 particle for each layer
        xList = take 20 $ randomRs ((-1)*defaultHorizontalResolution,defaultHorizontalResolution) g1 
        yList = take 20 $ randomRs ((-1)* defaultVerticalResolution/2, defaultVerticalResolution/2) g2 
        (g1,g2) = split gen