{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

import System.IO
import System.Linux.Input.Event
import Control.Concurrent.STM
import Control.Concurrent
import Control.Lens
import Control.Applicative
import Data.Int
import Linear
import qualified MercuryController as MM
import Control.Monad (forever)
import Control.Monad.State
import Data.Traversable as T
import Data.Foldable as F

newtype Position = Pos Int
                 deriving (Show, Eq, Ord, Num, Integral, Real, Enum)

data NavAxis = NavAxis { _position :: Position
                       , _velocity :: Double    -- | Position units per second
                       }
             deriving (Show, Eq)
makeLenses ''NavAxis

type Mover = Position -> IO ()

newNavAxis :: Int -> Mover -> Position -> IO (MVar NavAxis)
newNavAxis updateRate move initial = do
    ref <- newMVar $ NavAxis initial 0
    forkIO $ forever $ update ref initial
    return ref
  where dt = 1 / realToFrac updateRate
        update ref lastPos = do
            threadDelay $ round (1e6 * dt)
            na' <- modifyMVar ref $ \na->do
              let dx = round $ dt * na ^. velocity
                  na' = na & position %~ (+dx)
                  pos = na' ^. position
              when (lastPos /= pos) $ move pos
              return (na', na')
            update ref (na' ^. position)

newMotorQueue :: FilePath -> IO (TQueue (MM.Bus -> IO ()))
newMotorQueue device = do
    bus <- MM.open "/dev/ttyUSB4"
    queue <- newTQueueIO
    forkIO $ forever $ atomically (readTQueue queue) >>= ($ bus)
    return queue
 
listenEvDev :: Handle -> V3 (MVar NavAxis) -> IO ()
listenEvDev h navAxes = forever $ hReadEvent h >>= maybe (return ()) handleEvent
  where
    handleEvent :: Event -> IO ()
    handleEvent event@(RelEvent {}) = do
        case relAxisToLens (evRelAxis event) of
          Just l  -> modifyMVar_ (navAxes ^. reflectLens l) (return . (velocity .~ v))
          Nothing -> return ()
        where v = valueToVelocity (evValue event)
    handleEvent _ = return ()

    relAxisToLens :: RelAxis -> Maybe (ReifiedLens' (V3 a) a)
    relAxisToLens ax
      | ax == rel_x   = Just $ ReifyLens _x
      | ax == rel_y   = Just $ ReifyLens _y
      | ax == rel_z   = Just $ ReifyLens _z
      | otherwise     = Nothing
   
valueToVelocity :: Int32 -> Double
valueToVelocity v
  | abs v < thresh  = 0
  | otherwise       = gain * realToFrac (signum v) * (realToFrac $ abs v - thresh)**1.4
  where thresh = 30
        gain = 1
           
updateRate = 30

axes = V2 (MM.Axis 0) (MM.Axis 1) 

main :: IO ()
main = do
    queue <- newMotorQueue "/dev/ttyUSB4"
    let enqueue :: (MM.Bus -> IO ()) -> IO ()
        enqueue = atomically . writeTQueue queue
    initialVar <- newEmptyMVar
    enqueue $ \bus->do
        initialPos <- T.forM axes $ \axis->do
            MM.select bus axis
            MM.setMotorPower bus True
            either (error "Error fetching initial position") fromIntegral <$> MM.getPosition bus
        putMVar initialVar initialPos
    initial <- takeMVar initialVar
    let moveStage :: MM.Axis -> Mover
        moveStage axis (Pos n) = enqueue $ \bus->do
            MM.select bus axis
            MM.moveAbs bus (MM.Pos $ fromIntegral n)
            putStrLn $ "Move "++show axis++" to "++show n

    forkIO $ forever $ threadDelay 1000000 >> enqueue reportPositions
    navAxes <- T.sequence $ V3
                 (newNavAxis updateRate (moveStage (axes ^. _x)) (initial ^. _x))
                 (newNavAxis updateRate (moveStage (axes ^. _y)) (initial ^. _y))
                 (newNavAxis updateRate (const $ return ()) 0)

    joystick <- openFile "/dev/input/event4" ReadMode
    listenEvDev joystick navAxes

reportPositions :: MM.Bus -> IO ()
reportPositions bus = F.forM_ axes $ \axis->do
    MM.select bus axis
    p <- MM.getPosition bus
    putStrLn $ show axis++" is "++show p
    
