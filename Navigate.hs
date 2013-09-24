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

newtype Position = Pos Int
                 deriving (Show, Eq, Ord, Num, Integral, Real, Enum)

data NavAxis = NavAxis { _position :: Position
                       , _velocity :: Double    -- | Position units per second
                       }
             deriving (Show, Eq)
makeLenses ''NavAxis

newNavAxis :: Int -> (Position -> IO ()) -> Position -> IO (MVar NavAxis)
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
              when (lastPos /= pos) $ move pos >> putStrLn ("Move "++show pos)
              return (na', na')
            update ref (na' ^. position)

runMotorQueue :: TQueue (MM.Bus -> IO ()) -> MM.Bus -> IO ()
runMotorQueue queue bus = forever $ atomically (readTQueue queue) >>= ($ bus)
    
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
  | v < thresh  = 0
  | otherwise   = 100 * realToFrac (v - min)
  where thresh = 20
           
updateRate = 30 

main :: IO ()
main = do
    bus <- MM.open "/dev/ttyUSB4"
    MM.select bus (MM.Axis 1)
    MM.setMotorPower bus True
    motorQueue <- newTQueueIO
    forkIO $ runMotorQueue motorQueue bus
    let moveStage :: MM.Axis -> Position -> IO ()
        moveStage axis (Pos n) = MM.select bus axis >> MM.moveAbs bus (MM.Pos $ fromIntegral n)

    navAxes <- T.sequence $ V3
                 (newNavAxis updateRate (moveStage $ MM.Axis 0) 0)
                 (newNavAxis updateRate (moveStage $ MM.Axis 1) 0)
                 (newNavAxis updateRate (const $ return ()) 0)

    joystick <- openFile "/dev/input/event4" ReadMode
    listenEvDev joystick navAxes
    MM.setMotorPower bus False

