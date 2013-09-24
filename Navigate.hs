{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

import System.IO
import System.Linux.Input.Event
import Control.Concurrent.STM
import Control.Concurrent
import Control.Lens
import Control.Applicative
import Data.Int
import Linear
import Control.Monad (forever)
import Control.Monad.State
import Data.Traversable as T
import Data.Foldable as F

import qualified MercuryController as MM
import qualified ZMotor as Z

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
listenEvDev h navAxes =
    void $ flip evalStateT (pure 0) $ forever
    $ lift (hReadEvent h) >>= maybe (return ()) handleEvent
  where
    handleEvent :: Event -> StateT (V3 Int32) IO ()
    handleEvent event@(RelEvent {}) = do
        case relAxisToLens (evRelAxis event) of
          Just l  -> do reflectLens l .= evValue event
                        update
          Nothing -> return ()
    handleEvent _ = return ()
  
    update :: StateT (V3 Int32) IO ()
    update = void $ T.sequence $ core $ \l->do
        v <- uses l realToFrac
        lift $ modifyMVar_ (navAxes ^. l) $ return . (velocity .~ v)

    relAxisToLens :: RelAxis -> Maybe (ReifiedLens' (V3 a) a)
    relAxisToLens ax
      | ax == rel_x   = Just $ ReifyLens _x
      | ax == rel_y   = Just $ ReifyLens _y
      | ax == rel_z   = Just $ ReifyLens _z
      | otherwise     = Nothing
   
valueToVelocity :: V3 Int32 -> V3 Double
valueToVelocity v
  | norm v' < thresh = 0
  | otherwise        = gain * (norm v' - thresh)**1.4 *^ vhat
  where thresh = 30
        gain = 1
        v' = fmap realToFrac v
        vhat = normalize v'
           
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

    zMotor <- Z.open "/dev/ttyACM0"
    let moveZStage (Pos n) = Z.move zMotor n >> putStrLn ("hi"++show n)

    forkIO $ forever $ threadDelay 1000000 >> enqueue reportPositions
    navAxes <- T.sequence $ V3
                 (newNavAxis updateRate (moveStage (axes ^. _x)) (initial ^. _x))
                 (newNavAxis updateRate (moveStage (axes ^. _y)) (initial ^. _y))
                 (newNavAxis updateRate  moveZStage               0)

    joystick <- openFile "/dev/input/event4" ReadMode
    listenEvDev joystick navAxes

reportPositions :: MM.Bus -> IO ()
reportPositions bus = F.forM_ axes $ \axis->do
    MM.select bus axis
    p <- MM.getPosition bus
    putStrLn $ show axis++" is "++show p
    
