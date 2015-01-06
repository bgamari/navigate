{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

import System.IO
import System.Linux.Input.Event
import Control.Concurrent.STM
import Control.Concurrent
import Control.Lens
import Control.Applicative
import Data.Int
import Data.Functor.Rep
import Linear
import Control.Monad (forever)
import Control.Monad.State.Strict
import Data.Traversable as T
import Data.Foldable as F

import Data.Monoid
import Options.Applicative hiding ((&))

import qualified MercuryController as MM
import qualified ZMotor as Z

data Config = Config { xyDevice :: FilePath
                     , zDevice  :: FilePath
                     , inputDevice :: FilePath
                     , driveCurrent :: Int
                     , holdCurrent :: Int
                     }

axisNames :: V3 String
axisNames = V3 "x" "y" "z"

config = Config
    <$> strOption ( long "xy" <> metavar "DEVICE"
                    <> value "/dev/ttyUSB.stage"
                    <> help "XY stepper device path")
    <*> strOption ( long "z" <> metavar "DEVICE"
                    <> value "/dev/ttyUSB.zstage"
                    <> help "Z stepper device path")
    <*> strOption ( long "input" <> metavar "DEVICE"
                    <> value "/dev/input/event4"
                    <> help "Joystick input device")
    <*> option auto ( long "drive-current" <> metavar "MA"
                      <> value 350
                      <> help "Motor drive current")
    <*> option auto ( long "hold-current" <> metavar "MA"
                      <> value 350
                      <> help "Motor drive current")

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
    bus <- MM.open device
    queue <- newTQueueIO
    forkIO $ forever $ atomically (readTQueue queue) >>= ($ bus)
    return queue

data JoystickState = JSS { _jssPosition    :: V3 Int32
                         , _jssEnabled     :: V3 Bool
                         , _jssCurrentAxis :: [E V3]
                         }
makeLenses ''JoystickState

listenEvDev :: Handle -> V3 (MVar NavAxis) -> IO ()
listenEvDev h navAxes =
    void $ flip evalStateT s0 $ forever
    $ lift (hReadEvent h) >>= maybe (return ()) handleEvent
  where
    s0 = JSS { _jssPosition = pure 0
             , _jssEnabled = pure True
             , _jssCurrentAxis = cycle [ex, ey, ez]
             }
    handleEvent :: Event -> StateT JoystickState IO ()
    handleEvent (KeyEvent {evKeyCode=key, evKeyEventType=Released})
      | key == btn_0 = do
        curAxis:_ <- use jssCurrentAxis
        jssEnabled . el curAxis %= not
        en <- use jssEnabled
        when (not $ en ^. el curAxis) $ do
            jssPosition . el curAxis .= 0
        lift $ putStrLn $ "Enabled axes: "++show en
      | key == btn_1 = do
        jssCurrentAxis %= tail
        axis:_ <- use jssCurrentAxis
        lift $ putStrLn $ "Selected axis: "++(axisNames ^. el axis)
    handleEvent event@(RelEvent {}) = do
        case relAxisToLens (evRelAxis event) of
          Just l  -> do
              enabled <- use $ jssEnabled . el l
              when enabled $ do
                  jssPosition . el l .= evValue event
                  update
          Nothing -> return ()
    handleEvent _ = return ()

    tabulateV3 = tabulate :: (E V3 -> a) -> V3 a
    update :: StateT JoystickState IO ()
    update = void $ T.sequence $ tabulateV3 $ \(E l)->do
        v <- uses (jssPosition . l) realToFrac
        lift $ modifyMVar_ (navAxes ^. l) $ return . (velocity .~ v)

    relAxisToLens :: RelAxis -> Maybe (E V3)
    relAxisToLens ax
      | ax == rel_x   = Just ex
      | ax == rel_y   = Just ey
      | ax == rel_z   = Just ez
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
    args <- execParser $ info (helper <*> config) mempty

    putStrLn "Opening XY stage"
    queue <- newMotorQueue (xyDevice args)
    let enqueue :: (MM.Bus -> IO ()) -> IO ()
        enqueue = atomically . writeTQueue queue

    initialVar <- newEmptyMVar
    enqueue $ \bus->do
        initialPos <- T.forM axes $ \axis->do
            MM.select bus axis
            --MM.getError bus >>= print
            MM.setBrake bus False
            MM.setDriveCurrent bus (driveCurrent args)
            MM.setHoldCurrent bus (holdCurrent args)
            MM.setMotorPower bus True
            either (error "Error fetching initial position") fromIntegral <$> MM.getPosition bus
        putMVar initialVar initialPos
    initial <- takeMVar initialVar
    let moveStage :: MM.Axis -> Mover
        moveStage axis (Pos n) = enqueue $ \bus->do
            MM.select bus axis
            MM.moveAbs bus (MM.Pos $ fromIntegral n)
            putStrLn $ "Move "++show axis++" to "++show n

    putStrLn "Opening Z stage"
    zMotor <- Z.open (zDevice args)
    let moveZStage (Pos n) = do
            Z.move zMotor n
            putStrLn $ "Move Z to "++show n

    --forkIO $ forever $ threadDelay 1000000 >> enqueue reportPositions
    navAxes <- T.sequence $ V3
                 (newNavAxis updateRate (moveStage (axes ^. _x)) (initial ^. _x))
                 (newNavAxis updateRate (moveStage (axes ^. _y)) (initial ^. _y))
                 (newNavAxis updateRate  moveZStage               0)

    putStrLn "Opening joystick"
    joystick <- openFile (inputDevice args) ReadMode
    listenEvDev joystick navAxes

reportPositions :: MM.Bus -> IO ()
reportPositions bus = F.forM_ axes $ \axis->do
    MM.select bus axis
    p <- MM.getPosition bus
    putStrLn $ show axis++" is "++show p
