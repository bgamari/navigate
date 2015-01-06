{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module MercuryController ( -- * Interacting with the bus
                           Bus
                         , open
                           -- * Selecting a target motor controller
                         , Axis(..)
                         , select
                           -- * Motion
                         , Position(..)
                         , moveAbs
                         , setBrake
                         , setMotorPower
                           -- * Motor drive parameters
                         , setHoldCurrent
                         , setDriveCurrent
                           -- * Status
                         , getPosition
                         , getError
                         , Error(..)
                         ) where

import Data.List
import Data.Maybe (fromMaybe)
import Data.Int
import Control.Applicative
import Control.Error
import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Data.Monoid
import Data.Char (chr)
import Control.Monad (void)
import Control.Monad.Trans.Class

newtype Axis = Axis Int deriving (Show, Eq, Ord)
newtype Bus = Bus SerialPort

open :: FilePath -> IO Bus
open path = do
    dev <- openSerial path defaultSerialSettings
    return $ Bus dev

select :: Bus -> Axis -> IO ()
select (Bus dev) (Axis n) =
    void $ send dev $ BS.singleton '\01' <> BS.singleton (chr $ 48+n)

sendCmd :: Bus -> ByteString -> IO ()
sendCmd (Bus dev) c = void $ send dev $ c <> BS.singleton '\r'

readReport :: Bus -> IO ByteString
readReport (Bus dev) = go BS.empty
  where go a | "\13\10\03" `BS.isSuffixOf` a = return $ BS.take (BS.length a - 3) a
        go a = (a <>) <$> recv dev 1 >>= go

newtype Position = Pos Int32 deriving (Show, Eq, Ord, Num, Integral, Real, Enum, Bounded)

moveAbs :: Bus -> Position -> IO ()
moveAbs bus (Pos n) = do
    sendCmd bus $ "MA"<>BS.pack (show n)

setBrake :: Bus -> Bool -> IO ()
setBrake bus True = sendCmd bus "BN"
setBrake bus False = sendCmd bus "BF"

setMotorPower :: Bus -> Bool -> IO ()
setMotorPower bus True = sendCmd bus "MN"
setMotorPower bus False = sendCmd bus "MF"

setDriveCurrent :: Bus -> Int -> IO ()
setDriveCurrent bus cur = sendCmd bus (BS.pack $ "DC"++show cur)

setHoldCurrent :: Bus -> Int -> IO ()
setHoldCurrent bus cur = sendCmd bus (BS.pack $ "HC"++show cur)

getPosition :: Bus -> IO (Either String Position)
getPosition bus = runEitherT $ do
    lift $ sendCmd bus "TP"
    r <- lift $ readReport bus
    case stripPrefix "P:" $ BS.unpack r of
      Just s  -> let stripped = (fromMaybe s $ stripPrefix "+" s)
                 in Pos <$> tryRead ("getPosition: Unable to parse: "<>show s) stripped
      Nothing -> left "getPosition: Invalid report format"

data Error = ParameterSyntaxErr
           | UnknownCmdErr
           | UnallowableMoveErr
           | PositionOutOfLimitsErr
           | VelocityOutOfLimitsErr
           | ControllerStoppedErr
           | InvalidAxisErr
           | UnknownStageNameErr
           | ParameterOutOfRangeErr
           | InvalidMacroNameErr
           | RecordingMacroErr
           | MacroNotFoundErr
           | AxisIdMoreThanOnceErr
           | IllegalAxisErr
           | IncorrectNumParametersErr
           | InvalidFloatErr
           | ParameterMissingErr
           | CmdNotAllowedForStageErr
           | UnknownParameterErr
           | UnknownAxisIdErr
           | OtherErr Int
           deriving (Show, Eq, Ord)

getError :: Bus -> IO (Either String (Maybe Error))
getError bus = runEitherT $ do
    lift $ sendCmd bus "ERR?"
    r <- lift $ readReport bus
    code <- tryRead "Failed to parse response" $ BS.unpack r
    return $ case code of
                 0 -> Nothing
                 n -> Just $ toError n
  where
    toError :: Int -> Error
    toError n =
        case n of
          1  -> ParameterSyntaxErr
          2  -> UnknownCmdErr
          5  -> UnallowableMoveErr
          7  -> PositionOutOfLimitsErr
          8  -> VelocityOutOfLimitsErr
          10 -> ControllerStoppedErr
          15 -> InvalidAxisErr
          16 -> UnknownStageNameErr
          17 -> ParameterOutOfRangeErr
          18 -> InvalidMacroNameErr
          19 -> RecordingMacroErr
          20 -> MacroNotFoundErr
          22 -> AxisIdMoreThanOnceErr
          23 -> IllegalAxisErr
          24 -> IncorrectNumParametersErr
          25 -> InvalidFloatErr
          26 -> ParameterMissingErr
          34 -> CmdNotAllowedForStageErr
          54 -> UnknownParameterErr
          (-1001) -> UnknownAxisIdErr
          n  -> OtherErr n
