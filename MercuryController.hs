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
                           -- * Status
                         , getPosition
                         ) where

import Data.Int
import Control.Applicative
import System.Hardware.Serialport
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Data.Monoid
import Data.Char (chr)
import Control.Monad (void)

newtype Axis = Axis Int
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
  where go a | "\13\10\03" `BS.isSuffixOf` a = return a
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
         
getPosition :: Bus -> IO Position
getPosition bus = do
    sendCmd bus "TP"
    r <- readReport bus
    print $ BS.break (==':') r
    return $ Pos 0