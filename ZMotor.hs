{-# LANGUAGE OverloadedStrings #-}

module ZMotor where

import System.Hardware.Serialport
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS

newtype ZMotor = ZMotor SerialPort

open :: FilePath -> IO ZMotor
open fname = do 
    dev <- openSerial fname settings
    send dev "rate 1000\n"
    return $ ZMotor dev
  where
    settings = defaultSerialSettings { commSpeed = CS115200 }
    
move :: ZMotor -> Int -> IO ()
move (ZMotor dev) pos =
    void $ send dev $ BS.pack $ "move "++show pos++"\n"

