import Data.Char
import Data.Time.Clock
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Trans
import System.Hardware.Arduino
import System.IO
import System.Hardware.Serialport

updateAction:: Double -> Pin -> Pin -> Handle -> Arduino()
updateAction time dataPin led fileHdl = do
    digitalWrite led True
    delay(10) 
    let data1 = (analogRead dataPin)
    x <- data1
    digitalWrite led False
    delay(10)
    --liftIO $ putStrLn $ show time++" Pack: "++show x
    liftIO $ do curTime <- getCurrentTime
                putStrLn $ "["++show curTime++"]: "++" Pack: "++show x ++ " --- " ++ show time
                hPutStrLn fileHdl $ show x ++ " " ++ show x ++ " " ++ show x ++ " " ++ show x ++ " " ++ show x
    if time > 0 then updateAction (time - 0.02) dataPin led fileHdl else return()
    

readSend:: Double -> IO()
readSend time = do 
    fileHdl <- openFile "log.txt" WriteMode
    putStrLn "File opened!"
    withArduino False "/dev/ttyS12" $do let dataPin = analog 3
                                        let led = digital 13
                                        setPinMode dataPin ANALOG
                                        setPinMode led OUTPUT
                                        updateAction time dataPin led fileHdl
    hClose fileHdl
    putStrLn "File closed!"



main :: IO()
main = do
    putStr "Write the time(in seconds) during which you need to register signals: "  
    time <- getLine
    readSend (read time ::Double)

--------------------------------------


{-convert1:: Char -> Int --преобразование одного символа
convert1 c = (ord c) - 48

convert:: [Char] -> [Int]
convert str = map convert1 str

toDec:: [Char] -> Int
toDec [x] = (convert1 x)
toDec xs = (foldl (\x y -> (x + y)*10) 0 (convert xs)) `div` 10 -}





{-module Main where

import Lib
import Control.Monad (forever)
import System.Hardware.Arduino

readValArdu :: IO ()
readValArdu = withArduino False "/dev/ttyS12" $ do
           let led = digital 13
           setPinMode led OUTPUT
           forever $ do digitalWrite led True
                        putStrLn "ON"
                        delay 1000
                        digitalWrite led False
                        putStrLn "OFF"
                        delay 1000

main :: IO ()
main = readValArdu-}


{-readValArdu :: IO ()
readValArdu = withArduino False "/dev/ttyS12" $ do
           let led = digital 13
           let data1 = digital 9
           let data2 = digital 9
           setPinMode led OUTPUT
           digitalWrite led True
           dataPack = getVal (digitalRead data1) (digitalRead data2) 
           delay 1000
           digitalWrite led False
           --putStrLn "OFF"
           delay 1000-}