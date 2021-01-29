module Main where

import Network.Simple.TCP
import Control.Concurrent
-- qualified => benötigt Namen über den man die Methoden aufrufen kann, um Verwechslungen mit Prelude o.a. zu vermeiden
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T


import Lib

main :: IO ()
main = do 
    connect "localhost" "8080" clientBody 

    

clientPrint (socket, sockAddr) = do
    loop
    where loop = do
            x <- recv socket 1024
            let input = case x of 
                    Nothing -> Left "Network failed"
                    Just bs -> deserialize bs
            case input of
                Left err -> putStrLn "Network failed"
                Right (Message msg) -> do
                    T.putStrLn msg
                    loop

clientBody :: (Socket, b) -> IO ()
clientBody (socket, sockAddr) = do
    forkIO (clientPrint (socket, sockAddr))
    putStrLn "Client connected"
    putStrLn "Enter nickname: "
    userInput <- T.getLine
    send socket (serialize (Nickname userInput))
    loop
   where
       loop = do
            putStrLn "Enter message: "
            userInput <- T.getLine
            let clientAction = if userInput == T.pack ":quit"
                then Quit 
                else Message userInput
            send socket (serialize clientAction)
            if clientAction == Quit
                then pure()
                else loop
