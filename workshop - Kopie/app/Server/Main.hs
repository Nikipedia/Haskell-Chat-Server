import Network.Simple.TCP

-- qualified => benötigt Namen über den man die Methoden aufrufen kann, um Verwechslungen mit Prelude o.a. zu vermeiden
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.IORef

import Lib

main = do
    putStrLn "Starting server"
    sockets <- newIORef []
    serve (Host "localhost") "8080" (serverBody sockets)

serverBody io (socket, sockAddr) = do
    putStrLn ("Hi, " ++ show sockAddr)
    x <- readIORef io
    writeIORef io (socket:x)
    --connect "localhost" "8081" (openConnection io)
    serverGetNick socket io
    putStrLn ("Bye, client " ++ show sockAddr)

openConnection io (socket, sockAddr) = do
    loop
    where loop = do loop

serverGetNick socket ref = do 
    nick <- recv socket 1024
    let input = case nick of 
            Nothing -> Left "Network failed"
            Just bs -> deserialize bs
    case input of
        Left err -> putStrLn "Network failed"
        Right (Nickname nick) -> serverLoop socket nick ref

sendMsgTo :: [Socket] -> ClientAction -> IO ()
sendMsgTo (s:xs) msg = do
    send s (serialize msg)
    sendMsgTo xs msg
sendMsgTo [] msg = do pure()

serverLoop :: Socket -> T.Text -> IORef [Socket] -> IO ()
serverLoop socket nick ref = do
    raw <- recv socket 1024
    let input = case raw of 
            Nothing -> Left "Network failed"
            Just bs -> deserialize bs
    case input of
        Left err -> putStrLn "Network failed"
        Right Quit -> putStrLn "Client says goodbye"
        Right (Message msg) -> do
            let y = T.append (T.append nick (T.pack ": ")) msg
            T.putStrLn y
            x <- readIORef ref
            sendMsgTo x (Message y)
            serverLoop socket nick ref