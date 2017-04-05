module Client where
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Network.Socket (socketToHandle)
import Network.Simple.TCP
import System.IO


main :: IO ()
main = do
  connect "127.0.0.1" "12345" clientFunc


clientFunc :: (Socket, SockAddr) -> IO ()
clientFunc (s, a) = do
  h <- socketToHandle s ReadWriteMode
  putStrLn "Connected to server"
  line <- hGetLine h
  putStrLn $ "Got line: " ++ line
  
  loop $ do
    client_input <- getLine
    hPutStrLn h client_input

loop :: IO () -> IO ()
loop a = do
  a
  loop a
