module Main where
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Network.Socket (socketToHandle)
import Network.Simple.TCP
import System.IO

import Board

main :: IO ()
main = do
  clientBoard <- newTVarIO (Board 10 5 [((5, 5), Black), ((8,7), White)])
  connect "127.0.0.1" "12345" (clientFunc clientBoard)


clientFunc :: TVar Board -> (Socket, SockAddr) -> IO ()
clientFunc nextBoard (s, a) = do
  h <- socketToHandle s ReadWriteMode
  putStrLn "Connected to server"
  line <- hGetLine h
  putStrLn $ "Got line: " ++ line
  sendBoard <- readTVarIO nextBoard
  loop $ do
    line2 <- getLine
    hPutStrLn h (show sendBoard)
    newBoard <- hGetLine h
    putStr $ show newBoard

loop :: IO () -> IO ()
loop a = do
  a
  loop a
