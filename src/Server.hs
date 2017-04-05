module Server where
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Network.Socket (socketToHandle)
import Network.Simple.TCP
import System.IO

data Board = Board Int

main :: IO ()
main = do
  boardVar <- newTVarIO (Board 1)
  serve HostAny "12345" (handleClient boardVar)
 
getVar :: Board -> Int
getVar (Board x) = x

incrementBoard :: Board -> Board
incrementBoard (Board x) = (Board (x+1))

handleClient :: TVar Board -> (Socket, SockAddr) -> IO ()
handleClient board (s, a) = do
  h <- socketToHandle s ReadWriteMode
  Board x <- readTVarIO board
  hPutStrLn h ("hey bitch: current board is " ++ (show x))
  atomically $ modifyTVar board incrementBoard
  loop $ do
    line <- hGetLine h
    putStrLn $ "Got message from client " ++ (show a) ++ ": " ++ line
