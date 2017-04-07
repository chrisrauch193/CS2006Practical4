module Main where
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Network.Socket (socketToHandle)
import Network.Simple.TCP
import System.IO
import Board

--data Board = Board Int

main :: IO ()
main = do
  startBoard <- newTVarIO (Board 6 3 [])
  -- serve HostAny "12345" (handleClient boardVar)
  serve HostAny "12345" (handleClient startBoard)
 
--getVar :: Board -> Int
--getVar (Board x) = x

--incrementBoard :: Board -> Board
--incrementBoard (Board x) = (Board (x+1))

--handleClient :: TVar Board -> (Socket, SockAddr) -> IO ()
--handleClient board (s, a) = do
handleClient :: TVar Board -> (Socket, SockAddr) -> IO ()
handleClient startBoard (s, a) = do
  h <- socketToHandle s ReadWriteMode
  firstBoard <- readTVarIO startBoard
  hPutStrLn h ("hey bitch: current board is " ++ (show firstBoard))
  --atomically $ modifyTVar replaceBoard board
  loop $ do
    line <- hGetLine h
    let boardUpdated = read line :: Board
    putStrLn $ "Got message from client " ++ (show boardUpdated) ++ ": " ++ line
    hPutStrLn h ("hey bitch: you updated the board to " ++ (show boardUpdated))

loop :: IO () -> IO ()
loop a = do
        a
        loop a

replaceBoard :: Board -> Board
replaceBoard newBoard = newBoard

--boardToString :: Board -> String
--boardToString b = (show (size b)) ++ " " ++ (show stringPieces)
--        where
--          piece = pieces b
--          stringPieces = map (showPiece) piece

-- "6 0:0=w 0:1=b"
