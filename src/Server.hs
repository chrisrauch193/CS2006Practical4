module Main where
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Network.Socket (socketToHandle)
import Network.Simple.TCP
import System.IO

import Board
import Draw
import Input
import AI


--data Board = Board Int

main :: IO ()
main = do
  startBoard <- newTVarIO (Board 6 3 [])
  player1Col <- newTVarIO Black
  player2Col <- newTVarIO White
  -- serve HostAny "12345" (handleClient boardVar)
  serve HostAny "12345" (handleClient startBoard player1Col)
  serve HostAny "12345" (handleClient startBoard player2Col)
--getVar :: Board -> Int
--getVar (Board x) = x

--incrementBoard :: Board -> Board
--incrementBoard (Board x) = (Board (x+1))

--handleClient :: TVar Board -> (Socket, SockAddr) -> IO ()
--handleClient board (s, a) = do
handleClient :: TVar Board -> TVar Col -> (Socket, SockAddr) -> IO ()
handleClient startBoard player (s, a) = do
  h <- socketToHandle s ReadWriteMode
  clientColour <- readTVarIO player
  firstBoard <- readTVarIO startBoard
  hPutStrLn h ("S_ACCEPT")
  hPutStrLn h (show clientColour)
  hPutStrLn h ("S_UPDATE_BOARD")
  hPutStrLn h (show firstBoard)
  --atomically $ modifyTVar replaceBoard board
  loop $ do
    line <- hGetLine h
    let boardUpdated = read line :: Board
    putStrLn $ "Got message from client " ++ (show boardUpdated)
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
