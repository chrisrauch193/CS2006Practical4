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

import Graphics.Gloss



import System.Environment
--connect "127.0.0.1" "12345" (clientFunc initWorld)

main :: IO ()
main = play (InWindow "Gomoku" (640, 480) (10, 10)) rose 10
            initWorld -- in Board.hs
            drawWorld -- in Draw.hs
            handleClientInput -- in Input.hs
            updateWorld -- in AI.hs




gameplayClientLoop :: TVar World -> (Socket, SockAddr) -> IO ()
gameplayClientLoop currentWorld (s, a) = do
  h <- socketToHandle s ReadWriteMode
  hPutStrLn h "C_CONNECT"
  hGetLine h
  playerColourString <- hGetLine h
  playeColour <- readTVarIO playerColourString
  hGetLine h
  firstBoardString <- hGetLine h
  firstBoard <- readTVarIO firstBoardString
  loop $ do
    handleInput
    updateWorld currentWorld
    nextBoard <- readTVarIO (hGetLine h)
    nextWorld <- currentWorld { board = nextBoard, turn = (turn currentWorld)}
    --hPutStrLn h (show sendBoard)
    newBoard <- hGetLine h
    putStr $ show newBoard


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


--initWorld

loop :: IO () -> IO ()
loop a = do
  a
  loop a
