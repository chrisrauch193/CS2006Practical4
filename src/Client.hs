module Main  where
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Network.Socket (socketToHandle)
import Network.Simple.TCP
import System.IO

import Board
import Draw
import Input
import AI

import Args

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent.Chan
import Control.Concurrent

main :: IO ()
main = do
  args <- getOptions
  
  moveVar <- newTVarIO Nothing :: IO (TVar (Maybe Position))
  worldVar <- newTVarIO (initWorld args)
  
  chan <- newChan

  putStrLn "here"
  _ <- forkIO $ connect "127.0.0.1" "12345" (gameplayClientLoop worldVar chan) >> return ()
  putStrLn "here2"
  myFunkyPlay worldVar (InWindow "Gomoku" (640, 480) (10, 10)) rose 10
    (initWorld args) -- in Board.hs
    drawWorld -- in Draw.hs
    (handleClientInput chan)
    updateWorld -- in AI.hs


myFunkyPlay :: TVar world
            -> Display
            -> Color
            -> Int
            -> world
            -> (world -> Picture)
            -> (Event -> world -> (world, IO()))
            -> (Float -> world -> world)
            -> IO ()
myFunkyPlay var dsp col sz initworld draw input update = do
  atomically $ writeTVar var initworld
  playIO dsp col sz initworld draw' input' update'
  where
    draw' _  = do
      wrld <- readTVarIO var
      let pic = draw wrld
      return pic
      
    input' event _  = do
      (nw, a) <-atomically $ do
        wrld <- readTVar var
        let (newwrld, action) = input event wrld
        writeTVar var newwrld
        return (newwrld, action)
      a
      return nw
      

    update' time _ = atomically $ do
      wrld <- readTVar var
      let newwrld = update time wrld
      writeTVar var newwrld
      return newwrld


gameplayClientLoop :: TVar World -> Chan String -> (Socket, SockAddr) -> IO ()
gameplayClientLoop currentWorld chan (s, _) = do
  h <- socketToHandle s ReadWriteMode
  hPutStrLn h "C_CONNECT"
  putStrLn "HERE FAM"
  _ <- hGetLine h -- assume accept
  
  playerColourString <- hGetLine h
  let playerColour = read playerColourString

  putStrLn "HERE FAM1"
  
  loop $ do
    putStrLn "HERE FAM2"
    msgType <- hGetLine h
    putStrLn "MEOW"
    case msgType of
      "S_UPDATE_BOARD" -> do
        putStrLn "HERE FAM3"
        firstBoardString <- hGetLine h
        let newBoard = read firstBoardString

        atomically $ do
          cw <- readTVar currentWorld
          let nw = cw {board = newBoard, turn= (other playerColour)}
          writeTVar currentWorld nw
      "S_START_MOVE" -> do
        putStrLn "HERE FAM4"
        atomically $ do
          current_world <- readTVar currentWorld
          let world = current_world { turn = playerColour }
          writeTVar currentWorld world
        nextMove <- readChan chan
        hPutStrLn h "C_MAKE_MOVE"
        hPutStrLn h nextMove
        putStrLn "HERE FAM5"
      _ -> return ()


--initWorld

loop :: IO () -> IO ()
loop a = do
  a
  loop a
