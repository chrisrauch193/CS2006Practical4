{- | Module to run the client code-}
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
import Utils
import Params
import Data
import Args

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent.Chan
import Control.Concurrent
{- | Main IO loop to start the connection to the Client
    Instantiate an initial world, as a TVar to allow atomic updating of the world
    ForkIo forks a connection to a new thread.
    Instantiates chan for communication outside of a connection
-}
main :: IO ()
main = do
  args <- getOptions
  worldVar <- newTVarIO (optionsWorld args)
  chan <- newChan
  _ <- forkIO $ connect (optIPAddress args) (optPort args) (gameplayClientLoop worldVar chan) >> return ()
  myFunkyPlay worldVar (InWindow "Gomoku" (1120, 800) (10, 10)) setColour updateRate
    (optionsWorld args) -- in Board.hs
    drawWorld -- in Draw.hs
    (handleClientInput chan)
    updateWorldNoAI -- in AI.hs

{- | Replacement for the Gloss Play Loop
    Takes a TVar world to allow for displaying of the TVar variable used in the
    connection loop
    The input function returns a world Chan tuple to allow for an event listener
    in the connection loop.
-}
myFunkyPlay :: TVar world
            -> Display
            -> Color
            -> Int
            -> world
            -> (world -> IO Picture)
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
      pic
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

{- | This is the gameplay loop of the client server connection
      Makes Connection -> Gets Colour -> Gets Server Board -> Waits to be told to make a move
      -> Makes Move -> Sends the Move to the server -> Waits for next message
      Messages consist of:
        Update board
        Make a Move
        Game has been Won
-}
gameplayClientLoop :: TVar World -> Chan String -> (Socket, SockAddr) -> IO ()
gameplayClientLoop currentWorld chan (s, _) = do
  h <- socketToHandle s ReadWriteMode
  hPutStrLn h "C_CONNECT"
  putStrLn "SENDING C_CONNECT"
  _ <- hGetLine h -- assume accept

  playerColourString <- hGetLine h
  let playerColour = read playerColourString :: Col

  --putStrLn "HERE FAM1"

  loop $ do
    --putStrLn "HERE FAM2"
    msgType <- hGetLine h
    --putStrLn "MEOW"
    case msgType of
      "S_UPDATE_BOARD" -> do
        putStrLn "RECEIVED S_UPDATE_BOARD"
        --putStrLn "HERE FAM3"
        firstBoardString <- hGetLine h
        let newBoard = read firstBoardString

        atomically $ do
          cw <- readTVar currentWorld
          let nw = cw {board = newBoard, turn= (other playerColour)}
          writeTVar currentWorld nw
      "S_START_MOVE" -> do
        --putStrLn "HERE FAM4"
        putStrLn "RECEIVED S_START_MOVE"
        atomically $ do
          current_world <- readTVar currentWorld
          let world = current_world { turn = playerColour }
          writeTVar currentWorld world
        nextMove <- readChan chan
        hPutStrLn h "C_MAKE_MOVE"
        putStrLn "SENDING C_MAKE_MOVE"
        hPutStrLn h nextMove
        --putStrLn "HERE FAM5"
      "S_GAME_WON" -> do
        wonCol <- hGetLine h
        let winningCol = (read wonCol) :: Col
        return ()
      _ -> return ()

{- | Simple loop function used for the main loop of the client connection-}
loop :: IO () -> IO ()
loop a = do
  a
  loop a
