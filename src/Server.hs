{- | The server function in the code -}
module Main where
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import Network.Socket (socketToHandle)
import Network.Simple.TCP
import System.IO
import Control.Concurrent.Chan

import Board
import Draw
import Input
import AI
import Args
import Data
import Utils
import Params

{- | The main loop in the server to handle connections from the clients
     Instantiate an initial world, as a TVar to allow atomic updating of the world
     Keeps a track of the number of clients connected.
     Instantiates chan for communication outside of a connection
-}
main :: IO ()
main = do
  os <- getOptions
  let w1 = (optionsWorld os)
  serverWorld <- newTVarIO w1
  playerCount <- newTVarIO 0
  chan <- newChan
  serve HostAny (optPort os) (handleClient serverWorld playerCount chan)

{- | This handles the gameplay loop for every client connection
    Connections go as follows:
    Accept Connection -> Send Client a Colour -> Send Client a Board
    -> Instantiate Dup Chan for potential extra players -> Start Loop
    Loop:
      Send current client board -> If won board send winning colour to client
      ->Tell current client to make move-> Wait for Client to Make move
      -> Accept/Reject Move
      -> Make accepted Move on Server Board -> Update World with new Board and next colour
      -> Send Updated World through Dup Chan to all clients
      -> Call loop again with Dup Chan in arguments so next client gets the updated world
-}
handleClient :: TVar World -> TVar Int -> Chan World -> (Socket, SockAddr) -> IO ()
handleClient startBoard playerCount chan (s, _) = do
  h <- socketToHandle s ReadWriteMode
  connectMessage <- hGetLine h
  putStrLn connectMessage

  nextColour <- atomically $ do
    curPlayerCount <- readTVar playerCount
    modifyTVar playerCount (+ 1)
    return $ assignColour curPlayerCount

  case nextColour of
    Nothing -> do
      hPutStrLn h "S_REFUSE"
      putStrLn "SENDING S_REFUSE"
      return ()
    Just col -> do
      hPutStrLn h "S_ACCEPT"
      putStrLn "SENDING S_ACCEPT"
      hPrint h col

      clientChan <- dupChan chan
      boardLoop (readTVarIO startBoard) h col clientChan
      return ()

  where boardLoop input h col clientChan = do
          nextWorld <- input

          hPutStrLn h "S_UPDATE_BOARD"
          putStrLn "SENDING UPDATE_BOARD"
          hPrint h  $ board nextWorld

          case checkWon nextWorld of
            Just wonCol -> do
              hPutStrLn h "S_GAME_WON"
              putStrLn "SENDING GAME_WON"
              putStrLn (show (board nextWorld))
              hPrint h wonCol
            Nothing -> if (turn nextWorld) == col then do
              hPutStrLn h "S_START_MOVE"
              putStrLn "SENDING START_MOVE"
              msgType <- hGetLine h
              case msgType of
                "C_MAKE_MOVE" -> do
                  putStrLn "RECEIVED C_MAKE_MOVE"
                  playerMoveString <- hGetLine h

                  case makeMove (board nextWorld) col (read playerMoveString) of
                    Nothing -> do
                      hPutStrLn h "S_REJECT_MOVE"
                      putStrLn "SENDING S_REJECT_MOVE"
                    Just newBoard -> do
                      let world = nextWorld { board = newBoard, turn = other(turn nextWorld) }
                      atomically $ writeTVar startBoard world
                      writeChan clientChan world
                      boardLoop (readChan clientChan) h col clientChan
                _ -> return ()
              else boardLoop (readChan clientChan) h col clientChan

{- | Used the connection to assign the next colour
-}
assignColour :: Int -> Maybe Col
assignColour 0 = Just Black
assignColour 1 = Just White
assignColour _ = Nothing
