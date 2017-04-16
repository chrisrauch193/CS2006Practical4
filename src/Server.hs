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

main :: IO ()
main = do
  os <- getOptions
  let w1 = (optionsWorld os)
  serverWorld <- newTVarIO w1
  playerCount <- newTVarIO 0
  chan <- newChan
  serve HostAny "12345" (handleClient serverWorld playerCount chan)

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


replaceBoard :: Board -> Board
replaceBoard newBoard = newBoard

--boardToString :: Board -> String
--boardToString b = (show (size b)) ++ " " ++ (show stringPieces)
--        where
--          piece = pieces b
--          stringPieces = map (showPiece) piece

-- "6 0:0=w 0:1=b"
assignColour :: Int -> Maybe Col
assignColour 0 = Just Black
assignColour 1 = Just White
assignColour _ = Nothing
