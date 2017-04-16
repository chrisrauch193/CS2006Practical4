module Input(handleInput, handleClientInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import Args
import System.Exit (exitSuccess)
import Debug.Trace
import Control.Concurrent.Chan
import Data
import Utils
import Params

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!

handleClientInput :: Chan String -> Event -> World -> (World, IO ())
handleClientInput chan (EventKey (MouseButton LeftButton) Up m (x, y)) w
  = case getPosition (size (board w)) x y of
          Nothing -> (w, return ())
          Just position -> (w { board = new_board, turn = other (turn w) }, writeChan chan (show position))
            where
              current_board = board w
              new_board     = current_board { pieces = (position, turn w):(pieces current_board) }

handleClientInput _ _ w = (w, return ())

handleInput :: Event -> World -> IO World
handleInput (EventMotion (x, y)) w
  =  return w

handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w
  | wrapWon w = return w
  | otherwise = return (wrapClick w x y)

handleInput (EventKey (Char k) Down _ _) w
  = case k of
    'u' -> return (undo w)
    'n' -> return (newWorld w)
    'p' -> return (w { paused = not (paused w) })
    's' -> do
      saveGame w savepath
      return w
    'l' -> do
      loadedWorld <- loadGame savepath
      return loadedWorld
    otherwise -> return w

handleInput e w = return w

getPosition :: Int -> Float -> Float -> Maybe Position
getPosition dimension x y = case getIndex dimension x of
                              Nothing -> Nothing
                              Just i  -> case getIndex dimension y of
                                           Nothing -> Nothing
                                           Just j  -> Just (i, j)

getIndex :: Int -> Float -> Maybe Int
getIndex dimension value
  | index == 0 || index == dimension + 1 = Nothing
  | otherwise                            = Just index
    where
      cell    = cellSize dimension
      cutoffs = [-halfSize + cell * i | i <- [0 .. fromIntegral dimension]]
      index   = length (takeWhile (< value) cutoffs)

wrapClick :: World -> Float -> Float -> World
wrapClick world x y = case getPosition (size (board world)) x y of
                         Nothing       -> world
                         Just position -> wrapPosition world position

wrapPosition :: World -> Position -> World
wrapPosition world position = case playRule (rule world) (board world) (turn world) position of
                                (_, Nothing)             -> world
                                (newRule, Just newBoard) -> world { board = newBoard, rule = newRule, turn = other (turn world), timeElapsed = 0 }

wrapWon :: World -> Bool
wrapWon world = case checkWon world of
                  Nothing -> False
                  _       -> True
