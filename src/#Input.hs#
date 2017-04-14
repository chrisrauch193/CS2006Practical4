module Input(handleInput, handleClientInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI

import Debug.Trace
import Control.Concurrent.Chan

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
--    = trace ("Left button pressed at: " ++ show (x,y)) b






handleInput :: Event -> World -> World
handleInput (EventMotion (x, y)) b
    = trace ("Mouse moved to: " ++ show (x, y) ++ show(x, y)) b
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) b
      = case getPosition (size (board b)) x y of
          Nothing -> b
          Just position -> b { board = new_board, turn = other (turn b) }
            where
              current_board = board b
              new_board     = current_board { pieces = (position, turn b):(pieces current_board) }
--    = trace ("Left button pressed at: " ++ show (x,y)) b
handleInput (EventKey (Char k) Down _ _) b
    = trace ("Key " ++ show k ++ " down") b
handleInput (EventKey (Char k) Up _ _) b
    = trace ("Key " ++ show k ++ " up") b
handleInput e b = b

{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}

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

-- spaceFreeCheck :: Int -> Int -> Board -> Bool
-- spaceFreeCheck x y board
--     |
--     where
