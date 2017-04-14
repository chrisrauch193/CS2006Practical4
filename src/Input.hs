module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import Args
import System.Exit (exitSuccess)
import Debug.Trace

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInput :: Event -> World -> World
handleInput (EventMotion (x, y)) b
    =  b
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) b
      = case winCondition of
          True -> b
          False -> case getPosition (size (board b)) x y of
                    Nothing -> b
                    Just position -> case new_board of
                                      Nothing -> (b { board = current_board})
                                      Just new_board -> case winCol of
                                                        Nothing -> b {board = new_board, turn = other (turn b) }
                                                        Just col -> trace (show(col) ++  "WON") b {board = new_board, turn = other (turn b),won = True}
                                          where
                                            winCol = checkWon new_board
                      where
                        current_board = board b
                        new_board     = makeMove current_board (turn b) position
          where
            winCondition = won b
--    = trace ("Left button pressed at: " ++ show (x,y)) b
handleInput (EventKey (Char k) Down _ _) b
    = case k of
      'u' -> case won b of
                False -> trace ("Key " ++ show k ++ " down") undoWorld
                True -> trace ("Can't Undo, Game Over")b
      'n' -> initB
      'a' -> b {option = aiOPtions}
      'm' -> b {option  = multiPlayerOptions}
      otherwise -> trace ("Key " ++ show k ++ " down") b
    where
      -- option = getCurrOption
      curr_board = board b
      options = option b
      aiOPtions = options {nextAI = True}
      multiPlayerOptions = options {nextAI = False}
      initB = genWorld options
      undoWorld = undo b
      -- new_b = b {undoWorld, turn = other (turn b)}
handleInput (EventKey (Char k) Up _ _) b
    = trace ("Key " ++ show k ++ " up") b
-- handleInput
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

-- getCurrOption :: Options
-- getCurrOption = do
--                   option <- getOptions
--                   return option
--                     --  options
