module Input(handleInput, handleClientInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import Args
import System.Exit (exitSuccess)
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






handleInput :: Event -> World -> IO World
handleInput (EventMotion (x, y)) b
    =  return b
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) b
      = case winCondition of
          True -> return(b)
          False -> case getPosition (size (board b)) x y of
                    Nothing -> return(b)
                    Just position -> case new_board of
                                      Nothing -> return(b { board = current_board})
                                      Just new_board -> case winCol of
                                                        Nothing -> return(b {board = new_board, turn = other (turn b), timeElapsed = 0 })
                                                        Just col -> trace (show(col) ++  "WON") return(b {board = new_board, turn = other (turn b), won = True})
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
                False -> trace ("Key " ++ show k ++ " down") return(undoWorld)
                True -> trace ("Can't Undo, Game Over") return(b)
      'n' -> return(initB)
      'a' -> return(b {option = aiOPtions})
      'm' -> return(b {option  = multiPlayerOptions})
      'p' -> return(b { paused = not (paused b) })
      's' -> do
        saveGame b savepath
        return b
      'l' -> do
        loadedWorld <- loadGame savepath
        return loadedWorld
      otherwise -> trace ("Key " ++ show k ++ " down") return(b)
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
    = trace ("Key " ++ show k ++ " up") return b
-- handleInput
handleInput e b = return b

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
