{- | Module Input which handles all of the client code, and it also handles input from the user-}
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
import Control.Concurrent.STM

{- | The event listeners for the client in multiplayers, same as input but takes in a chan string,
     Allows for event listeners to happen to happen in the Client Server loop by sending moves through a channel.
-}
handleClientInput :: Chan String -> Event -> World -> (World, IO ())
handleClientInput chan (EventKey (MouseButton LeftButton) Up m (x, y)) w
  | wrapWon w = (w, return())
  | otherwise = wrapChan w chan x y

handleClientInput chan (EventKey (Char k) Down _ _) w
  = case k of
    'h' -> (w { board = currentboard { hintPieces = getHints currentboard } }, return ())
    _ -> (w, return())
    where
      currentboard = board w

handleClientInput _ _ w = (w, return ())

{- | Handle the player clicking or pressing a key while the game is being played-}
handleInput :: Event -> World -> IO World
handleInput (EventMotion (x, y)) w
  =  return w

handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w
  | wrapWon w = return w --  checks whether the game has been won, and if so returns the old board so the player cannot click on the board anymore.
  | otherwise = return (wrapClick w x y) -- process the player clicking on the screen.

handleInput (EventKey (Char k) Down _ _) w -- Processes the keyboard inputs
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
    'h' -> return w { board = currentboard { hintPieces = getHints currentboard } }
    _ -> return w
    where
      currentboard = board w

handleInput e w = return w 
{- | Wrapper function to process a click on the board, if on the board, returns the position on the board otherrwise returns Nothing-}
getPosition :: Int -> Float -> Float -> Maybe Position
getPosition dimension x y = case getIndex dimension x of
                              Nothing -> Nothing
                              Just i  -> case getIndex dimension y of
                                           Nothing -> Nothing
                                           Just j  -> Just (i, j)
{- | Given a dimension of the board and the position of the mouse click, this calculates the position if applicable on the board where the click was made.-}
getIndex :: Int -> Float -> Maybe Int
getIndex dimension value
  | index == 0 || index == dimension + 1 = Nothing
  | otherwise                            = Just index
    where
      cell    = cellSize dimension
      cutoffs = [-halfSize + cell * i | i <- [0 .. fromIntegral dimension]]
      index   = length (takeWhile (< value) cutoffs)

{- | Given a click on the screen it calculates whether it is on the board or not, and if it is on the board runs the make move for the position on which it has been clicked-}
wrapClick :: World -> Float -> Float -> World
wrapClick world x y = case getPosition (size (board world)) x y of
                         Nothing       -> world
                         Just position -> wrapPosition world position

{- | Wrapper function to make a move and return the new world if the move gets made correctly, and return the old world if it doesn't -}
wrapPosition :: World -> Position -> World
wrapPosition world position = case playRule (rule world) (board world) (turn world) position of
                                (_, Nothing)             -> world
                                (newRule, Just newBoard) -> world { board = newBoard { hintPieces = [] }, rule = newRule, turn = other (turn world), timeElapsed = 0 }
{- | wrapper function to check if the game has been won or not -}
wrapWon :: World -> Bool
wrapWon world = case checkWon world of
                  Nothing -> False
                  _       -> True
{- | sends the click of the user after getting the position if valid, and send it down the channel -}
wrapChan :: World -> Chan String -> Float -> Float -> (World, IO ())
wrapChan world chan x y = case getPosition (size (board world)) x y of
                         Nothing       -> (world , return())
                         Just position -> (wrapPosition world position, writeChan chan (show  position))
