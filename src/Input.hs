module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI

import Debug.Trace

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInput :: Event -> World -> World
handleInput (EventMotion (x, y)) b
    = trace ("Mouse moved to: " ++ show (convertCoord x, convertCoord y) ++ show(x, y)) b
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) b
    = trace ("Left button pressed at: " ++ show (x,y)) b
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

getPosition :: Maybe Float -> Maybe Float -> Maybe Position
getPosition x y = case x of
                    Just x -> case y of
                                    Just y ->  Just (truncate x, truncate y)
                                    otherwise -> Nothing
                    otherwise -> Nothing

convertCoord :: Float -> Maybe Float
convertCoord coord
    | not (cond_neg9 !! 0) = Nothing
    | (cond_neg9 !! 1) = Just (-9)
    | cond_neg8 = Just (-8)
    | cond_neg7 = Just (-7)
    | cond_neg6 = Just (-6)
    | cond_neg5 = Just (-5)
    | cond_neg4 = Just (-4)
    | cond_neg3 = Just (-3)
    | cond_neg2 = Just (-2)
    | cond_neg1 = Just (-1)
    | cond_zero = Just (0)
    | cond_pos1 = Just (1)
    | cond_pos2 = Just (2)
    | cond_pos3 = Just (3)
    | cond_pos4 = Just (4)
    | cond_pos5 = Just (5)
    | cond_pos6 = Just (6)
    | cond_pos7 = Just (7)
    | cond_pos8 = Just (8)
    | cond_pos9 = Just (9)
    | otherwise = Nothing
    where
        cond_neg9 = [(coord > -475), (coord < -425)]
        cond_neg8 = coord < -375
        cond_neg7 = coord < -325
        cond_neg6 = coord < -275
        cond_neg5 = coord < -225
        cond_neg4 = coord < -175
        cond_neg3 = coord < -125
        cond_neg2 = coord < -75
        cond_neg1 = coord < -25
        cond_zero = coord < 25
        cond_pos1 = coord < 75
        cond_pos2 = coord < 125
        cond_pos3 = coord < 175
        cond_pos4 = coord < 225
        cond_pos5 = coord < 275
        cond_pos6 = coord < 325
        cond_pos7 = coord < 375
        cond_pos8 = coord < 425
        cond_pos9 = coord < 475

-- spaceFreeCheck :: Int -> Int -> Board -> Bool
-- spaceFreeCheck x y board
--     |
--     where
