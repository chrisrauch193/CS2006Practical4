module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Board
import Draw
import Input
import AI
import Args

import System.Environment
import System.Exit

-- 'play' starts up a graphics window and sets up handlers for dealing
-- with inputs and updating the world state.
--
-- 'drawWorld' converts the world state into a gloss Picture
--
-- 'handleInput' is called whenever there is an input event, and if it is
-- a human player's turn should update the board with the move indicated by
-- the event
--
-- 'updateWorld' is called 10 times per second (that's the "10" parameter)
-- and, if it is an AI's turn, should update the board with an AI generated
-- move



main :: IO ()
main = do options <- getOptions
          boardPicture <- readBitmap "./assets/board.bmp" (cellSize $ optSize options)
          whitePicture <- readBitmap "./assets/white.bmp" (cellSize $ optSize options)
          blackPicture <- readBitmap "./assets/black.bmp" (cellSize $ optSize options)
          playIO (InWindow "Gomoku" (640, 480) (10, 10)) setColour updateRate
            (genWorld options) -- in Args.hs
            (drawWorld boardPicture whitePicture blackPicture) -- in Draw.hs
            handleInput -- in Input.hs
            chooseUpdateWorld -- in AI.hs
