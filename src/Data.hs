{-# LANGUAGE DeriveGeneric #-}
module Data where

import Data.List
import GHC.Generics
import Data.Binary


data Flag =
    Target String
  | Size String
  | Colour String
  | Help

data Options = Options { optTarget :: Int
                       , optSize   :: Int
                       , optColour :: Col
                       , optAI     :: Bool
                       , optRule   :: Rule }
  deriving (Show, Read, Generic)
instance Binary Options

data Col = Black | White
  deriving (Show, Eq, Read, Generic)
instance Binary Col

type Position = (Int, Int)

type Coordinate = (Float, Float)

data Rule =
    Standard
  | Handicap
  | Pente PenteState
  deriving (Show, Read, Generic)
instance Binary Rule

-- A Board is a record containing the board size (a board is a square grid,
-- n * n), the number of pieces in a row required to win, and a list
-- of pairs of position and the colour at that position.  So a 10x10 board
-- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- would be represented as:
--
-- Board 10 5 [((5, 5), Black), ((8, 7), White)]

data PenteState = PenteState { blackCaptures  :: Int
                             , whiteCaptures  :: Int
                             , captureHistory :: [ (Board, Int, Int) ] }
  deriving (Show, Read, Generic)
instance Binary PenteState

data Board = Board { size   :: Int
                   , target :: Int
                   , pieces :: [ (Position, Col) ] }
  deriving (Show, Read, Eq, Generic)
instance Binary Board

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board        :: Board
                   , turn         :: Col
                   , playerColour :: Col
                   , playAI       :: Bool
                   , timeElapsed  :: Float
                   , paused       :: Bool
                   , rule         :: Rule }
  deriving (Show, Read, Generic)
instance Binary World

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }
  deriving (Show)
