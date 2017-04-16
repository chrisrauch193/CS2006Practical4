module Data where

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

data Col = Black | White
  deriving (Show, Eq, Read)

type Position = (Int, Int)

type Coordinate = (Float, Float)

data Rule =
    Standard
  | Handicap
  | Pente PenteState
  deriving (Show, Read)

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
  deriving (Show, Read)

data Board = Board { size   :: Int
                   , target :: Int
                   , pieces :: [ (Position, Col) ] }
  deriving (Show, Read, Eq)

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
