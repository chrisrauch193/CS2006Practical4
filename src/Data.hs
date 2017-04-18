{- | Stores all of the data definitions of the program-}
{-# LANGUAGE DeriveGeneric #-}
module Data where

import Data.List
import GHC.Generics
import Data.Binary

{- | All of the flags used in the program -}
data Flag =
    Target String
  | Size String
  | Colour String
  | Help

{- | all of the Options to the game -}
data Options = Options { optTarget    :: Int -- | The target to win the game
                       , optSize      :: Int -- | The board size
                       , optColour    :: Col -- | The colour being played
                       , optAI        :: Bool -- | AI or No AI
                       , optAIType    :: Difficulty -- | AI Difficulty
                       , optRule      :: Rule -- | The version of the game being played, i.e. the rule
                       , optLimit     :: Float -- | The time limit for a move
                       , optTime      :: Bool -- | Whether a time limit is in force
                       , optIPAddress :: String -- | The IP address to connect to
                       , optPort      :: String }
  deriving (Show, Read, Generic)
-- | For saving and loading
instance Binary Options

-- | The colours in the game
data Col = Black | White
  deriving (Show, Eq, Read, Generic)
-- | For saving and loading
instance Binary Col
-- | Postion type
type Position = (Int, Int)
-- | Co-ordinate type
type Coordinate = (Float, Float)

-- | Different types of rules to be played
data Rule =
   Standard -- | standard rules
  | Handicap -- | Rule of Three and Three and Four and Four applied
  | Pente PenteState
  deriving (Show, Read, Generic)
-- | For saving and loading
instance Binary Rule

-- | Pente State holding the data for pente
data PenteState = PenteState { blackCaptures  :: Int  -- | Number of Captures Black has
                             , whiteCaptures  :: Int  -- | Number of Captures White has
                             , captureHistory :: [ (Board, Int, Int) ] }
  deriving (Show, Read, Generic)
-- | For saving and loading
instance Binary PenteState

-- | The board
data Board = Board { size   :: Int -- | The board size
                   , target :: Int -- | The target number of pieces needed to get in a row
                   , pieces :: [ (Position, Col) ] -- | All of the pieces on the board
                   , playerColour :: Col           -- | The colour of the player
                   , hintPieces :: [Position] }
  deriving (Show, Read, Eq, Generic)
-- | For saving and loading
instance Binary Board

-- | The world information
data World = World { board        :: Board -- | The Board
                   , turn         :: Col -- | Which colour's turn it is
                   , playAI       :: Bool -- | Whether or not the AI is being played
                   , aiType       :: Difficulty -- | Difficulty of the AI
                   , timeElapsed  :: Float -- | Time elapsed since re-setting
                   , timeLimit    :: Float -- | The max time limit for moves
                   , timeEnabled  :: Bool -- | Whether or not you are playing with a time limit
                   , paused       :: Bool -- | Whether or not the game has been paused or not
                   , rule         :: Rule }
  deriving (Show, Read, Generic)
-- | For saving and loading
instance Binary World

-- | The Game Tree used by the AI
data GameTree = GameTree { game_board :: Board, -- | The board at that moment in time
                           game_turn :: Col,    -- | Who's turn it is
                           next_moves :: [(Position, GameTree)] }
  deriving (Show)

-- | Difficulty of the AI,
data Difficulty = Basic | Aggressive | Defensive
  deriving(Show, Read, Eq, Generic)
-- | For saving and loading
instance Binary Difficulty
