module Args where

import Board
import System.Console.GetOpt
import System.IO
import Control.Monad
import System.Exit
import System.Environment
import Data
import Utils

startOptions :: Options
startOptions = Options { optTarget = 5
                       , optSize   = 19
                       , optColour = Black
                       , optAI     = True
                       , optRule   = Standard
                       , optLimit  = 20.0 }

--testBoard = Board 19 5 [((5,5), White), ((5,4), White), ((5,3), White), ((5,2), White), ((5,1), White)]
--testWorld = World testBoard White False startOptions 0 False

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "t" ["target"]
        (ReqArg
            (\arg opt -> return opt { optTarget = read arg :: Int })
            "INTEGER")
        "Target number of pieces.  Positive integer."

    , Option "s" ["size"]
        (ReqArg
            (\arg opt -> return opt { optSize = read arg :: Int })
            "INTEGER")
        "Size of board.  Positive integer."

    , Option "c" ["colour"]
        (ReqArg
            (\arg opt -> return opt { optColour = read arg :: Col })
            "COLOUR")
        "The player colour.  Black or White."

    , Option "a" ["AI"]
        (ReqArg
            (\arg opt -> return opt { optAI = read arg :: Bool })
            "BOOLEAN")
        "Whether to play an AI.  True or False."

    , Option "r" ["rule"]
        (ReqArg
            (\arg opt -> case arg of
                           "Pente" -> return opt { optRule = Pente emptyPenteState }
                           _       -> return opt { optRule = read arg :: Rule })
            "RULE")
        "The variation of Gomoku to play.  Standard, Handicap, or Pente."

    , Option "l" ["limit"]
        (ReqArg
            (\arg opt -> return opt { optLimit = read arg :: Float })
            "FLOAT")
        "Time limit.  Positive floating point number."

    , Option "h" ["help"]
        (NoArg
            (\_ -> do prg <- getProgName
                      hPutStrLn stderr (usageInfo prg options)
                      exitSuccess))
        "Show help."
    ]

getOptions :: IO Options
getOptions = do args <- getArgs
                let (actions, nonOptions, errors) = getOpt RequireOrder options args
                foldl (>>=) (return startOptions) actions

optionsWorld :: Options -> World
optionsWorld options = World board Black (optAI options) 0 (optLimit options) False (optRule options)
                       where
                         board = Board (optSize options) (optTarget options) [] (optColour options)
