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
                       , optSize   = 10
                       , optColour = Black
                       , optAI     = False
                       , optRule   = Pente emptyPenteState }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "t" ["target"]
        (ReqArg
            (\arg opt -> return opt { optTarget = read arg :: Int })
            "INTEGER")
        "Target number of pieces."

    , Option "s" ["size"]
        (ReqArg
            (\arg opt -> return opt { optSize = read arg :: Int })
            "INTEGER")
        "Size of board."

    , Option "c" ["colour"]
        (ReqArg
            (\arg opt -> return opt { optColour = read arg :: Col })
            "COLOUR")
        "The player colour."

    , Option "a" ["AI"]
        (ReqArg
            (\arg opt -> return opt { optAI = read arg :: Bool })
            "BOOLEAN")
        "Whether to play an AI."

    , Option "r" ["rule"]
        (ReqArg
            (\arg opt -> return opt { optRule = read arg :: Rule })
            "RULE")
        "The variation of Gomoku to play."

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
optionsWorld options = World board Black (optColour options) (optAI options) 0 False (optRule options)
                       where
                         board = Board (optSize options) (optTarget options) []
