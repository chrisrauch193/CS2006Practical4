{- | Module to deal with the command line arguments-}
module Args where

import Board
import System.Console.GetOpt
import System.IO
import Control.Monad
import System.Exit
import System.Environment
import Data
import Utils

{- | Default options for the game -}
startOptions :: Options
startOptions = Options { optTarget    = 5
                       , optSize      = 19
                       , optColour    = Black
                       , optAI        = True
                       , optAIType    = Defensive
                       , optRule      = Standard
                       , optLimit     = 20.0
                       , optTime      = False
                       , optIPAddress = "138.251.29.97"
                       , optPort      = "12345" }


{- | function contained in import System.Console.GetOpt to get all of the command line options, it gets all of the
    data from all of the flags, and then it parses it to be the correct type, and then replaces the default options with the option provided.
    It works as an action genarating functions.
    Req Arg means that it only goes into the parsing for those flags if data provided.
-}
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

    , Option "d" ["AI Type"]
        (ReqArg
            (\arg opt -> return opt { optAIType = read arg :: Difficulty })
            "DIFFICULTY")
        "Type of AI to play against.  Basic Defensive or Aggressive."

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

    , Option "g" ["enable time"]
        (ReqArg
            (\arg opt -> return opt { optTime = read arg :: Bool })
            "BOOLEAN")
        "Enable time.  True or False."

    , Option "i" ["IP"]
        (ReqArg
            (\arg opt -> return opt { optIPAddress = read arg :: String })
            "STRING")
        "IP address for multiplayer."

    , Option "p" ["port"]
        (ReqArg
            (\arg opt -> return opt { optPort = read arg :: String })
            "STRING")
        "Port for multiplayer."

    , Option "h" ["help"]
        (NoArg
            (\_ -> do prg <- getProgName
                      hPutStrLn stderr (usageInfo prg options)
                      exitSuccess))
        "Show help."
    ]

{- | This functions replaces the defualt options with the options based on the command flags the user has entered in
    It gets the arguments from getArgs, and then takes the default options, and foldleft's them across each flag and argument supplied
    to build up the custom options the user has specified for the parameter.
-}
getOptions :: IO Options
getOptions = do args <- getArgs
                let (actions, nonOptions, errors) = getOpt RequireOrder options args -- doesn't require the flags to be in order.
                foldl (>>=) (return startOptions) actions

{- | Sets up the options of the world-}
optionsWorld :: Options -> World
optionsWorld options = World board Black (optAI options) (optAIType options) 0 (optLimit options)(optTime options) False (optRule options)
                       where
                         board = Board (optSize options) (optTarget options) [] (optColour options) []
