module Args where

import Board
import System.Console.GetOpt
import System.IO
import Control.Monad
import System.Exit
import System.Environment

startOptions :: Options
startOptions = Options { optTarget = 5
                       , optSize   = 10
                       , optColour = Black
                       , nextAI = True
                       , ai = True
                       }

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
        "Colour to play first."

    , Option "h" ["help"]
        (NoArg
            (\_ -> do prg <- getProgName
                      hPutStrLn stderr (usageInfo prg options)
                      exitSuccess))
        "Show help."
    ]

getOptions :: IO Options
getOptions = do args <- getArgs

                -- Parse options, getting a list of option actions
                let (actions, nonOptions, errors) = getOpt RequireOrder options args

                -- Here we thread startOptions through all supplied option actions
                foldl (>>=) (return startOptions) actions

genWorld :: Options -> World
genWorld options = World (Board (optSize options) (optTarget options) []) (optColour options) False nextOP
    where
      next = nextAI options
      nextOP = options {ai = next}
