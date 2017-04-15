module Args where

import Board
import System.Console.GetOpt
import System.IO
import Control.Monad
import System.Exit
import System.Environment

startOptions :: Options
startOptions = Options { optTarget = 5
                       , optSize   = 19
                       , optColour = Black
                       , nextAI = True
                       , ai = True
                       }

testBoard = Board 19 5 [((5,5), Black), ((5,4), Black), ((5,3), Black), ((5,2), Black), ((5,1), Black)]
testWorld = World testBoard White False startOptions 0 False

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
genWorld options = World (Board (optSize options) (optTarget options) []) (optColour options) False nextOP 0 False
    where
      next = nextAI options
      nextOP = options {ai = next}
