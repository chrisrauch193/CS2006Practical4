module Args where

--module Args (getOptions, genWorld) where
import Board
import System.Console.GetOpt
import System.IO
import Control.Monad
import System.Exit
import System.Environment



-- import System.Directory
-- import System.Console.Haskeline
-- import Control.Monad.IO.Class
-- import Control.Monad.Trans

import Data.Binary
import Data.ByteString.Lazy (writeFile, readFile)


startOptions :: Options
startOptions = Options { optTarget = 5
                       , optSize   = 10
                       , optColour = Black
                       , nextAI = True
                       , ai = True
                       }

testBoard = Board 19 5 [((5,5), White), ((5,4), White), ((5,3), White), ((5,2), White), ((5,1), White)]
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


-- -- |The Save function, saves binary encoding of the current GameState to a file to be decoded later
-- saveGame :: World            -- ^ The GameState to be saved
--             -> FilePath         -- ^ The save file path
--             -> (World, IO ()) -- ^ The IO() reutrn to continue playing
-- saveGame world file = do
--   let str = encode world
--   (world, Data.ByteString.Lazy.writeFile file str)

-- -- |The load function, loads and decodes the binary gamestate that from the save file
-- loadGame :: World
--          -> FilePath                    -- ^ The save file path
--          -> (World, IO ()) -- ^ Returns IO GameData to continue playing be with the new GameState
-- loadGame w f = do
--   read_file <- (w, Data.ByteString.Lazy.readFile f)
--   ((decode read_file :: World), return ())

-- -- ((decode read_file :: World), return ())


-- |The Save function, saves binary encoding of the current GameState to a file to be decoded later
saveGame :: World            -- ^ The GameState to be saved
            -> FilePath         -- ^ The save file path
            -> IO () -- ^ The IO() reutrn to continue playing
saveGame world file = do
  let str = encode world
  Data.ByteString.Lazy.writeFile file str

-- |The load function, loads and decodes the binary gamestate that from the save file
loadGame :: FilePath                    -- ^ The save file path
         -> IO World -- ^ Returns IO GameData to continue playing be with the new GameState
loadGame f = do
  read_file <- Data.ByteString.Lazy.readFile f
  return (decode read_file :: World)
