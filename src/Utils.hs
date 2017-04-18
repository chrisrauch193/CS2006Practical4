{- | Module of Utility functions-}
module Utils where

import Data
import Graphics.Gloss
import Params

import Data.Binary
import Data.ByteString.Lazy (writeFile, readFile)
-- | Function to get the other colour to the one given
other :: Col -> Col
other Black = White
other White = Black

-- | Function to read a bitmapped image given a filename
readBitmap :: String -> Float -> IO Picture
readBitmap fileName size = do picture <- loadBMP fileName
                              let xScale = size / bmpSize
                              let yScale = size / bmpSize
                              return (Scale xScale yScale picture)

-- | Function to generate a new world when 'n' is pressed
newWorld :: World -> World
newWorld world = World newBoard Black (playAI world) (aiType world) 0 (timeLimit world) False False newRule
                   where
                     newBoard = Board (size (board world)) (target (board world)) [] (playerColour (board world)) []
                     newRule = case rule world of
                                 Pente _ -> Pente emptyPenteState
                                 oldRule -> oldRule

-- | Initial Pente state with no captures or moves made
emptyPenteState :: PenteState
emptyPenteState = PenteState { blackCaptures = 0
                             , whiteCaptures = 0
                             , captureHistory = [] }

-- |The Save function, saves binary encoding of the current world to a file to be decoded later when the game is loaded
saveGame :: World
           -> FilePath
           -> IO ()
saveGame world file = do
  let str = encode world
  Data.ByteString.Lazy.writeFile file str

-- |The load function, loads and decodes the binary encoding of a world that from the save file
loadGame :: FilePath                    
        -> IO World
loadGame f = do
  read_file <- Data.ByteString.Lazy.readFile f
  return (decode read_file :: World)
