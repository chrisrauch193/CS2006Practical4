module Utils where

import Data
import Graphics.Gloss
import Params

import Data.Binary
import Data.ByteString.Lazy (writeFile, readFile)

other :: Col -> Col
other Black = White
other White = Black

readBitmap :: String -> Float -> IO Picture
readBitmap fileName size = do picture <- loadBMP fileName
                              let xScale = size / bmpSize
                              let yScale = size / bmpSize
                              return (Scale xScale yScale picture)

newWorld :: World -> World
newWorld world = World newBoard Black (playAI world) (aiType world) 0 (timeLimit world) False False newRule
                   where
                     newBoard = Board (size (board world)) (target (board world)) [] (playerColour (board world)) []
                     newRule = case rule world of
                                 Pente _ -> Pente emptyPenteState
                                 oldRule -> oldRule

emptyPenteState :: PenteState
emptyPenteState = PenteState { blackCaptures = 0
                             , whiteCaptures = 0
                             , captureHistory = [] }

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
