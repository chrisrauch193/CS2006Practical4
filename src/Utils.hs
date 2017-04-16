module Utils where

import Data
import Graphics.Gloss
import Params

other :: Col -> Col
other Black = White
other White = Black

readBitmap :: String -> Float -> IO Picture
readBitmap fileName size = do picture <- loadBMP fileName
                              let xScale = size / bmpSize
                              let yScale = size / bmpSize
                              return (Scale xScale yScale picture)

newWorld :: World -> World
newWorld world = World newBoard Black (playerColour world) (playAI world) 0 False newRule
                   where
                     newBoard = Board (size (board world)) (target (board world)) []
                     newRule = case rule world of
                                 Pente _ -> Pente emptyPenteState
                                 oldRule -> oldRule

emptyPenteState :: PenteState
emptyPenteState = PenteState { blackCaptures = 0
                             , whiteCaptures = 0
                             , captureHistory = [] }
