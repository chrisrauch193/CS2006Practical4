module Params where

import Data
import Graphics.Gloss

savepath :: String      -- ^ The path is of type String
savepath = "./save"

gridSize :: Float
gridSize = 480

updateRate :: Int
updateRate = 10

getTime :: Int -> Float
getTime updates = (1 / fromIntegral updateRate) * fromIntegral updates

halfSize :: Float
halfSize = gridSize / 2

cellSize :: Int -> Float
cellSize dimension = gridSize / fromIntegral dimension

pieceRadius :: Float
pieceRadius = 10

bmpSize :: Float
bmpSize = 50

turnOffset :: Coordinate
turnOffset = (-85, halfSize + 30)

textOffset :: Coordinate
textOffset = (-50, halfSize + 20)

timeOffset :: Coordinate
timeOffset = (-100, -halfSize - 40)

pauseOffset :: Coordinate
pauseOffset = (-50, -halfSize - 2 * textSpacing)

textScale :: Float
textScale = 0.2

titleScale :: Float
titleScale = 0.3

setColour :: Color
setColour = makeColorI 100 100 100 0

leftTextOffset :: Coordinate
leftTextOffset = (-halfSize - 250, 2 * textSpacing)

textSpacing :: Float
textSpacing = 40

rightTextOffset :: Coordinate
rightTextOffset = (halfSize + 50, 2.5 * textSpacing)

depthAI :: Int
depthAI = 9

updateLoopTime :: Float
updateLoopTime = 2.0
