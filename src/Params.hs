{-| List of parameters used in the program-}
module Params where

import Data
import Graphics.Gloss
-- | Path where the savefile is stored
savepath :: String      -- ^ The path is of type String
savepath = "./save"

-- | Size of the Grid
gridSize :: Float
gridSize = 480

-- | Rate of Updating the board
updateRate :: Int
updateRate = 10

-- | Half the size of the board
halfSize :: Float
halfSize = gridSize / 2

-- | Gets a cell size on the board
cellSize :: Int -> Float
cellSize dimension = gridSize / fromIntegral dimension

-- | Radius of a piece
pieceRadius :: Float
pieceRadius = 10

-- | Size of a BMP
bmpSize :: Float
bmpSize = 50

-- | Offset for piece
turnOffset :: Coordinate
turnOffset = (-85, halfSize + 30)

-- | for text next to piece at the top of the function
textOffset :: Coordinate
textOffset = (-50, halfSize + 20)

-- | Offset for the time
timeOffset :: Coordinate
timeOffset = (-100, -halfSize - 40)

-- | Offset for the pause text
pauseOffset :: Coordinate
pauseOffset = (-50, -halfSize - 2 * textSpacing)

-- | Scaling of the text
textScale :: Float
textScale = 0.2

-- | Scaling of the title
titleScale :: Float
titleScale = 0.3

-- | Grey background colour
setColour :: Color
setColour = makeColorI 100 100 100 0

-- | offset for the text on the left
leftTextOffset :: Coordinate
leftTextOffset = (-halfSize - 250, 2 * textSpacing)

-- | spacing of the text
textSpacing :: Float
textSpacing = 40

-- | offset for the text on the right
rightTextOffset :: Coordinate
rightTextOffset = (halfSize + 50, 2.5 * textSpacing)

-- | The depth the AI goes to in minimax
depthAI :: Int
depthAI = 2

-- | List of half of the 8 directions
posDirectionList :: [Position]
posDirectionList = [(0,1), (1,0), (1,1), (1,-1)]

-- | List of the other half of the 8 directions
negDirectionList :: [Position]
negDirectionList = [(0,-1), (-1,0), (-1,-1), (-1,1)]

-- | A list of all of the 8 directions
directionList :: [Position]
directionList = [(0, 1), (0, -1), (-1, 0), (1, 0), (1, 1), (-1, -1), (1, -1), (-1, 1)]
