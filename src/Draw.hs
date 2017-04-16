module Draw(drawWorld, setColour, readBitmap) where

import Graphics.Gloss
import Board


setColour :: Color
setColour = makeColorI 100 100 100 0

readBitmap :: String -> Float -> IO Picture
readBitmap fileName size = do picture <- loadBMP fileName
                              let xScale = size / bmpSize
                              let yScale = size / bmpSize
                              return (Scale xScale yScale picture)

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: Picture -> Picture -> Picture -> World -> IO Picture
drawWorld boardPicture whitePicture blackPicture world
  = return (pictures (bottom ++ middle ++ top))
               where
                 dimension = size (board world)
                 boards = [ (i, j) | i <- [1 .. dimension], j <- [1 .. dimension] ]
                 whites = getPositions (pieces (board world)) White
                 blacks = getPositions (pieces (board world)) Black
                 bottom = [ drawSquares boardPicture dimension boards ]
                 middle = if paused world
                            then []
                            else [ drawSquares whitePicture dimension whites
                                 , drawSquares blackPicture dimension blacks ]
                 top    = [ Color black $ drawGrid dimension
                          , drawTurn (turn world)
                          , drawTime (timeElapsed world) ]

drawBoard :: Picture -> Picture
drawBoard boardPicture = undefined

getPositions :: [(Position, Col)] -> Col -> [Position]
getPositions [] _ = []
getPositions ((p, c):remain) check = if c == check then p : getPositions remain check else getPositions remain check

drawLines :: Int -> Coordinate -> Coordinate -> Coordinate -> Picture
drawLines dimension (a, b) (c, d) (x, y) = pictures [ line [(a + x * i, b + y * i), (c + x * i, d + y * i)] | i <- [0 .. fromIntegral dimension] ]

drawGrid :: Int -> Picture
drawGrid dimension = pictures [ drawLines dimension (-halfSize, -halfSize) (halfSize, -halfSize) (0, cell), drawLines dimension (-halfSize, -halfSize) (-halfSize, halfSize) (cell, 0) ]
                       where
                         cell = cellSize dimension

drawSquares :: Picture -> Int -> [Position] -> Picture
drawSquares squarePicture dimension positions = pictures (map (drawSquare squarePicture dimension) positions)

drawSquare :: Picture -> Int -> Position -> Picture
drawSquare squarePicture dimension position = translate x y squarePicture
                                 where
                                   (x, y) = getCoordinate dimension position

getCoordinate :: Int -> Position -> Coordinate
getCoordinate dimension (r, c) = (x, y)
                                   where
                                     mid = fromIntegral (dimension + 1) / 2
                                     x   = (fromIntegral r - mid) * cellSize dimension
                                     y   = (fromIntegral c - mid) * cellSize dimension

drawTurn :: Col -> Picture
drawTurn colour = pictures [ translate a b turnPicture
                           , translate c d scaledText ]
                             where
                               (a, b) = turnOffset
                               (c, d) = textOffset
                               glossColour = if colour == White then white else black
                               turnPicture = Color glossColour $ circleSolid pieceRadius
                               scaledText = Scale textScale textScale $ Text "to play next."

drawTime :: Float -> Picture
drawTime timeElapsed = translate a b scaledText
                         where
                           (a, b) = timeOffset
                           scaledText = Scale textScale textScale $ Text ("Time elapsed: " ++ show (round timeElapsed))
