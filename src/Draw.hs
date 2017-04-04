module Draw(drawWorld) where

import Graphics.Gloss
import Board


-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld world = pictures [Color black $ drawGrid dimension,
                            Color white $ drawPieces dimension whites,
                            Color black $ drawPieces dimension blacks]
                    where
                      dimension = size (board world)
                      whites = getPositions (pieces (board world)) White
                      blacks = getPositions (pieces (board world)) Black

getPositions :: [(Position, Col)] -> Col -> [Position]
getPositions [] _ = []
getPositions ((p, c):remain) check = if c == check then p:(getPositions remain check) else getPositions remain check

drawLines :: Int -> Coordinate -> Coordinate -> Coordinate -> Picture
drawLines dimension (a, b) (c, d) (x, y) = pictures [line [(a + x * i, b + y * i), (c + x * i, d + y * i)] | i <- [0 .. fromIntegral dimension]]

drawGrid :: Int -> Picture
drawGrid dimension = pictures [drawLines dimension (-halfSize, -halfSize) (halfSize, -halfSize) (0, cell), drawLines dimension (-halfSize, -halfSize) (-halfSize, halfSize) (cell, 0)]
                       where
                         cell = cellSize dimension

drawPieces :: Int -> [Position] -> Picture
drawPieces dimension pieces = pictures (map (drawPiece dimension) pieces)

drawPiece :: Int -> Position -> Picture
drawPiece dimension position = translate x y (circleSolid pieceRadius)
                                 where
                                   (x, y) = getCoordinate dimension position

getCoordinate :: Int -> Position -> Coordinate
getCoordinate dimension (r, c) = (x, y)
                                   where
                                     mid = fromIntegral (dimension + 1) / 2
                                     x   = (fromIntegral r - mid) * cellSize dimension
                                     y   = (fromIntegral c - mid) * cellSize dimension
