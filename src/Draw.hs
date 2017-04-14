module Draw(drawWorld) where

import Graphics.Gloss
import Board


-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: Picture -> Picture -> Picture -> World -> Picture
drawWorld boardPicture whitePicture blackPicture world
  = pictures [ drawSquares boardPicture dimension boards
             , drawSquares whitePicture dimension whites
             , drawSquares blackPicture dimension blacks
             , Color black $ drawGrid dimension ]
               where
                 dimension = size (board world)
                 boards = [ (i, j) | i <- [1 .. dimension], j <- [1 .. dimension] ]
                 whites = getPositions (pieces (board world)) White
                 blacks = getPositions (pieces (board world)) Black

drawBoard :: Picture -> Picture
drawBoard boardPicture = undefined

getPositions :: [(Position, Col)] -> Col -> [Position]
getPositions [] _ = []
getPositions ((p, c):remain) check = if c == check then p : getPositions remain check else getPositions remain check

drawLines :: Int -> Coordinate -> Coordinate -> Coordinate -> Picture
drawLines dimension (a, b) (c, d) (x, y) = pictures [line [(a + x * i, b + y * i), (c + x * i, d + y * i)] | i <- [0 .. fromIntegral dimension]]

drawGrid :: Int -> Picture
drawGrid dimension = pictures [drawLines dimension (-halfSize, -halfSize) (halfSize, -halfSize) (0, cell), drawLines dimension (-halfSize, -halfSize) (-halfSize, halfSize) (cell, 0)]
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
