module Draw(drawWorld) where

import Graphics.Gloss
import Board

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
-- drawWorld w = Color blue $ line [(0, 0), (10, 10)]
drawWorld world =

drawLines :: Position -> Position -> Position -> Picture
drawLines (a, b) (c, d) (x, y) = [line [(a + x * i, b + y * i), (c + x * i, d + y * i)] |  i <- [0..19]]

drawGrid :: Picture
drawGrid = pictures [
                      drawLines (-475, -475) (475, -475) (0, 50),
                      drawLines (-475, -475) (-475, 475) (50, 0)
                    ]
