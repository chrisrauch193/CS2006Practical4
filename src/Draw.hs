{- | Module to draw the world-}
module Draw where

import Graphics.Gloss
import Board
import AI
import Data
import Params
import Utils
import Debug.Trace

{- | This reads the bitmapped images from file for the board and the pieces, sets the board to be the dimension given
      and builds the list of black white and hint pieces; the info about the game; and the entire key map of in game options.
      Then it builds the layers of the board, then the pieces depending on whether the board is paused or not, then the information layer, and the grid layer,
      before printing the time layer and the win layer
-}
drawWorld :: World -> IO Picture
drawWorld world = do
  let dimension = size (board world)
  boardPicture <- readBitmap "./assets/board.bmp" (cellSize $ dimension)
  whitePicture <- readBitmap "./assets/white.bmp" (cellSize $ dimension)
  blackPicture <- readBitmap "./assets/black.bmp" (cellSize $ dimension)
  hintPicture <- readBitmap "./assets/hint.bmp" (cellSize $ dimension)
  let boards = [ (i, j) | i <- [1 .. dimension], j <- [1 .. dimension] ]
  let whites = getPositions (pieces (board world)) White
  let blacks = getPositions (pieces (board world)) Black
  let hints = hintPieces (board world)
  let (gameInfoTitle, gameInfoText) = buildGameInfo world
  let (keyMapTitle, keyMapText) = buildKeyMap
  let boardLayer = [ drawSquares boardPicture dimension boards ]
  let pausedLayer = if paused world then [ drawPaused ] else [ drawSquares whitePicture dimension whites, drawSquares blackPicture dimension blacks, drawSquares hintPicture dimension hints  ]
  let infoLayer = [ drawInfo gameInfoTitle gameInfoText leftTextOffset, drawInfo keyMapTitle keyMapText rightTextOffset ]
  let gridLayer  = [ Color black $ drawGrid dimension ]
  let timeElapsedGame = if (timeEnabled world) then (show (timeElapsed world)) else ""
  let wonLayer   = case checkWon world of
                     Nothing     -> [ drawTurnWon (turn world) "to play next.", drawTime timeElapsedGame (timeLimit world) ]
                     Just colour -> [ drawTurnWon colour "has won.", drawGameOver ]
  return (pictures (boardLayer ++ pausedLayer ++ infoLayer ++ gridLayer ++ wonLayer))

{- | gets a list of positions, on which the colour matches the colour specified.
-}
getPositions :: [(Position, Col)] -> Col -> [Position]
getPositions [] _ = []
getPositions ((p, c):remain) check = if c == check then p : getPositions remain check else getPositions remain check

{- | Draws the lines for the board.
-}
drawLines :: Int -> Coordinate -> Coordinate -> Coordinate -> Picture
drawLines dimension (a, b) (c, d) (x, y) = pictures [ line [(a + x * i, b + y * i), (c + x * i, d + y * i)] | i <- [0 .. fromIntegral dimension] ]

{- | Draws the vertical and horizontal lines which are needed for the grid on the board.
-}
drawGrid :: Int -> Picture
drawGrid dimension = pictures [ drawLines dimension (-halfSize, -halfSize) (halfSize, -halfSize) (0, cell), drawLines dimension (-halfSize, -halfSize) (-halfSize, halfSize) (cell, 0) ]
                       where
                         cell = cellSize dimension

{- | Draws the picture on every square on the grid-}
drawSquares :: Picture -> Int -> [Position] -> Picture
drawSquares squarePicture dimension positions = pictures (map (drawSquare squarePicture dimension) positions)

{- | Takes the picture of either the board, white piece, black piece, or hint piece, and a position to put it at, and draws it at that position
-}
drawSquare :: Picture -> Int -> Position -> Picture
drawSquare squarePicture dimension position = translate x y squarePicture
                                 where
                                   (x, y) = getCoordinate dimension position
{- | Gets the offset for a square -}
getCoordinate :: Int -> Position -> Coordinate
getCoordinate dimension (r, c) = (x, y)
                                   where
                                     mid = fromIntegral (dimension + 1) / 2
                                     x   = (fromIntegral r - mid) * cellSize dimension
                                     y   = (fromIntegral c - mid) * cellSize dimension
{- | The win message gets printed here otherwise a message for who is playing next-}
drawTurnWon :: Col -> String -> Picture
drawTurnWon colour info = pictures [ translate a b turnPicture
                                , translate c d scaledText ]
                                  where
                                    (a, b) = turnOffset
                                    (c, d) = textOffset
                                    glossColour = if colour == White then white else black
                                    turnPicture = Color glossColour $ circleSolid pieceRadius
                                    scaledText = Scale textScale textScale $ Text info

{- | Prints the countdown time for the move-}
drawTime :: String -> Float -> Picture
drawTime timeElapsed timeLimit
  | timeElapsed == "" = Scale textScale textScale $ Text ("")
  | otherwise = translate a b scaledText
                         where
                           (a, b) = timeOffset
                           timeRemaining = abs (round (timeLimit - (read timeElapsed :: Float)))
                           scaledText = Scale textScale textScale $ Text ("Time left: " ++ show timeRemaining)

{- | Prints Game Over-}
drawGameOver :: Picture
drawGameOver = translate a b scaledText
                         where
                           (a, b) = timeOffset
                           scaledText = Scale textScale textScale $ Text "Game Over."

{- | Draws the string, Paused, when the game is paused-}
drawPaused :: Picture
drawPaused = translate a b scaledText
  where
    (a, b) = pauseOffset
    scaledText = Scale textScale textScale $ Text "Paused."

{- | Draw's the information on the left hand side of the screen -}
drawInfo :: String -> [String] -> Coordinate -> Picture
drawInfo title info (xOffset, yOffset) = pictures infoPictures
  where
    titlePicture = translate xOffset yOffset $ Scale titleScale titleScale $ Text title
    textPictures = map (translate xOffset yOffset . Scale textScale textScale . Text) info
    textPicturesIndexed = zip textPictures [1 .. fromIntegral (length textPictures)]
    textPicturesTranslated = map (\(p, i) -> translate 0 (-i * textSpacing) p ) textPicturesIndexed
    infoPictures = titlePicture : textPicturesTranslated

{- | List of Strings which should be printed on the right hand side of the screen-}
buildKeyMap :: (String, [String])
buildKeyMap = ("Key Map:", keyText)
  where
    keyText = [ "u: undo"
              , "n: new game"
              , "p: pause"
              , "s: save"
              , "l: load"
              , "h: hint" ]

{- | Builds the game information such as the rule, Who is playing, the number of Captures, size and target of the board-}
buildGameInfo :: World -> (String, [String])
buildGameInfo world = ("Game Info:", infoText)
  where
    currentBoard = board world
    ruleText = case rule world of
                 Pente _   -> "Pente"
                 ruleType  -> show ruleType
    captureText = case rule world of
                    Pente penteState -> "B" ++ show (blackCaptures penteState) ++ " W" ++ show (whiteCaptures penteState)
                    _                -> "N/A"
    infoText = [ "Size: " ++ show (size currentBoard)
               , "Target: " ++ show (target currentBoard)
               , "Rule: " ++ ruleText
               , "Captures: " ++ captureText ]
