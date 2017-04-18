module Draw where

import Graphics.Gloss
import Board
import AI
import Data
import Params
import Utils
import Debug.Trace

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
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

drawTurnWon :: Col -> String -> Picture
drawTurnWon colour info = pictures [ translate a b turnPicture
                                , translate c d scaledText ]
                                  where
                                    (a, b) = turnOffset
                                    (c, d) = textOffset
                                    glossColour = if colour == White then white else black
                                    turnPicture = Color glossColour $ circleSolid pieceRadius
                                    scaledText = Scale textScale textScale $ Text info

drawTime :: String -> Float -> Picture
drawTime timeElapsed timeLimit
  | timeElapsed == "" = Scale textScale textScale $ Text ("")
  | otherwise = translate a b scaledText
                         where
                           (a, b) = timeOffset
                           timeRemaining = abs (round (timeLimit - (read timeElapsed :: Float)))
                           scaledText = Scale textScale textScale $ Text ("Time left: " ++ show timeRemaining)

drawGameOver :: Picture
drawGameOver = translate a b scaledText
                         where
                           (a, b) = timeOffset
                           scaledText = Scale textScale textScale $ Text "Game Over."

drawPaused :: Picture
drawPaused = translate a b scaledText
  where
    (a, b) = pauseOffset
    scaledText = Scale textScale textScale $ Text "Paused."


drawInfo :: String -> [String] -> Coordinate -> Picture
drawInfo title info (xOffset, yOffset) = pictures infoPictures
  where
    titlePicture = translate xOffset yOffset $ Scale titleScale titleScale $ Text title
    textPictures = map (translate xOffset yOffset . Scale textScale textScale . Text) info
    textPicturesIndexed = zip textPictures [1 .. fromIntegral (length textPictures)]
    textPicturesTranslated = map (\(p, i) -> translate 0 (-i * textSpacing) p ) textPicturesIndexed
    infoPictures = titlePicture : textPicturesTranslated

buildKeyMap :: (String, [String])
buildKeyMap = ("Key Map:", keyText)
  where
    keyText = [ "u: undo"
              , "n: new game"
              , "p: pause"
              , "s: save"
              , "l: load"
              , "h: hint" ]

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
