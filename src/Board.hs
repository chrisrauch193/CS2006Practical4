module Board where
import Debug.Trace

import Data
import Utils
import Debug.Trace
import Params
import Data.Maybe

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board colour position = if validPlace board position then Just newBoard else Nothing
  where
    newBoard = board { pieces = (position, colour) : pieces board }

playRule :: Rule -> Board -> Col -> Position -> (Rule, Maybe Board)
playRule Standard board colour position = case makeMove board colour position of
                                            Nothing  -> (Standard, Nothing)
                                            newBoard -> (Standard, newBoard)

playRule Handicap board colour position = case makeMove board colour position of
                                            Nothing       -> (Handicap, Nothing)
                                            Just newBoard -> (Handicap, checkHandicapMove newBoard colour position)

playRule (Pente penteState) board colour position = checkPenteMove (Pente penteState) board colour position

checkHandicapMove :: Board -> Col -> Position -> Maybe Board
checkHandicapMove board colour position =
  case colour of
    White -> Just board
    Black -> if handicapCondition then Just board else Nothing
  where
    handicapCondition = checkFourRule board && checkThreeRule board

checkPenteMove :: Rule -> Board -> Col -> Position -> (Rule, Maybe Board)
checkPenteMove rule board colour position = case makeMove board colour position of
                                              Nothing   -> (rule, Nothing)
                                              Just moveBoard -> do let newBoard = foldr (capturePente colour position) moveBoard directionList
                                                                   let captures = div (length (pieces moveBoard) - length (pieces newBoard)) 2
                                                                   (updatePenteState rule board colour captures, Just newBoard)

updatePenteState :: Rule -> Board -> Col -> Int -> Rule
updatePenteState (Pente penteState) board colour captures =
  Pente penteState { blackCaptures = newBlackCount
                   , whiteCaptures = newWhiteCount
                   , captureHistory = newCaptureHistory }
  where
    oldBlack = blackCaptures penteState
    oldWhite = whiteCaptures penteState
    newBlackCount = if colour == Black then oldBlack + captures else oldBlack
    newWhiteCount = if colour == White then oldWhite + captures else oldWhite
    newCaptureHistory = (board, oldBlack, oldWhite) : captureHistory penteState

capturePente :: Col -> Position -> (Int, Int) -> Board -> Board
capturePente colour (xPosition, yPosition) (xDirection, yDirection) board = board { pieces = newPieces }
  where
    (xStart, yStart) = (xPosition + xDirection, yPosition + yDirection)
    count = checkFunction (xStart, yStart) (other colour) (pieces board) 0 (xDirection, yDirection)
    checkPiece = ((xStart + xDirection * 2, yStart + yDirection * 2), colour)
    bounded = elem checkPiece (pieces board)
    toRemove = if count == 2 && bounded then [ ((xStart + xDirection * i, yStart + yDirection * i), other colour) | i <- [0 .. count - 1] ] else []
    newPieces = filter (\(p, c) -> notElem (p, c) toRemove) (pieces board)

checkFourRule :: Board -> Bool
checkFourRule board = length validDirections < 2
  where
    piece = head (pieces board)
    directions = getDirectionList piece board
    validDirections = filter (== 4) directions

checkThreeRule :: Board-> Bool
checkThreeRule board = length unboundedList < 2
   where
     piece = head (pieces board)
     counts = getDirectionList piece board
     countList = zip counts posDirectionList
     threeList = filter (\(x, y) -> x == 3) countList
     unboundedList = filter (isUnbounded board) (snd (unzip threeList))

isUnbounded :: Board -> Position -> Bool
isUnbounded  board (xDirection, yDirection) = not outLimits && null emptySpace
  where
    pieceList = pieces board
    ((xPosition, yPosition), colour) = head pieceList
    (xStart, yStart) = getBottom (xPosition, yPosition) colour (xDirection, yDirection) pieceList
    dimension = size board
    outLimits = checkOutLimit xStart xDirection dimension || checkOutLimit yStart yDirection dimension
    checkUnderneath = filter (matchPos (xStart - xDirection, yStart - yDirection)) pieceList
    checkAbove = filter (matchPos (xStart + (3 * xDirection), yStart - (3 * yDirection))) pieceList
    emptySpace = checkAbove ++ checkUnderneath

checkOutLimit :: Int -> Int -> Int -> Bool
checkOutLimit numToCheck dirToCheck size = (numToCheck - dirToCheck) < 1 || (numToCheck + (3 * dirToCheck)) > size

getBottom :: Position -> Col -> Position -> [(Position, Col)] -> Position
getBottom (xPosition, yPosition) colour (xDirection, yDirection) listOfPieces =
  case condition of
    [] -> (xPosition, yPosition)
    _  -> getBottom newPosition colour (xDirection, yDirection) listOfPieces
  where
    newPosition = (xPosition - xDirection, yPosition - yDirection)
    condition = filter (matchPiece (newPosition, colour)) listOfPieces

undo :: World -> World
undo world
  | null (pieces (board world)) = world
  | (playAI world) && length (pieces (board world)) < 2 = world
  | playAI world                = world { board = secondBoard { hintPieces = [] }, rule = secondRule, timeElapsed = 0 }
  | otherwise                   = world { board = firstBoard { hintPieces = [] }, rule = firstRule, turn = other (turn world), timeElapsed = 0 }
  where
    (firstRule, firstBoard) = revertWorld (rule world) (board world)
    (secondRule, secondBoard) = revertWorld firstRule firstBoard

revertWorld :: Rule -> Board -> (Rule, Board)
revertWorld (Pente penteState) board = (Pente newPenteState, newBoard)
  where
    (newBoard, newBlack, newWhite) = head (captureHistory penteState)
    newPenteState = penteState { blackCaptures = newBlack, whiteCaptures = newWhite, captureHistory = tail (captureHistory penteState) }

revertWorld rule board = (rule, board { pieces = tail (pieces board) })

validPlace :: Board -> Position -> Bool
validPlace board position = null condition
  where
    condition = filter (matchPos position) (pieces board)

matchPos :: Position -> (Position, Col) -> Bool
matchPos position (checkPosition, checkColour) = position == checkPosition

matchPiece :: (Position, Col) -> (Position, Col) -> Bool
matchPiece (position, colour) (checkPosition, checkColour) = condition
  where
    condition = matchPos position (checkPosition, checkColour) && colour == checkColour

-- insideBoard :: Int -> Position -> Bool
-- insideBoard dimension (x,y) = (elem x valid) && (elem y valid)
--                                   where valid = [1..dimension]
-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkBoardWon :: Board -> Maybe Col
checkBoardWon board
  | null (pieces board) = Nothing
  | otherwise           = if null condition then Nothing else Just y
    where
      (x, y) = head (pieces board)
      allCounts = getDirectionList (x, y) board
      condition = filter (== target board) allCounts

checkWon :: World -> Maybe Col
checkWon world = case rule world of
                   Standard         -> case checkTime world of
                                         Just colour -> Just colour
                                         Nothing     -> checkBoardWon (board world)
                   Handicap         -> case checkTime world of
                                         Just colour -> Just colour
                                         Nothing     -> checkBoardWon (board world)
                   Pente penteState -> case checkTime world of
                                         Just colour -> Just colour
                                         Nothing     -> case checkBoardWon (board world) of
                                                          Just colour -> Just colour
                                                          Nothing     -> checkCaptures (rule world) (board world)

checkCaptures :: Rule -> Board -> Maybe Col
checkCaptures (Pente penteState) board
  | whiteCaptures penteState == target board = Just White
  | blackCaptures penteState == target board = Just Black
  | otherwise                               = Nothing

checkTime :: World -> Maybe Col
checkTime world
  | not (timeEnabled world) = Nothing
  | otherwise = if timeElapsed world > timeLimit world then Just (other (turn world)) else Nothing

getDirectionList :: (Position, Col) -> Board -> [Int]
getDirectionList (x, y) board = list
  where
    allDirectionMoves = map (checkFunction x y (pieces board) 0) directionList
    allCounts = getTotal allDirectionMoves [] 0
    list = map (subtract 1) allCounts

getTotal :: [Int] -> [Int] -> Int -> [Int]
getTotal individualMoves calculatedMoves directionNumber
  | directionNumber == 8 = calculatedMoves
  | otherwise = getTotal individualMoves newCalcMoves newDirNumber
  where
    intOneToAdd = individualMoves !! directionNumber
    intTwoToAdd = individualMoves !! (directionNumber + 1)
    intToAdd = intOneToAdd + intTwoToAdd
    newDirNumber = directionNumber + 2
    newCalcMoves = intToAdd : calculatedMoves

checkFunction :: Position -> Col -> [(Position, Col)] -> Int -> (Int, Int) -> Int
checkFunction (xCheck, yCheck) colToCheck listOfPieces numPieces (aToAdd, bToAdd)
  | not validPieceHere = numPieces
  | otherwise          = checkFunction newPos colToCheck listOfPieces (numPieces + 1) (aToAdd, bToAdd)
  where
    newPos = (xCheck + aToAdd, yCheck + bToAdd)
    validPieceHere = any (matchPiece ((xCheck, yCheck), colToCheck)) listOfPieces

{- Hint: One way to implement 'checkWon' would be to write functions
which specifically check for lines in all 8 possible directions
(NW, N, NE, E, W, SE, SW)

In these functions:
To check for a line of n in a row in a direction D:
For every position ((x, y), col) in the 'pieces' list:
- if n == 1, the colour 'col' has won
- if n > 1, move one step in direction D, and check for a line of
  n-1 in a row.
-}
