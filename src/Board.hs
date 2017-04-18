{- | Module board which contains all of the functions for the game mechanics for all of the rules-}
module Board where
import Debug.Trace

import Data
import Utils
import Debug.Trace
import Params
import Data.Maybe

{- | Wrapper function to check whether the position for the piece attempting to be played has a piece already on it.
-}
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board colour position = if validPlace board position then Just newBoard else Nothing
  where
    newBoard = board { pieces = (position, colour) : pieces board }

{- | Main function which gets called to make a move,
Depending on what rule is being played, different conditions get applied.
For standard, it is just checking if the position is empty,
For handicap, after applying standard rules, it checks whether if the piece is Black,
\it doesn't simultaneously create 2 unbounded rows of 3 or 2 rows of 4.
For Pente, the rule to ensure a move can be played is the same as the standard,
but then it checks for any pieces which have been captured, and removes them from the board.
-}
playRule :: Rule -> Board -> Col -> Position -> (Rule, Maybe Board)
playRule Standard board colour position = case makeMove board colour position of
                                            Nothing  -> (Standard, Nothing)
                                            newBoard -> (Standard, newBoard)

playRule Handicap board colour position = case makeMove board colour position of
                                            Nothing       -> (Handicap, Nothing)
                                            Just newBoard -> (Handicap, checkHandicapMove newBoard colour position)

playRule (Pente penteState) board colour position = checkPenteMove (Pente penteState) board colour position

{- | Handicap move function which for white pieces, just ignored them, but for black pieces,
it checks whether it playing the piece would break the conditions of three anf three and four and four.
-}
checkHandicapMove :: Board -> Col -> Position -> Maybe Board
checkHandicapMove board colour position =
  case colour of
    White -> Just board
    Black -> if handicapCondition then Just board else Nothing
  where
    handicapCondition = checkFourRule board && checkThreeRule board

{- | This function uses a foldr on the capturePente function to check if you have a capture available going in every direction.
If there are pieces to be captured the capture pente removes them from the board,
and then the number of capture made is the difference in the number of pieces between the initial board
and the board in which the captured pieces have been removed
To edit the pente state, the updatePenteState function is called with the number of
captures, the old board, the rule, and the new board.
-}
checkPenteMove :: Rule -> Board -> Col -> Position -> (Rule, Maybe Board)
checkPenteMove rule board colour position = case makeMove board colour position of
                                              Nothing   -> (rule, Nothing)
                                              Just moveBoard -> do let newBoard = foldr (capturePente colour position) moveBoard directionList
                                                                   let captures = div (length (pieces moveBoard) - length (pieces newBoard)) 2
                                                                   (updatePenteState rule board colour captures, Just newBoard)

{- | This function updates the pente state every time a move is made.
It adds the new board, and new list of captures for both black and white pieces
to the list of boards and captures which is stored to allow undo for pente to be completed,
 as this allowed the entire board to be rolled back including reverting the captures
 to what they used to be before a move was made.
-}
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

{- | This function checks for captures in all directions,
    It counts the number of pieces in the direction supplied, and then it
    checks whether there is a piece of the other colour to the 2 counted
    after the 2 counted, and if so, it makes a list of the two elements, and then
    removes those from the list of pieces on the board.
-}
capturePente :: Col -> Position -> (Int, Int) -> Board -> Board
capturePente colour (xPosition, yPosition) (xDirection, yDirection) board = board { pieces = newPieces }
  where
    (xStart, yStart) = (xPosition + xDirection, yPosition + yDirection)
    count = checkFunction (xStart, yStart) (other colour) (pieces board) 0 (xDirection, yDirection)
    checkPiece = ((xStart + xDirection * 2, yStart + yDirection * 2), colour)
    bounded = elem checkPiece (pieces board)
    toRemove = if count == 2 && bounded then [ ((xStart + xDirection * i, yStart + yDirection * i), other colour) | i <- [0 .. count - 1] ] else []
    newPieces = filter (\(p, c) -> notElem (p, c) toRemove) (pieces board)

{- | To check the rule of four and four for the handicapped rule, the function
     calls the get Direction List function which gets a list of rows in all directions
     of that colour of which have the piece just added in the row, and then it checks
     if 2 rows of 4 are being made, and if so, then the move being made is invalid.
-}
checkFourRule :: Board -> Bool
checkFourRule board = length validDirections < 2
  where
    piece = head (pieces board)
    directions = getDirectionList piece board
    validDirections = filter (== 4) directions

{- | To check the rule of three and three for the handicapped rule, the function
     calls the get Direction List function which gets a list of rows in all directions
     of that colour of which have the piece just added in the row, and then it
     filters the rows by rows of three, and then filters that by checking how many of the
     rows of three are unbounded, and if more than 1 row of unbounded three occurs
     by the playing of that piece the move is invalid.
-}
checkThreeRule :: Board-> Bool
checkThreeRule board = length unboundedList < 2
   where
     piece = head (pieces board)
     counts = getDirectionList piece board
     countList = zip counts posDirectionList -- This is zipped together such that when the list is filtered by three, you know the direction of each row.
     threeList = filter (\(x, y) -> x == 3) countList
     unboundedList = filter (isUnbounded board) (snd (unzip threeList))

{- | To check if a piece is unbounded, the function takes the direction of the row,
     and the board, and then on the last piece played, finds the end of the row,
     and then it checks whether the row is unbounded at both ends of the row,
     it does this by checking whether the the piece at either end of the row is within
     the limits of the board, and then checking if there is a piece already on the board
     at those positions.
-}
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

{- | This checks whether the piece after the end of the row is outside the limits
     of the board, it is called for both x and y.
-}
checkOutLimit :: Int -> Int -> Int -> Bool
checkOutLimit numToCheck dirToCheck size = (numToCheck - dirToCheck) < 1 || (numToCheck + (3 * dirToCheck)) > size

{- | Given a Piece and the direction of the row, this function traverses the row,
until it reaches the end of the row, and doesn't find a piece of the same colour at the end of it.
-}
getBottom :: Position -> Col -> Position -> [(Position, Col)] -> Position
getBottom (xPosition, yPosition) colour (xDirection, yDirection) listOfPieces =
  case condition of
    [] -> (xPosition, yPosition)
    _  -> getBottom newPosition colour (xDirection, yDirection) listOfPieces
  where
    newPosition = (xPosition - xDirection, yPosition - yDirection)
    condition = filter (matchPiece (newPosition, colour)) listOfPieces

{- | For the Undo function, the functionality was dependant on what type of game was being played
      for a Human Human game, one piece was removed from whichever stack was being used.
      for a Human AI game, two pieces were removed from the stack, to allow you to replay your move,
      For standard and handicap, due to storing the new piece played at the front of the list, all that was required was to
      return the tail of the list. For Pente, since capture had to be rolled back, the board was rolled back to the last pente state board
-}
undo :: World -> World
undo world
  | null (pieces (board world)) = world
  | (playAI world) && length (pieces (board world)) < 2 = world
  | playAI world                = world { board = secondBoard { hintPieces = [] }, rule = secondRule, timeElapsed = 0 }
  | otherwise                   = world { board = firstBoard { hintPieces = [] }, rule = firstRule, turn = other (turn world), timeElapsed = 0 }
  where
    (firstRule, firstBoard) = revertWorld (rule world) (board world)
    (secondRule, secondBoard) = revertWorld firstRule firstBoard

{- | reverts the world depending on the rule,
    for the pente state, it gets the board and captures from the capture history,
    and sets that to the current state of the board, for the other rules, it sets the
    board to be the old board minus the last piece placed.
-}
revertWorld :: Rule -> Board -> (Rule, Board)
revertWorld (Pente penteState) board = (Pente newPenteState, newBoard)
  where
    (newBoard, newBlack, newWhite) = head (captureHistory penteState)
    newPenteState = penteState { blackCaptures = newBlack, whiteCaptures = newWhite, captureHistory = tail (captureHistory penteState) }

revertWorld rule board = (rule, board { pieces = tail (pieces board) })

{- | checks if a piece exists on the board in the position where you are trying to place
    a piece
-}
validPlace :: Board -> Position -> Bool
validPlace board position = null condition
  where
    condition = filter (matchPos position) (pieces board)

{- | Mathces a position, mainly used in list filtering
-}
matchPos :: Position -> (Position, Col) -> Bool
matchPos position (checkPosition, checkColour) = position == checkPosition

{- | Matches 2 pieces, also mainly used in list filtering -}
matchPiece :: (Position, Col) -> (Position, Col) -> Bool
matchPiece (position, colour) (checkPosition, checkColour) = condition
  where
    condition = matchPos position (checkPosition, checkColour) && colour == checkColour

{- | Checks if a player has won by calling the get direction list function and seeing
    if the player has accumulated five in a row. Only checks the last piece played,
    as that is the only piece on the board which could cause the game to win.
-}
checkBoardWon :: Board -> Maybe Col
checkBoardWon board
  | null (pieces board) = Nothing
  | otherwise           = if null condition then Nothing else Just y
    where
      (x, y) = head (pieces board)
      allCounts = getDirectionList (x, y) board
      condition = filter (== target board) allCounts

{- | checkwon differentiates the checkwins based on the rules,
    first it checks whether someone had won based on the time limit expriring,
    and then it checks the checkBoardWon function for the standard and handicap rules
    for pente, it also checks captures to equal 5 for any colour.
-}
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

{- | Checks whether the capture is equal to the target in the game-}
checkCaptures :: Rule -> Board -> Maybe Col
checkCaptures (Pente penteState) board
  | whiteCaptures penteState == target board = Just White
  | blackCaptures penteState == target board = Just Black
  | otherwise                               = Nothing

{- | Checks if the player has run out of time on their move.-}
checkTime :: World -> Maybe Col
checkTime world
  | not (timeEnabled world) = Nothing
  | otherwise = if timeElapsed world > timeLimit world then Just (other (turn world)) else Nothing

{- | Gets the list of the length of rows in the 4 directions which include the piece provided.
    This calls the checkfunction for each direction and gets the count of the number of pieces of the same colour in that direction
    and then calls getTotal on the liost to get a list of length 4 of the length of each of the rows from that position.
-}
getDirectionList :: (Position, Col) -> Board -> [Int]
getDirectionList (x, y) board = list
  where
    allDirectionMoves = map (checkFunction x y (pieces board) 0) directionList
    allCounts = getTotal allDirectionMoves [] 0
    list = map (subtract 1) allCounts

{- | Takes the list of moves in all of the directions,
      and since the list is set up to have 2 opposing directions next to each other,
      to get the list of 4 rows, the list was summed up 2 at a time, and then
      1 was subtracted to remove the effect of the starting piece being counted twice, in both rows.
-}
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

{- | Given a piece and a direction, this returns the number of pieces of the same colour in that direction
      counting the intial piece itself.
      It does this by going along the row in the direction given, and checking if there is a piece of the same colour there or not.
-}
checkFunction :: Position -> Col -> [(Position, Col)] -> Int -> (Int, Int) -> Int
checkFunction (xCheck, yCheck) colToCheck listOfPieces numPieces (aToAdd, bToAdd)
  | not validPieceHere = numPieces
  | otherwise          = checkFunction newPos colToCheck listOfPieces (numPieces + 1) (aToAdd, bToAdd)
  where
    newPos = (xCheck + aToAdd, yCheck + bToAdd)
    validPieceHere = any (matchPiece ((xCheck, yCheck), colToCheck)) listOfPieces
