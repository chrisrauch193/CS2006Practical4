module Board where

data Flag = Target String
          | Size String
          | Colour String
          | Help

data Options = Options  { optTarget :: Int
                        , optSize   :: Int
                        , optColour :: Col
                        , nextAI :: Bool
                        , ai :: Bool
                        }
  deriving (Show)
data Col = Black | White
  deriving (Show, Eq, Read)

other :: Col -> Col
other Black = White
other White = Black

type Position = (Int, Int)

type Coordinate = (Float, Float)

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

textScale :: Float
textScale = 0.2

directionList :: [Position]
directionList = [(0,1),(0,-1),(-1,0),(1,0),(1,1),(-1,-1),(1,-1),(-1,1)]

upDirectionList :: [Position]
upDirectionList = [(0,1),(1,0),(1,1),(-1,1)]
-- A Board is a record containing the board size (a board is a square grid,
-- n * n), the number of pieces in a row required to win, and a list
-- of pairs of position and the colour at that position.  So a 10x10 board
-- for a game of 5 in a row with a black piece at 5,5 and a white piece at 8,7
-- would be represented as:
--
-- Board 10 5 [((5, 5), Black), ((8,7), White)]

data Board = Board { size :: Int,
                     target :: Int,
                     pieces :: [(Position, Col)]
                   }
  deriving (Show,Eq)

-- Default board is 6x6, target is 3 in a row, no initial pieces
initBoard = Board 19 5 []

testOptions = Options { optTarget = 5
                       , optSize   = 10
                       , optColour = Black
                       , nextAI = True
                       , ai = True
                       }



-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board
                   , turn :: Col
                   , won :: Bool
                   , option :: Options
                   , timeElapsed :: Float
                   , paused :: Bool }
  deriving (Show)
-- initWorld = World initBoard Black False False

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Col -> Position -> Maybe Board
makeMove board col position = case col of
                                White -> case condition of
                                            False -> Nothing
                                            True -> Just new_board
                                Black -> case extraConditions of
                                            False -> Nothing
                                            True -> Just new_board
                    where
                      condition = validPlace board position == True -- && insideBoard (size board) position
                      new_board = board {pieces = ((position,col):pieces board)}
                      extraFour = checkFourRule new_board
                      extraThree = checkThreeRule new_board
                      extraConditions = extraThree == True && extraFour== True && condition == True

checkFourRule :: Board -> Bool
checkFourRule board = case listlength of
                        0 -> True
                        1 -> True
                        otherwise -> False
  where
    (x,y) = head (pieces board)
    list = getDirectionList (x,y) board
    condition = filter (\x -> x ==4) list
    listlength = length condition

checkThreeRule :: Board-> Bool
checkThreeRule board = case listlength of
                          0 -> True
                          1 -> True
                          otherwise -> False
  where
    (x,y) = head (pieces board)
    list = getDirectionList (x,y) board
    posList = zip list upDirectionList
    filteredList = filter (checkListThree) posList
    unboundedList = filter (isUnbounded (head (pieces board)) board) (snd (unzip filteredList))
    listlength = length unboundedList

checkListThree :: (Int,Position)-> Bool
checkListThree (checkNum,pos) = checkNum == 3

isUnbounded :: (Position,Col) -> Board->Position ->  Bool
isUnbounded ((posX,posY),col) board (directionX,directionY) = case outLimits of
                                                                  True -> False
                                                                  False -> case emptySpace of
                                                                            [] -> True
                                                                            otherwise -> False
                                                                  where
                                                                    piecesBoard = pieces board
                                                                    (startX,startY) = getBottom (posX,posY) col (directionX,directionY) piecesBoard
                                                                    boardSize = size board
                                                                    outLimits = (checkOutLimit startX directionX boardSize) || (checkOutLimit startY directionY boardSize)
                                                                    checkUnderneath = filter (matchPos (startX - directionX,startY - directionY)) piecesBoard
                                                                    checkAbove = filter (matchPos (startX + (3 * directionX),startY - (3 * directionY))) piecesBoard
                                                                    emptySpace = concat [checkAbove,checkUnderneath]

checkOutLimit :: Int -> Int -> Int -> Bool
checkOutLimit numToCheck dirToCheck size = ((numToCheck - dirToCheck) < 1) || ((numToCheck + (3 * dirToCheck)) > size)

getBottom :: Position -> Col -> Position-> [(Position,Col)]-> Position
getBottom (posx,posy) col (dirx,diry) listOfPieces = case condition of
                                                        [] -> (posx,posy)
                                                        otherwise -> getBottom newpos col (dirx,diry) listOfPieces
                                  where
                                    newpos = ((posx-dirx),(posy-diry))
                                    condition = filter (matchPiece (newpos,col)) listOfPieces


undo :: World -> World
undo world = case optionAI of
              True -> case pieceList of
                        [] -> world
                        otherwise -> world {board = aiBoard}
              False -> case pieceList of
                        [] -> world
                        otherwise -> world {board = multiPlayerBoard, turn = other (turn world)}
  where
    options = option world
    optionAI = ai options
    currBoard = board world
    pieceList = pieces currBoard
    aiBoard = currBoard {pieces = tail (tail(pieceList))}
    multiPlayerBoard = currBoard {pieces = tail(pieceList)}

makeAIMove :: Board -> Col -> Position -> Board
makeAIMove board col pos = board{pieces = ((pos,col):pieces board)}

validPlace :: Board->Position -> Bool
validPlace board position
  | condition == [] = True
  | otherwise = False
  where
    condition = filter (matchPos position) (pieces board)

matchPos :: Position-> (Position,Col) -> Bool
matchPos pos (checkPos,checkCol) = pos == checkPos

matchPiece :: (Position,Col) -> (Position,Col) -> Bool
matchPiece (pos,col) (checkPos,checkCol) = condition
  where
    condition = matchPos pos (checkPos,checkCol) == True && col == checkCol

-- insideBoard :: Int -> Position -> Bool
-- insideBoard dimension (x,y) = (elem x valid) && (elem y valid)
--                                   where valid = [1..dimension]
-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Col
checkWon board
  | condition == [] = Nothing
  | otherwise = Just (y)
  where
    (x,y) = head (pieces board)
    allCounts = getDirectionList (x,y) board
    condition = filter (winExist (target board)) allCounts

getDirectionList :: (Position,Col) -> Board -> [Int]
getDirectionList (x,y) board = list
    where
      -- (x,y) = head (pieces board)
      allDirectionMoves = map (checkfunction x y (pieces board) 0) directionList
      allCounts = getTotal allDirectionMoves [] 0
      list = map (subtract 1) allCounts


getTotal :: [Int] -> [Int] -> Int-> [Int]
getTotal individualMoves calculatedMoves directionNumber
  | directionNumber == 8 = calculatedMoves
  | otherwise = getTotal individualMoves newCalcMoves newDirNumber
  where
    intOneToAdd = individualMoves !! directionNumber
    intTwoToAdd = individualMoves !! (directionNumber + 1)
    intToAdd = intOneToAdd + intTwoToAdd
    newDirNumber = directionNumber + 2
    newCalcMoves = intToAdd : calculatedMoves


winExist :: Int-> Int -> Bool
winExist target listInt = listInt == target

checkfunction :: Position -> Col-> [(Position,Col)] -> Int-> Position-> Int
checkfunction (xCheck,yCheck) colToCheck listOfPieces numPieces (aToAdd,bToAdd)
  | validPieceHere == False = numPieces
  | otherwise  = checkfunction newPos colToCheck listOfPieces newNumPieces (aToAdd,bToAdd)
  where
    newNumPieces = numPieces + 1
    newPos = (xCheck + aToAdd, yCheck + bToAdd)
    validPieceHere = filter (matchPiece ((xCheck,yCheck), colToCheck)) listOfPieces /= []
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

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Col -> Int
evaluate = undefined
