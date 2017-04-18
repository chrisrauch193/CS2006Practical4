{- | The Artificial Intelligence module
-}
module AI where

import Board
import Debug.Trace
import Args
import Data
import Utils
import Params
import Data.List

-- | A test board for debugging
testBoard = Board 19 5 [((2,3), Black), ((1,3), Black), ((1,2), Black), ((2,2), White), ((1,1), White)] Black

{- |  Generates Moves in a particular radius of the pieces which have been played.
-}
genAllMoves :: Board -> Col -> [Position]
genAllMoves board colour = if null matchedPositions then allPositions else matchedPositions
  where
    allPositions = [(i, j) | i <- [1 .. size board], j <- [1 .. size board] ]
    allMatchedPositions = filter (checkNeighbour board 2) allPositions
    existingPositions = map fst (pieces board)
    matchedPositions = filter (\p -> notElem p existingPositions) allMatchedPositions

{- | Checks for a given board, radius, and position, whether or not there is a piece
on the board within the radius of the specified position
-}
checkNeighbour :: Board -> Int -> Position -> Bool
checkNeighbour board maxRadius (xPosition, yPosition) = not (null matched)
  where
    checkPositions = [ (xPosition + i * radius, yPosition + j * radius)
                     | radius <- [1 .. maxRadius], (i, j) <- directionList ]
    existingPositions = map fst (pieces board)
    matched = intersect checkPositions existingPositions

-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.
{- | Builds a tree of all of the available moves which can be played and their corresponding trees
-}
buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree
buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b c pos of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                             -- successful, make move and build tree from
                             -- here for opposite player
{- | Function to generate a specific gametree based on the current world
-}
getCurrentTreeMax :: World -> GameTree
getCurrentTreeMax w = buildTree genAllMoves b t
    where
        b = board w
        t = turn w

--testTree = buildTree genAllMoves testBoard White

{- | Get the best next move from a game tree. This
   traverses the game tree up to a depth, and picks the move which
   leads to the position with the best score for the player whose turn it
   is. Based on the arguments it will either activate the advanced defensive, or advanced aggressive AI
-}
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> World
               -> Position
getBestMove meowdepth currentTree w = fst bestMoveTuple
  where
    aiTypeToPlay = aiType w
    bestMoveTuple = if  aiTypeToPlay == Defensive then minimax currentTree meowdepth True evaluateDefensive else minimax currentTree meowdepth True evaluateAggressive

{- | Just adds the move to the board without having to do all of the checks of MakeMove, beacause we already know the move is valid.-}
makeAIMove :: Board -> Col -> Position -> Board
makeAIMove board col pos = board{pieces = ((pos,col):pieces board)}

{- | Update the world state for the AI, only when it is the AI's turn
    Based on the arguments it will either activate the basic AI, or one of the advanced AI's
-}
updateWorld :: Float -> World -> World
updateWorld t w
  | gameWon                                  = w { timeElapsed = newTime }
  | turn w == other (playerColour (board w)) = newWorld
  | otherwise                                = w { timeElapsed = newTime }
  where
    basicAI = (aiType w) == Basic
    gameWon = case checkWon w of
                Nothing -> False
                _       -> True
    currentTurn = turn w
    currentBoard = board w
    currentTree = getCurrentTreeMax w
    nextMovePos = if basicAI then getBestBasicMove depthAI currentTree else getBestMove depthAI currentTree w
    newWorld = case playRule (rule w) (board w) (turn w) nextMovePos of
                 (_, Nothing)             -> w
                 (newRule, Just newBoard) -> w { board = newBoard, rule = newRule, turn = other (turn w), timeElapsed = 0 }
    newTime = if paused w then timeElapsed w else timeElapsed w + t

{- | This is for 2 player, doesn't do any AI processing, just returns the world it gets passed, and updates the time-}
updateWorldNoAI :: Float -- ^ time since last update (you can ignore this)
           -> World -- ^ current world state
           -> World
updateWorldNoAI t w = w { timeElapsed = timeElapsed w + t }

{- | Chooses which update world function to call depending on whether AI or local Multiplayer -}
chooseUpdateWorld :: Float -> World -> IO World
chooseUpdateWorld t w = if playAI w then return (updateWorld t w) else return (updateWorldNoAI t w)

{- | Test debugger function -}
safeMax :: [(Int, Position)] -> (Int, Position)
safeMax scores
  | null scores = (0, (0, 0))
  | otherwise = maximum scores

{- | The main MiniMax function it uses the normal minimax method to simulate both players playing on the
     board. It then generates a tree for each available move at each level and evaluated the board at each
     leaf node. It picks the highest scoring move. It gives maximum and minimum bounds at each depth for the AI winning, or the AI losing.
     The function values wins at higher levels of the tree more than wins lower down.
-}
minimax :: GameTree -> Int -> Bool -> (Board -> Int) -> (Position, Int)
minimax currentTree depth maximise evaluateFunc
  | checkWinAI currentBoard (playerColour currentBoard) depth =  (lastPlayedPiece, (minBound :: Int) + 20 - depth)
  | checkWinAI currentBoard (other (playerColour currentBoard)) depth = (lastPlayedPiece, (maxBound :: Int) - 20 + depth)
  | (depth == 0) || (null nextMoves) = (lastPlayedPiece, evaluateFunc currentBoard)
  | maximise = (snd maxScoreMove, fst maxScoreMove)
  | otherwise =  (snd minScoreMove, fst minScoreMove)
  where
    currentBoard = game_board currentTree
    (nextMoves, nextTrees) = unzip (next_moves currentTree)
    scores = map (\t -> snd (minimax t (depth - 1) (not maximise) evaluateFunc)) nextTrees
    maxScoreMove = safeMax $ zip scores nextMoves
    minScoreMove = minimum $ zip scores nextMoves
    lastPlayedPiece = fst (head (pieces currentBoard))

{- | The evaluation function for the advanced defensive AI.
-}
evaluateDefensive :: Board -> Int
evaluateDefensive leafBoard = evaluateAdv leafBoard True

{- | The evaluation function for the advanced Aggressive AI.
-}
evaluateAggressive :: Board -> Int
evaluateAggressive leafBoard = evaluateAdv leafBoard False

{- | The main evaluation function for the advanced AI, calculates a board score for the
     Aggressive and defensive AI, as the heuristics are similar.
-}
evaluateAdv :: Board -> Bool -> Int
evaluateAdv leafBoard defensive
  | playerWin = minBound :: Int
  | aiWin = (maxBound :: Int) - 1
  | otherwise = sum finalScoreList
  where
    playerCol = playerColour leafBoard
    aiWin = checkWinAI leafBoard (other playerCol) depthAI
    playerWin = checkWinAI leafBoard playerCol depthAI
    pieceLineCountList = map (\(pos, col) -> sum(getDirectionListNotBlocked (pos, col) leafBoard)) (pieces leafBoard)
    pieceColourList = map (\(pos, col) -> col) (pieces leafBoard)
    pieceScoreList = zip pieceColourList pieceLineCountList
    finalScoreList = map (\(col, score) -> calculateLineScore leafBoard col score defensive) pieceScoreList

{- | The heuristic calculation for the advanced evaluator, based on the number and
     size of the unbounded lines for each player.
-}
calculateLineScore :: Board -> Col -> Int -> Bool -> Int
calculateLineScore board col numLines defensive
  | col == aiCol = numLines
  | otherwise = if defensive then  numLines * (-10) else numLines * (-3)
  where
    aiCol = other (playerColour board)

{- | Same as the direction list function in Board, but it takes into account if the line has been blocked or not-}
getDirectionListNotBlocked :: (Position,Col) -> Board -> [Int]
getDirectionListNotBlocked (x,y) board = totalList
    where
      posDirectionMoves = map (checkUnblockedFunction board x y (pieces board) 0) posDirectionList
      negDirectionMoves = map (checkUnblockedFunction board x y (pieces board) 0) negDirectionList
      tupleDirectionMoves = zip posDirectionMoves negDirectionMoves
      totalList = map (\(a, b) -> (4 ^ (fst a + fst b + (-1))) * (snd a + snd b)) tupleDirectionMoves

{- | Function to check whether the Line is Unblocked or not in a given direction, based on the checkFunction, but for unblocked rows.
-}
checkUnblockedFunction :: Board -> Position -> Col-> [(Position,Col)] -> Int-> Position-> (Int, Int)
checkUnblockedFunction currentBoard (xCheck,yCheck) colToCheck listOfPieces numPieces (aToAdd,bToAdd)
  | validPieceHere == True = checkUnblockedFunction currentBoard newPos colToCheck listOfPieces newNumPieces (aToAdd,bToAdd)
  | blockedPieceHere == True = (numPieces, 0)
  | checkOutOfBounds currentBoard (xCheck, yCheck) = (numPieces, 0)
  | otherwise  = (numPieces, 1)
  where
    newNumPieces = numPieces + 1
    newPos = (xCheck + aToAdd, yCheck + bToAdd)
    validPieceHere = any (matchPiece ((xCheck,yCheck), colToCheck)) listOfPieces
    blockedPieceHere = any (matchPiece ((xCheck,yCheck), other colToCheck)) listOfPieces

{- | Used by checkUnblockedFunction to check if the row encroaches the end of the board-}
checkOutOfBounds :: Board -> Position -> Bool
checkOutOfBounds currentBoard (x, y)
  | (x > bSize) || (x < (bSize * (-1))) = True
  | (y > bSize) || (y < (bSize * (-1))) = True
  | otherwise = False
  where
    bSize = size currentBoard

{- | Used to check if the AI / Player has won at a specific depth in the gametree
     by checking the board state for each each piece played at each level
-}
checkWinAI :: Board -> Col -> Int -> Bool
checkWinAI currentBoard wonCol depth
  | colWonList == [] = False
  | fst (last colWonList) == wonCol = True
  | otherwise = False
  where
    piecesToCheck = take (depth + 1) (pieces currentBoard)
    pieceDirections = map (\(a, b) -> getDirectionList (a, b) currentBoard) piecesToCheck
    pieceResults = zip (map snd (pieces currentBoard)) (map (\dirList -> (any (\dir -> dir >= (target currentBoard)) dirList)) pieceDirections)
    colWonList = filter (\(a, b) -> b == True) pieceResults

{- | the evaluation function for the basic AI. it does not use the tree in the same way, it instead,
     checks for unbounded lines of length target -1 and target -2. It then calculates how to block this line.
-}
evaluateBoardBasic :: Board -> Int
evaluateBoardBasic leafBoard = calculateLineScoreBasic blackWhiteTotals
  where
    pieceLineCountList = map (\(pos, col) -> (col, getDirectionList (pos, col) leafBoard)) (pieces leafBoard)
    lineSizeTotalsWhite= map (\a -> getTotalSizeOccurence pieceLineCountList a (other (playerColour leafBoard))) [2..(target leafBoard)]
    lineSizeTotalsBlack = map (\a -> getTotalSizeOccurence pieceLineCountList a (playerColour leafBoard)) [2..(target leafBoard)]
    blackWhiteTotals = zip [2..(target leafBoard)] $ zip lineSizeTotalsWhite lineSizeTotalsBlack

{- | Gets the size of a line
-}
getTotalSizeOccurence :: [(Col, [Int])] -> Int -> Col -> Int
getTotalSizeOccurence pieceLineCountList lineSize countingCol = sum(map (\(a, b) -> getOccurence lineSize b) filteredCol)
  where
    filteredCol = filter (\(a, b) -> a == countingCol) pieceLineCountList

{- | Basic function to find the number of occurrences of a number in a list
-}
getOccurence :: Eq a => a -> [a] -> Int
getOccurence element = length . filter (==element)

{- | Heuristic calculation for a particular line for the Basic AI.
-}
calculateLineScoreBasic :: [(Int,(Int,Int))] -> Int
calculateLineScoreBasic scoreList = sum $ map (\(a, (b, c)) -> ((4 ^ a) * b) - ((4 ^ a) * c)) scoreList

{- | Gets a position using one of the AI's but from the Player's perspective, to generate a hint for the player
-}
getHints :: Board -> [Position]
getHints currentBoard = [fst (minimax playerTree 2 False evaluateAggressive)]
  where
    playerTree  = buildTree genAllMoves currentBoard (playerColour currentBoard)

{- | Get best move function for the basic AI
     it does not use the tree in the same way, it instead,
     checks for unbounded lines of length target -1 and target -2. It then calculates how to block this line.
-}
getBestBasicMove :: Int ->  GameTree -> Position
getBestBasicMove depth currentTree
  | not (null maxTargetNegOne) = if (piecePlayed currentTree fourPlacePos) then fourPlacePos  else  fst (head (next_moves currentTree))
  | not (null maxTargetNegTwo) = if (piecePlayed currentTree threePlacePos) then threePlacePos else fst (head (next_moves currentTree))
  | otherwise = fst (minimax currentTree depth True evaluateAggressive)
  where
    currentBoard = game_board currentTree
    gameTarget = target currentBoard
    playerCol = playerColour currentBoard
    playerPieces = filter (\(pos, col) -> col == playerCol) (pieces currentBoard)
    pieceLineCountList = zip playerPieces (map (\(pos, col) -> getDirectionListNotBlockedBasic (pos, col) currentBoard) playerPieces)
    anyPiecesToBlock = map (\(pos, array) -> ((pos,array), any (\size -> size > ((target currentBoard) - 2)) array)) pieceLineCountList
    urgentMoves = filter (\((pos,array), block) -> block == True) anyPiecesToBlock
    maxTargetNegOne = filter (\((pos,array), block) -> maximum array == gameTarget - 1) anyPiecesToBlock
    maxTargetNegTwo = filter (\((pos,array), block) -> maximum array == gameTarget - 2) anyPiecesToBlock
    fourPlacePos = getPlacePosition currentBoard (fst (fst (fst (head maxTargetNegOne)))) 4
    threePlacePos = getPlacePosition currentBoard (fst (fst (fst (head maxTargetNegTwo)))) 3

{- | Checks if the particular position has a piece on it.
-}
piecePlayed :: GameTree -> Position -> Bool
piecePlayed currentTree piece = any (\pos -> pos == piece) availPos
  where
    availPos = map fst (next_moves currentTree)

{- | Used by the Basic AI to get a different heuristic calculation for unbounded lines
-}
getDirectionListNotBlockedBasic :: (Position,Col) -> Board -> [Int]
getDirectionListNotBlockedBasic (x,y) board = totalList
    where
      posDirectionMoves = map (checkUnblockedFunction board x y (pieces board) 0) posDirectionList
      negDirectionMoves = map (checkUnblockedFunction board x y (pieces board) 0) negDirectionList
      dirMoves = zip posDirectionMoves negDirectionMoves
      totalList = map (\(a, b) -> (fst a + fst b - 1)  * (snd a + snd b)) dirMoves

{- | Used by the basic AI to find the best position to place the piece on to block the enemy player.
-}
getPlacePosition :: Board -> Position -> Int -> Position
getPlacePosition board lineEdge lineSize = (((fst lineEdge) + (fst flippedDir)), ((snd lineEdge) + (snd flippedDir)))
  where
    dirList = zip directionList $ map (checkUnblockedFunction board lineEdge (playerColour board) (pieces board) 0) directionList
    maxDir = snd $ maximum $ map (\(d, (s, b)) -> (s, d)) $ filter (\(d, (s, b)) -> b == 1) dirList
    flippedDir = (((fst maxDir) * (-1)), ((snd maxDir) * (-1)))
