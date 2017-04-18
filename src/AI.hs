
module AI where

import Board
import Debug.Trace
import Args
import Data
import Utils
import Params
import Data.List

-- Currently just generate all positions, update to make smarter generations
{-genAllMoves :: Board -> Col -> [Position]
genAllMoves b c = [(x, y) | x <- [1..s],
                            y <- [1..s] ]
    where
        s = size b
-}

testBoard = Board 19 5 [((2,3), Black), ((1,3), Black), ((1,2), Black), ((2,2), White), ((1,1), White)] Black

genAllMoves :: Board -> Col -> [Position]
genAllMoves board colour = if null matchedPositions then allPositions else matchedPositions
  where
    allPositions = [(i, j) | i <- [1 .. size board], j <- [1 .. size board] ]
    allMatchedPositions = filter (checkNeighbour board 2) allPositions
    existingPositions = map fst (pieces board)
    matchedPositions = filter (\p -> notElem p existingPositions) allMatchedPositions

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

getCurrentTreeMax :: World -> GameTree
getCurrentTreeMax w = buildTree genAllMoves b t
    where
        b = board w
        t = turn w

--testTree = buildTree genAllMoves testBoard White

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> World
               -> Position
getBestMove meowdepth currentTree w = trace (show (bestMoveTuple)) fst bestMoveTuple
  where
    aiTypeToPlay = aiType w
    bestMoveTuple = if  aiTypeToPlay == Defensive then minimax currentTree meowdepth True evaluateDefensive else minimax currentTree meowdepth True evaluateAggressive

--getBestMove depthAI currentTree = (fst (head (next_moves currentTree)))
        -- | depthAI == 0 = (fst (head (shuffle currentTree)))
        -- | otherwise = getBestMove (depthAI - 1) (snd (head (shuffle currentTree)))
-- traversing tree code kinda done ^^

makeAIMove :: Board -> Col -> Position -> Board
makeAIMove board col pos = board{pieces = ((pos,col):pieces board)}

updateMultiplayerWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
updateMultiplayerWorld t w = w


-- Update the world state after some time has passed
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
    nextMovePos = if basicAI then trace (show (aiType w)) getBestBasicMove depthAI currentTree else getBestMove depthAI currentTree w
    newWorld = case playRule (rule w) (board w) (turn w) nextMovePos of
                 (_, Nothing)             -> w
                 (newRule, Just newBoard) -> w { board = newBoard, rule = newRule, turn = other (turn w), timeElapsed = 0 }
    newTime = if paused w then timeElapsed w else timeElapsed w + t

updateWorldNoAI :: Float -- ^ time since last update (you can ignore this)
           -> World -- ^ current world state
           -> World
updateWorldNoAI t w = w { timeElapsed = timeElapsed w + t }

chooseUpdateWorld :: Float -> World -> IO World
chooseUpdateWorld t w = if playAI w then return (updateWorld t w) else return (updateWorldNoAI t w)

{- Hint: 'updateWorld' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateWorld should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 At first, it is reasonable for this to be a random move!

 If both players are human players, the simple version above will suffice,
 since it does nothing.

 In a complete implementation, 'updateWorld' should also check if either
 player has won and display a message if so.
-}


safeMax :: [(Int, Position)] -> (Int, Position)
safeMax scores
  | null scores = (0, (0, 0))
  | otherwise = maximum scores

minimax :: GameTree -> Int -> Bool -> (Board -> Int) -> (Position, Int)
minimax currentTree depth maximise evaluateFunc
  | checkWinAI currentBoard (playerColour currentBoard) depth =  (lastPlayedPiece, (minBound :: Int) + 20 - depth)--trace (show currentBoard) (lastPlayedPiece, minBound :: Int)
  | checkWinAI currentBoard (other (playerColour currentBoard)) depth = (lastPlayedPiece, (maxBound :: Int) - 20 + depth)--trace (show currentBoard) (lastPlayedPiece, maxBound :: Int)
  | (depth == 0) || (null nextMoves) = (lastPlayedPiece, evaluateFunc currentBoard) --trace (show currentBoard) (lastPlayedPiece, evaluateFunc currentBoard)
  | maximise = (snd maxScoreMove, fst maxScoreMove)
  | otherwise =  (snd minScoreMove, fst minScoreMove)
  where
    currentBoard = game_board currentTree
    (nextMoves, nextTrees) = unzip (next_moves currentTree)
    scores = map (\t -> snd (minimax t (depth - 1) (not maximise) evaluateFunc)) nextTrees
    maxScoreMove = safeMax $ zip scores nextMoves
    minScoreMove = minimum $ zip scores nextMoves
    lastPlayedPiece = fst (head (pieces currentBoard))

evaluateDefensive :: Board -> Int
evaluateDefensive leafBoard = evaluateAdv leafBoard True

evaluateAggressive :: Board -> Int
evaluateAggressive leafBoard = evaluateAdv leafBoard False

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


calculateLineScore :: Board -> Col -> Int -> Bool -> Int
calculateLineScore board col numLines defensive
  | col == aiCol = numLines--4 ^ numLines
  | otherwise = if defensive then  numLines * (-10) else numLines * (-3)--(4 ^ numLines) * (-2) --if numLines > 1 then (minBound :: Int) + 1 else 0
  where
    aiCol = other (playerColour board)



posDirectionList :: [Position]
posDirectionList = [(0,1), (1,0), (1,1), (1,-1)]

negDirectionList :: [Position]
negDirectionList = [(0,-1), (-1,0), (-1,-1), (-1,1)]


getDirectionListNotBlocked :: (Position,Col) -> Board -> [Int]
getDirectionListNotBlocked (x,y) board = totalList
    where
      -- (x,y) = head (pieces board)
      posDirectionMoves = map (checkUnblockedFunction board x y (pieces board) 0) posDirectionList
      negDirectionMoves = map (checkUnblockedFunction board x y (pieces board) 0) negDirectionList
      tupleDirectionMoves = zip posDirectionMoves negDirectionMoves
      totalList = map (\(a, b) -> (4 ^ (fst a + fst b + (-1))) * (snd a + snd b)) tupleDirectionMoves
      --allCounts = getUnboundTotals allDirectionMoves [] 0
      --list = map (subtract 1) allCounts

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

checkOutOfBounds :: Board -> Position -> Bool
checkOutOfBounds currentBoard (x, y)
  | (x > bSize) || (x < (bSize * (-1))) = True
  | (y > bSize) || (y < (bSize * (-1))) = True
  | otherwise = False
  where
    bSize = size currentBoard

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


evaluateBoardBasic :: Board -> Int
evaluateBoardBasic leafBoard
  | playerWin = trace "PLAYER WILL FUCKING WIN" minBound :: Int
  | aiWin = (maxBound :: Int)
  | otherwise = calculateLineScoreBasic blackWhiteTotals
  where
    playerCol = playerColour leafBoard
    aiWin = checkWinAI leafBoard (other playerCol) depthAI
    playerWin = checkWinAI leafBoard playerCol depthAI
    pieceLineCountList = map (\(pos, col) -> (col, getDirectionList (pos, col) leafBoard)) (pieces leafBoard)
    lineSizeTotalsWhite= map (\a -> getTotalSizeOccurence pieceLineCountList a (other (playerColour leafBoard))) [2..(target leafBoard)]
    lineSizeTotalsBlack = map (\a -> getTotalSizeOccurence pieceLineCountList a (playerColour leafBoard)) [2..(target leafBoard)]
    blackWhiteTotals = zip [2..(target leafBoard)] $ zip lineSizeTotalsWhite lineSizeTotalsBlack


getTotalSizeOccurence :: [(Col, [Int])] -> Int -> Col -> Int
getTotalSizeOccurence pieceLineCountList lineSize countingCol = sum(map (\(a, b) -> getOccurence lineSize b) filteredCol)
  where
    filteredCol = filter (\(a, b) -> a == countingCol) pieceLineCountList


getOccurence :: Eq a => a -> [a] -> Int
getOccurence element = length . filter (==element)

calculateLineScoreBasic :: [(Int,(Int,Int))] -> Int
calculateLineScoreBasic scoreList = sum $ map (\(a, (b, c)) -> ((4 ^ a) * b) - ((4 ^ a) * c)) scoreList


evaluateSimple :: Board -> Int
evaluateSimple currentBoard
  | playerWin = minBound :: Int
  | aiWin = (maxBound :: Int) - 1
  | otherwise = getOccurence (other (playerColour currentBoard)) piecess - (getOccurence (playerColour currentBoard) piecess)
  where
    playerCol = playerColour currentBoard
    aiWin = checkWinAI currentBoard (other playerCol) depthAI
    playerWin = checkWinAI currentBoard playerCol depthAI
    piecess = map snd $ pieces currentBoard


getHints :: Board -> [Position]
getHints currentBoard = [fst (minimax playerTree 2 False evaluateBoardBasic)]
  where
    playerTree  = buildTree genAllMoves currentBoard (playerColour currentBoard)



getBestBasicMove :: Int ->  GameTree -> Position
getBestBasicMove depth currentTree
  | not (null maxTargetNegOne) = if (piecePlayed currentTree fourPlacePos) then fourPlacePos  else  fst (head (next_moves currentTree))
  | not (null maxTargetNegTwo) = if (piecePlayed currentTree threePlacePos) then threePlacePos else fst (head (next_moves currentTree))
  | otherwise = fst (minimax currentTree depth True evaluateAggressive)
  where
    currentBoard = game_board currentTree
    gameTarget = target currentBoard
    playerCol = playerColour currentBoard
    -- aiWin = checkWinAI currentBoard (other playerCol) depthAI
    -- playerWin = checkWinAI currentBoard playerCol depthAI
    playerPieces = filter (\(pos, col) -> col == playerCol) (pieces currentBoard)
    pieceLineCountList = zip playerPieces (map (\(pos, col) -> getDirectionListNotBlockedBasic (pos, col) currentBoard) playerPieces)
    anyPiecesToBlock = map (\(pos, array) -> ((pos,array), any (\size -> size > ((target currentBoard) - 2)) array)) pieceLineCountList
    urgentMoves = filter (\((pos,array), block) -> block == True) anyPiecesToBlock
    maxTargetNegOne = filter (\((pos,array), block) -> maximum array == gameTarget - 1) anyPiecesToBlock
    maxTargetNegTwo = filter (\((pos,array), block) -> maximum array == gameTarget - 2) anyPiecesToBlock
    fourPlacePos = getPlacePosition currentBoard (fst (fst (fst (head maxTargetNegOne)))) 4
    threePlacePos = getPlacePosition currentBoard (fst (fst (fst (head maxTargetNegTwo)))) 3



piecePlayed :: GameTree -> Position -> Bool
piecePlayed currentTree piece = any (\pos -> pos == piece) availPos
  where
    availPos = map fst (next_moves currentTree)


-- getUrgentMove :: [(Position,Bool)] -> Board -> Position
-- getUrgentMove urgentMoves board = finalMove
--   where
--     directionScores = map (\((x,y),bool) -> ((x,y),(map (checkUnblockedFunction board x y  0) directionList)) urgentMoves
--     maxScores = map (
    
    


getDirectionListNotBlockedBasic :: (Position,Col) -> Board -> [Int]
getDirectionListNotBlockedBasic (x,y) board = totalList
    where
      -- (x,y) = head (pieces board)
      posDirectionMoves = map (checkUnblockedFunction board x y (pieces board) 0) posDirectionList
      negDirectionMoves = map (checkUnblockedFunction board x y (pieces board) 0) negDirectionList
      dirMoves = zip posDirectionMoves negDirectionMoves
      totalList = map (\(a, b) -> (fst a + fst b - 1)  * (snd a + snd b)) dirMoves
      --allCounts = getUnboundTotals allDirectionMoves [] 0
      --list = map (subtract 1) allCounts


    -- pieceLineCountList = map (\(pos, col) -> sum(getDirectionListNotBlocked (pos, col) leafBoard)) (pieces leafBoard)
    -- pieceColourList = map (\(pos, col) -> col) (pieces leafBoard)
    -- pieceScoreList = zip pieceColourList pieceLineCountList
getPlacePosition :: Board -> Position -> Int -> Position
getPlacePosition board lineEdge lineSize = (((fst lineEdge) + (fst flippedDir)), ((snd lineEdge) + (snd flippedDir)))
  where
    dirList = zip directionList $ map (checkUnblockedFunction board lineEdge (playerColour board) (pieces board) 0) directionList 
    maxDir = snd $ maximum $ map (\(d, (s, b)) -> (s, d)) $ filter (\(d, (s, b)) -> b == 1) dirList
    flippedDir = (((fst maxDir) * (-1)), ((snd maxDir) * (-1)))
    
