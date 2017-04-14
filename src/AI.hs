
module AI where

import Board
import Debug.Trace
import Data.Random.Extras

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }

depthAI :: Int
depthAI = 2

updateLoopTime :: Float
updateLoopTime = 2.0

-- Currently just generate all positions, update to make smarter generations
genAllMoves :: Board -> Col -> [Position]
genAllMoves b c = [(x, y) | x <- [1..s],
                            y <- [1..s] ]
    where
        s = size b

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
               Just b' -> (pos, buildTree gen b' c) : mkNextStates xs
                             -- successful, make move and build tree from
                             -- here for opposite player

getCurrentTreeMax :: World -> GameTree
getCurrentTreeMax w = buildTree genAllMoves b t
    where
        b = board w
        t = turn w

getCurrentTreeMin :: World -> GameTree
getCurrentTreeMin w = buildTree genAllMoves b t
    where
        b = board w
        t = other (turn w)

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove depth currentTree = fst bestMoveTuple
  where
    bestMoveTuple = minimax currentTree depth
    
--getBestMove depthAI currentTree = (fst (head (next_moves currentTree)))
        -- | depthAI == 0 = (fst (head (shuffle currentTree)))
        -- | otherwise = getBestMove (depthAI - 1) (snd (head (shuffle currentTree)))
-- traversing tree code kinda done ^^

-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
--rupdateWorld t w = w
updateWorld t w = case won w of
                    True -> w
                    False -> case nextMove of
                              Nothing -> w
                              Just move -> case checkWon move of
                                             Nothing -> if colour == optionCol then w
                                                        else nextWorld{board = move} -- Update World with new move. Also send t server
                                             Just col -> trace ("col: " ++ show(col) ++ " won") nextWorld {won = True}
                     where
                         optionList = option w
                         optionCol = optColour optionList
                         currentTree = getCurrentTreeMax w
                         b = board w
                         colour = turn w
                         next_t = other colour
                         nextMovePos = getBestMove depthAI currentTree
                         nextMove = makeMove b colour nextMovePos
                         nextWorld = w {turn = next_t}
                         --newBoard = board nextWorld

updateWorldNoAI :: Float -- ^ time since last update (you can ignore this)
           -> World -- ^ current world state
           -> World
updateWorldNoAI t w = w

chooseUpdateWorld :: Float -> World -> World
chooseUpdateWorld f w = case boolAI of
                          False -> w
                          True -> updateWorld f w
  where
    options = option w
    boolAI = ai options
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


minimax :: GameTree -> Int -> (Position, Int)
minimax currentTree depth
  | depth == 0 = evaluateBoard (game_board currentTree)
  | maxPlayer = maxMove (next_moves currentTree) depth 
  | otherwise = minMove (next_moves currentTree) depth 
  where
    maxPlayer = (game_turn currentTree) == Black

maxMove :: [(Position, GameTree)] -> Int -> (Position, Int)  
maxMove [x] depth = minimax nextTree (depth - 1)
  where
    xBoard = game_board (snd x)
    nextTree = snd x
maxMove (x:xs) depth   
    | xBoardScore > maxTail = ((fst x), (snd xBoardScore))
    | otherwise = maxTail
    where
      maxTail = maxMove xs depth
      xBoard = game_board (snd x)
      nextTree = snd x
      xBoardScore = minimax nextTree (depth - 1)

minMove :: [(Position, GameTree)] -> Int -> (Position, Int)  
minMove [x] depth = minimax nextTree (depth - 1)
  where
    xBoard = game_board (snd x)
    nextTree = snd x
minMove (x:xs) depth   
    | xBoardScore < maxTail = ((fst x), (snd xBoardScore))
    | otherwise = maxTail
    where
      maxTail = maxMove xs depth
      xBoard = game_board (snd x)
      nextTree = snd x
      xBoardScore = minimax nextTree (depth - 1)

-- maximisingPlayer :: GameTree -> Int -> (Position, Int)
-- maximisingPlayer = do
--   let bestMove = minBound :: Int
  

evaluateBoard :: Board -> (Position, Int)
evaluateBoard leafBoard = ((1, 1), 1)
