module AI where

import Board
import Data.Random.Extras

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }

depthAI :: Int
depthAI = 1

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

getCurrentTree :: World -> GameTree
getCurrentTree w = buildTree genAllMoves b t
    where
        b = board w
        t = turn w

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.
getBestMove :: Int -- ^ Maximum search depth
               -> GameTree -- ^ Initial game tree
               -> Position
getBestMove depthAI currentTree = (fst (head (next_moves currentTree)))
        -- | depthAI == 0 = (fst (head (shuffle currentTree)))
        -- | otherwise = getBestMove (depthAI - 1) (snd (head (shuffle currentTree)))
-- traversing tree code kinda done ^^





-- Update the world state after some time has passed
updateWorld :: Float -- ^ time since last update (you can ignore this)
            -> World -- ^ current world state
            -> World
updateWorld t w = w
-- updateWorld t w = nextWorld -- Update World with new move. Also send t server
--     where
--         currentTree = getCurrentTree w
--         b = board w
--         t = turn w
--         next_t = other t
--         nexMovePos = getBestMove depthAI currentTree
--         nextMove = makeMove b t nexMovePos
--         nextWorld = nextMove next_t



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
