-- Inf2d Assignment 1 2017-2018
-- Matriculation number: s1643102
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import ConnectFour

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6

{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the 3pm Tuesday 13th March 2018.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]


-- The next function should return all the possible continuations of input search branch thr ough the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.
next::Branch-> [Branch]
next branch = [(x + d, y) : branch| d <- [-1,1], x+d>=1, x+d <=6, ((x+d, y) `elem` branch) == False] ++ [(x, y + d) : branch| d <- [-1,1], y+d>=1, y+d<=6, ((x, y+d) `elem` branch) == False]
 where (x,y) = head branch

-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
checkArrival::Node-> Node-> Bool
checkArrival (d1, d2) (c1, c2) = d1 == c1 && d2 == c2

-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.
breadthFirstSearch::Node-> (Branch-> [Branch])-> [Branch]->[Node]-> Maybe Branch
breadthFirstSearch destination next [] exploredList = Nothing
breadthFirstSearch destination next (branch:branches) exploredList
  |checkArrival destination (head branch) = Just branch
  |otherwise = if ((head branch) `elem` exploredList) then breadthFirstSearch destination next (branches) (exploredList) else breadthFirstSearch destination next (branches ++ (next branch)) (head(branch):exploredList)

-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.

depthFirstSearch::Node-> (Branch-> [Branch])-> [Branch]-> [Node]-> Maybe Branch
depthFirstSearch destination next [] exploredList = Nothing
depthFirstSearch destination next (branch:branches) exploredList
  |checkArrival destination (head branch) = Just branch
  |otherwise = if ((head branch) `elem` exploredList) then depthFirstSearch destination next (branches) (exploredList) else depthFirstSearch destination next ((next branch) ++ branches) (head(branch):exploredList)


-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree..

depthLimitedSearch::Node-> (Branch-> [Branch])-> [Branch]-> Int-> [Node]-> Maybe Branch
depthLimitedSearch destination next [] d [] = Nothing
depthLimitedSearch destination next (branch:branches) d []
 |checkArrival destination (head branch) = Just branch
 |otherwise  = if (length branch) > d then depthLimitedSearch destination next (branches) d [] else depthLimitedSearch destination next ((next branch) ++ branches) d []

-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
iterDeepSearch:: Node-> (Branch-> [Branch])-> Node-> Int-> Maybe Branch
iterDeepSearch destination next initialNode d = undefined

-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the current position and the destination position.
manhattan::Node-> Node-> Int
manhattan (p1, p2) (d1, d2) = abs (p1 - d1) + abs (p2 - d2)

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.
bestFirstSearch:: Node-> (Branch-> [Branch])-> (Node->Int)-> [Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next manhattan [] exploredList = Nothing
bestFirstSearch destination next manhattan (branch:branches) exploredList
  |checkArrival destination (head branch) = Just branch
  |otherwise = if ((head branch) `elem` exploredList) then bestFirstSearch destination next manhattan (sortBy (compareBranchesBFS destination) [b | b <- branches] ) (exploredList)
  else bestFirstSearch destination next manhattan (sortBy (compareBranchesBFS destination) [b | b <- (branches ++ (next branch))] ) (head(branch):exploredList)

compareBranchesBFS:: Node -> Branch -> Branch -> Ordering
compareBranchesBFS destination br1 br2
  |manhattan (head br1) destination <= manhattan (head br2) destination = LT
  |otherwise = GT


-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch:: Node-> (Branch-> [Branch])-> (Node->Int)-> (Branch-> Int)-> [Branch]-> [Node]-> Maybe Branch
aStarSearch destination next manhattan cost [] exploredList = Nothing
aStarSearch destination next manhattan cost (branch:branches) exploredList
  |checkArrival destination (head branch) = Just branch
  |otherwise = if ((head branch) `elem` exploredList) then aStarSearch destination next manhattan cost (sortBy (compareBranchesAStar destination) [b | b <- branches] ) (exploredList)
  else aStarSearch destination next manhattan cost (sortBy (compareBranchesAStar destination) [b | b <- (branches ++ (next branch))] ) (head(branch):exploredList)


-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch-> Int
cost branch = length branch

compareBranchesAStar:: Node -> Branch -> Branch -> Ordering
compareBranchesAStar destination br1 br2
  |manhattan (head br1) destination + cost br1 <= manhattan (head br2) destination + cost br2 = LT
  |otherwise = GT




-- In this section, the function determines the score of a terminal state, assigning it a value of +1, -1 or 0:
eval :: Game-> Int
eval game
  |terminal game = if checkWin game 1 then 1 else if checkWin game 0 then -1 else 0
  |otherwise = 0

-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Role-> Game-> Int
minimax player game =
       if terminal game then eval game
       else if player == 0 then minimum([minimax (switch player) move | move <- moves game player])
       else maximum([minimax (switch player) move | move <- moves game player])

-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

alphabeta:: Role-> Game-> Int
alphabeta player game | player == 1 = maxValue game player (-2,2)
                      | otherwise   = minValue game player (-2,2)
    where maxValue g player (a,b) | terminal g = eval g
                           | otherwise = snd $ foldr (\g' (a',v) ->
                                       let newV = max v (minValue g' 0 (a',b)) in
                                       if v >= b
                                       then (a',v) -- If a state with an evaluation higher than beta has been found, we don't bother checking any further nodes on this branch.
                                       else (max a' newV, newV)) -- If not, we calculate the new alpha and v values.
                                (a,-2) (moves g player)
          minValue g player (a,b) | terminal g = eval g
                           | otherwise = snd $ foldr (\g' (b',v) ->
                                       let newV = min v (maxValue g' 1 (a,b')) in
                                       if v <= a
                                       then (b',v) -- If a state with an evaluation lower than alpha has been found, ignore the rest of the nodes on this branch.
                                       else (min b' newV, newV)) -- If not, calculate the new beta and v values.
                                (b,2) (moves g player)


{- Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms below.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
-}
