module Search (State(..), aStar) where

import Data.Map ((!), Map, empty, insert, member, singleton)
import Data.Maybe (fromJust, isNothing, maybe)
--import Data.Set as S
import Data.Set ((\\), empty, insert, Set, toList)
import Data.PSQueue (alter, Binding ((:->)), findMin, minView, null, PSQ, singleton)

data State k c = State {frontier :: PSQ k c,
                        visited  :: Set k,
                        cameFrom :: Map k k,
                        score    :: Map k c} deriving (Show)

aStar :: (Num c, Ord k, Ord c) => k -> (k -> Set k) -> (k -> k -> c) -> (k -> c) -> (k -> Bool) -> State k c
aStar start children dist heuristic isGoal = aStarR initialState where
  initialState = State {frontier = (Data.PSQueue.singleton start (heuristic start)),
                        visited = Data.Set.empty,
                        cameFrom = Data.Map.empty,
                        score = Data.Map.singleton start 0}
  aStarR state = case minView (frontier state) of
    Nothing -> state
    Just (x :-> _, frontierRest) ->
      if isGoal x then state
      else aStarR $ foldr upState (state {visited = Data.Set.insert x (visited state),
                                          frontier = frontierRest}) (Data.Set.toList (children x \\ visited state)) where
      upState y state = if y `member` score state && (score state) ! y < score state ! x + dist x y then state else link x y state where
        link x y state = state {frontier = frontFold (y, (score state) ! x + dist x y + heuristic y) (frontier state),
                                cameFrom = Data.Map.insert y x (cameFrom state),
                                score    = Data.Map.insert y ((score state) ! x + dist x y) (score state)}
  frontFold (k, p) = alter (insertDec p) k
  insertDec p Nothing = Just p
  insertDec p (Just q)  | p < q     = Just p
                        | otherwise = Just q



treeSearch start isGoal children = treeSearchR (Data.PSQueue.singleton start 0) isGoal children Data.Set.empty

treeSearchR :: (Ord k, Ord p) => PSQ k p -> (k -> Bool) -> (k -> [(k, p)]) -> Set k -> Maybe k
treeSearchR frontier isGoal children explored | Data.PSQueue.null frontier = Nothing
--                                              | isNothing mBinding = Nothing -- Should be unneccesary
                                              | isGoal x = Just x
                                              | otherwise = nextStep where
                                        nextStep = treeSearchR newFrontier isGoal undefined (Data.Set.insert x explored)
                                        mBinding      = minView frontier
                                        ((x :-> _), frontierRest) = fromJust mBinding
                                        
                                        newFrontier = foldr frontFold frontierRest (children x)
                                        frontFold (k, p) = alter (insertDec p) k
                                        insertDec p Nothing = Just p
                                        insertDec p (Just q)  | p < q     = Just p
                                                              | otherwise = Just q