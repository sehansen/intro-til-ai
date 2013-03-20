import Data.Map (Map, empty, singleton)
import Data.Maybe (fromJust, isNothing, maybe)
--import Data.Set as S
import Data.Set (empty, insert, Set)
import Data.PSQueue (alter, Binding ((:->)), findMin, minView, null, PSQ, singleton)

data State k c = State {frontier :: PSQ k c,
                        visited  :: Set k,
                        cameFrom :: Map k k,
                        score    :: Map k c}

aStar start children heuristic = aStarR initialState children heuristic where
  initialState = State {frontier = (Data.PSQueue.singleton start (heuristic start)),
                        visited = Data.Set.empty,
                        cameFrom = Data.Map.empty,
                        score = Data.Map.singleton start 0}
  aStarR state children heuristic = case minView (frontier state) of
    Nothing -> state
    Just (current :-> _, frontierRest) -> aStarR (state {frontier = add (children current) frontierRest}) children heuristic
  ---- Note: Dette er en uskøn blanding af A* og uniform cost fra her
  add childList frontier = foldr frontFold frontier childList
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
                                        nextStep = treeSearchR newFrontier isGoal undefined (insert x explored)
                                        mBinding      = minView frontier
                                        ((x :-> _), frontierRest) = fromJust mBinding
                                        
                                        newFrontier = foldr frontFold frontierRest (children x)
                                        frontFold (k, p) = alter (insertDec p) k
                                        insertDec p Nothing = Just p
                                        insertDec p (Just q)  | p < q     = Just p
                                                              | otherwise = Just q