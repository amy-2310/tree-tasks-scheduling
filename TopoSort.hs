module TopoSort (topo_sort, Relation(Rel)) where
import Data.List
import Data.Maybe

--data type for relation.
--Rel 1 3 [2,4] means "the third operation of process 1 generates process 2 and 4."
data Relation = Rel PId OId [PId]
        deriving (Show,Eq, Ord)
type PId = Int
type OId = Int

-------------------------------------------------------------------------------------------------------------------
{-
Sort a list of relations with topological sorting: for two relations x and y,
if y is in the sub-tree having x as root, than y should be in front of x in the 
sorted list. This sorted list will be used to generate recursive data type of 
tree-like processes: the sub-process that does not generate
any sub-procee will be inserted into its generator-operation first, and so on...
Total time
= Time for building the adjacency list + time for sorting
= O(R^2) + O(R^2) = O(R^2)
-}
topo_sort :: [Relation] -> [Relation]
topo_sort relations = topo_sort_h (build_adjList relations) []

{-
build_adjList builds a directed adjacency list for relations.
An element of the adjacency list is a 3-er tupel:
(relation (x), number of relations(predecessors) that x is pointed to,
list of the predecessors)
If a relation (Rel pid oid subps) has a predecessor, it means that the
predecessor located in the sub-tree having oid as root. 
Time = O(R^2) where R = number of relations
-}
build_adjList :: [Relation] -> [(Relation,Int,[Relation])]
build_adjList relations =
    build_adjList_h relations relations
    where
    build_adjList_h :: [Relation] -> [Relation] -> [(Relation,Int,[Relation])]
    build_adjList_h [] _ = []
    build_adjList_h (r:rs) relations =
        let preds = find_pred r relations
        in (r,length preds, preds):(build_adjList_h rs relations)
    
    --find predecessors of a relation
    find_pred :: Relation -> [Relation] -> [Relation]
    find_pred _ [] = []
    find_pred r@(Rel pid oid pids) (mayb_pred@(Rel pid2 oid2 pids2):rest)
        | pid2 `elem` pids = [mayb_pred] ++ (find_pred r rest)
        | pid == pid2 && oid < oid2 = [mayb_pred] ++ (find_pred r rest)
        |otherwise = (find_pred r rest)


{-
Help-function for topo_sort: sort the relations recursively.
In each round, find the next relation that does not have any predecessor in the
adjacency list. Insert it to the sorted list and update the adjacency list. Go
to the next round.
Total Time = number of recursions * time for each recursion = R*O(R) = O(R^2)
-}
topo_sort_h :: [(Relation,Int,[Relation])] -> [Relation] -> [Relation]
topo_sort_h adjList order =
    let mayb_next = (next_relation adjList)
    in
        if isNothing mayb_next
        then reverse order
        else  let next = fromJust mayb_next
                  adjList_new = new_adjList next adjList
                  toRelation (r,n,rs) = r
              in topo_sort_h adjList_new ((toRelation next):order)
    where
    --find the next relation
    next_relation :: [(Relation,Int,[Relation])] -> Maybe (Relation,Int,[Relation])
    next_relation adjList = find (\(r, n, preds) -> n == 0) adjList
    
    --generate a new adjlist with new numbers of predecessors
    new_adjList :: (Relation,Int,[Relation]) -> [(Relation,Int,[Relation])]
                    -> [(Relation,Int,[Relation])]
    new_adjList _ [] = []
    new_adjList next_r@(r', n', preds') ((r, n, preds):rs)
     | r' == r = (r, (-1), preds): new_adjList next_r rs
     | r' `elem` preds = (r, (n-1), preds): new_adjList next_r rs
     | otherwise = (r, n, preds): new_adjList next_r rs                 
       
