module OptSpaceBF (best_first, print_best_first) where
import TopoSort --ony the data type Relation is needed, not the toposort
import qualified Data.Heap as Heap
import Data.List

--data types of heap-element
{-
A heap is used to implement a search tree.
Mspace is the peak space consumption from root to this element.
Space is the space consumption of the state.
State is a state of a schedule and consists of index of operations.
The list is used to memorize the path from root to this element.
-}
data HeapElement = HeapE MSpace Space State [HeapElement]
        deriving (Show, Eq, Ord)

type MSpace = Int
type Space = Int
type State = [Index]
type Index = Int


--Input data types
type Input_Process = (PId, [Space])
--Important!! The processes in the list should be arranged in ascending order 
--according to PId
type Input_Program = [Input_Process]

--Output dara types
data Operation_out = OP PId OId Space deriving (Show, Eq, Ord)
type Output_State = [Operation_out] --a state in a schedule
type Output_Schedule = [(Space, Output_State)]
type OId = Int
type PId = Int
--------------------------------------------------------------------------------
{-
Two main functions for calculating a space-optimal schedule of tree-like
processes.
-}
best_first :: Input_Program -> [Relation] -> Output_Schedule
best_first ps rs = let --initiate the heap by inserting the first element into a list
                       heap = [startElement ps rs]
                       
                       --generate the last state according to the given program
                       endState' = endState ps
                       
                       --conduct the search by calling the help-function best_first_h,
                       --which returns the path of the best solution
                       path = best_first_h heap ps rs endState' []
                   
                   --convert the path back into a schedule    
                   in toSchedule path ps

print_best_first :: Input_Program -> [Relation] -> IO()
print_best_first ps rs = print_list $ best_first ps rs
--------------------------------------------------------------------------------
--help-functions for best_first
{-
toSchedule converts the path (a list of heap elements) returned by best_first_h
into a schedule.
-}
toSchedule :: [HeapElement] -> Input_Program -> Output_Schedule
toSchedule path ps =
    let 
        --Firtly, pack the states of the path into a list.
        --Since each state consists only of indexes but a state of out-put
        --schedule consists of operations, all indexes must be converted
        --into operations:
        --1. transpose the state-list so that the first element of
        --the transposed list contains op-indexs of the first process,
        --and the second element of the list contains op-indexes
        --of the second process, and so on.  
        indexLists = transpose $ getStateList path

        --2. convert all indexes into operations in the transposed list
        opLists = indexListsToOpLists [] indexLists ps

        --3. transpose the above list back. Now the states in the list
        --consist of ops instead of only indexes.
        stateLists = transpose opLists

        --get list of space of states
        spList = getSpaceList path
        --conbine the state-list and the space list to get the final
        --out-put schedule
    in addSpaceToStateList [] stateLists spList
    where
    --help-functions:
    --pack the space of all states into a list
    getSpaceList path = map (\(HeapE _ sp _ _) -> sp) path 

    --pack all states into a list
    getStateList path = map (\(HeapE _ _ state _) -> state) path

    --go through the transposed list and process list recursively
    --to convert all indexes into operations
    indexListsToOpLists opLists (pi:piRest) (p:pRest)
        = let opList = map(\index -> if index == 0
                                     then OP (fst p) index 0
                                     else OP (fst p) index (snd p !! (index-1))
                           ) pi
          in indexListsToOpLists (opList:opLists) piRest pRest

    indexListsToOpLists opLists [] [] = reverse opLists

    --go through the state-list and space-list recursively to 
    --combine each state with its space.
    addSpaceToStateList schedule (state:sRest) (space:spRest)
        = let stateOfSchedule = (space, state)
          in addSpaceToStateList (stateOfSchedule:schedule) sRest spRest
    addSpaceToStateList schedule [] [] = reverse schedule
{-
best_first_h expands a search tree recursively until a compleate optimal schedule
is found. It returns a path as output.
All the open-leaves (HeapElement) are saved in a heap-list. In each
round, the min-element of the heap-list is omitted and checked if it has the
final state:
if it doesn't, all of its valid possible children are generated and
inserted into the heap which is then sorted. The function then calls itself again
with the new-heap as one of its arguments.
If the min-element does have the last state, the path saved in it is returned
and the recursion stops.
Arguments: heap-list -> input-program -> relations -> end state -> visted states
-> path
-}                 
best_first_h :: [HeapElement] -> Input_Program -> [Relation]-> State -> [State]
                -> [HeapElement]

best_first_h heap@(minElement:rest) ps rs endState vStates =
  if getState minElement == endState
  --Get the path saved in minElement and insert the minElement into the path.
  --Reverse the path and return it as output.
  then reverse $ (clearPath minElement):(getPath minElement)
  else
   let 
    --Generate the valid children of minElement
    next_elements
        = toElements (nextStates (getState minElement) endState heap rs vStates)
                     minElement ps
    --Insert them into the heap without the minElement.   
    heap' = Heap.sort (next_elements ++ rest)
   
   --go to the next round with the new heap and new list of visited states
   in best_first_h heap' ps rs endState ((getState minElement):vStates)

best_first_h [] _ _ _ _ = [] --should not happen

{-
startElement generates a start-element for the best-first tree according to the
given program and relations.
The help-function startElement_h generates the start-state by inserting 1 or 0
as index into the startState. If it is a subprocess, the index should be 0, else 1
-}
startElement :: Input_Program -> [Relation] -> HeapElement
startElement ps rs =
    let startState = startElement_h [] ps rs
        space = getSpaceOfState startState ps
        mspace = space
    in HeapE mspace space startState []
    where
    --help-function
    startElement_h startState ((pid, spaces):ps) rs
        = if isSubprocess pid then startElement_h (1:startState) ps rs
          else startElement_h (0:startState) ps rs
        where isSubprocess x = not $ any (\(Rel _ _ subPs) -> elem x subPs) rs    
    
    startElement_h startState [] _ = reverse startState


--generate the end-state, which contains the last operation-index of every process
endState :: Input_Program -> State
endState ps = map (\(pid,sps) -> length sps) ps

--generate all possible next states of a state
nextStates :: State -> State -> [HeapElement] -> [Relation] -> [State] -> [State]
nextStates state endState heap relations vStates =
  let 
    --all possible (valid and invalid) next states of the state
    nextStates_temp = generate_next state
  
  --filter to get the valid ones
  in filter (\state' -> --not violate the restrictions
                       check_state_rs state' relations &&
                       
                       --not already in heap
                       (not $ elem state' $ map getState heap) &&
                       
                       --no index exceeds
                       (not $ any (\[oid',oid] -> oid' > oid) $ transpose [state', endState]) &&
                       
                       --not already visited
                       (not $ elem state' vStates) &&
                       
                       --not the current state
                       state' /= state
            ) nextStates_temp
  where
  --help-functions:
  --generate all possible next states
  generate_next (oid:rest) = [a:b | a <- [oid,oid+1], b <- generate_next rest]
  generate_next [] = [[]]
  
  --check if a state violates any restriction in the restriction-list      
  check_state_rs state' rList = all (\rel -> check_state state' rel) rList
  
  --check if a state violates a restriction
  check_state state' (Rel pid oid subPlist) =
    let --index of op of process pid in the current state
        index_main_o = state !! (pid-1)
        
        --index of op of process pid in a new state
        index_main_n = state' !! (pid-1)
              
        --last index of process pid
        index_main_l = endState !! (pid-1)
    in --normal case: the generator-op is not the last op
       (all (\spid -> --for all sub-processes in subPlist, either
                      --the generator-op of process pid has not been executed
                      --and the sub-procee has not been started, 
                      (index_main_n <= oid && state' !! (spid-1) == 0) ||
                      --or the generator-op has been execuated and the sub-process
                      --has been started.
                      (index_main_n > oid && state' !! (spid-1) > 0)
            ) subPlist
       ) ||
       --special case: the generator-op is the last op
       (
        --the generator-op is the last op 
        index_main_n == index_main_l &&
        
        --the generator-op should be executed in the next state
        index_main_o == oid && --oid is in the current state
        index_main_n == oid && --oid is in the next state
        
        --all sub-processes in subPlist should be started in the next state
        (all (\spid -> state' !! (spid-1) > 0) subPlist)
       )
                                    

--convert all next states to heap-elements
toElements :: [State] -> HeapElement -> Input_Program -> [HeapElement]
toElements states element ps =
  let mspace_before = getMSpace element --mspace of current min-element
      path = getPath element --path of current min-element
  in map (\state -> let space = getSpaceOfState state ps
                    in HeapE (max mspace_before space) space state
                             (clearPath element:path)
          ) states




--------------------------------------------------------------------------------
--some general basic functions

--calculate the space consumption of a state
getSpaceOfState :: State -> Input_Program -> Int
getSpaceOfState state ps =
    getSpaceOfState_h state ps 0
    where
       --go through the ops in a state and the process-list recursively. Add up
       --the looked up space during the recursion and return the sum as output.
       getSpaceOfState_h (oid:sRest) ((pid, sps):pRest) sum
         = if oid == 0 then getSpaceOfState_h sRest pRest sum
           else let spOfOp = sps!!(oid-1)
                in getSpaceOfState_h sRest pRest (sum+spOfOp)
       getSpaceOfState_h [] [] sum = sum

--get the state of a heap element
getState :: HeapElement -> State
getState (HeapE _ _ state _) = state

--clear the path of a heap element
clearPath :: HeapElement -> HeapElement
clearPath (HeapE msp sp state path) = (HeapE msp sp state [])

--get the peak space consumption of a heap element
getMSpace :: HeapElement -> Int
getMSpace (HeapE mspace _ _ _) = mspace

--get the path of a heap element
getPath :: HeapElement -> [HeapElement]
getPath (HeapE _ _ _ path) = path

--print a list of anything that can be showed
print_list list = putStrLn $ toString_list list 
toString_list [] = ""
toString_list (x:rest) = (show x) ++ "\n\n" ++ toString_list rest
