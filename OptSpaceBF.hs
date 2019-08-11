module OptSpaceBF (best_first, print_best_first) where
import TopoSort --ony the data type Relation is needed, not the toposort
import Data.Heap (Heap)
import qualified Data.Heap as Heap
import Debug.Trace
import Data.List
data HeapElement = HeapE MSpace Space State [HeapElement]
        deriving (Show, Eq, Ord)
--data Relation = Rel PId OId [PId]

type MSpace = Int
type Space = Int
type State = [Index]
type Index = Int
type PId = Int

--Use the same input form as the form for recursive approach so that it is easier to compare the results of different approaches.
type Input_Process = (PId, [Space])
type Input_Program = [Input_Process] --Important!! the processes in the list should be arranged in ascending order according to PId
 --for output
data Operation_out = OP PId OId Space deriving (Show, Eq, Ord)
type OId = Int
type Output_State = [Operation_out] --a state in a schedule
type Output_Schedule = [(Space, Output_State)]
------------------------------------------------------------------------------------------------------------------
--main functions
print_best_first :: Input_Program -> [Relation] -> IO()
print_best_first ps rs = print_list $ best_first ps rs

best_first :: Input_Program -> [Relation] -> Output_Schedule
best_first ps rs = let heap = [startElement ps rs]
                       endState' = endState ps
                       path = best_first_h heap ps rs endState' []
                   in toSchedule path ps
-------------------------------------------------------------------------------------------------------------------
--help-functions for best_first
{-
convert the returned path, which is a list of heap elements, into a schedule as final output.
-}
toSchedule :: [HeapElement] -> Input_Program -> Output_Schedule
toSchedule path ps = let spList = getSpaceList path
                         indexLists = transpose $ getStateList path
                         opLists = indexListsToOpLists [] indexLists ps
                         stateLists = transpose opLists
                     in addSpaceToStateList [] stateLists spList
                     where
                     getSpaceList path = map (\(HeapE msp sp _ _) -> sp) path 
                     getStateList path = map (\(HeapE _ _ state _) -> state) path
                     indexListsToOpLists opLists (pi:piRest) (p:pRest)
                        = let opList = map(\index -> if index == 0
                                                     then OP (fst p) index 0
                                                     else OP (fst p) index (snd p !! (index-1))
                                           ) pi
                          in indexListsToOpLists (opList:opLists) piRest pRest

                     indexListsToOpLists opLists [] [] = reverse opLists
                     
                     addSpaceToStateList schedule (state:sRest) (space:spRest)
                        = let stateOfSchedule = (space, state)
                          in addSpaceToStateList (stateOfSchedule:schedule) sRest spRest
                     addSpaceToStateList schedule [] [] = reverse schedule
{-
recursivly search for the min-element in the heap. In every round, check if the min-element has the final state.
If yes, return the path which is saved in second list of the min-element.
If not, generate all possibls kid-elements(which have a possible next state) of the min-element and insert them into the heap. Remove the min-element, sort the heap, and then go to the next round.
-}                 
best_first_h :: [HeapElement] -> Input_Program -> [Relation]-> State -> [State] -> [HeapElement]
best_first_h heap@(minElement:rest) ps rs endState vStates
 = --trace ("Heap: " ++ show heap) $ if getState minElement == endState
          if getState minElement == endState
          then reverse $ (clearPath minElement):(getPath minElement)
          else let next_elements = toElements (nextStates (getState minElement) endState heap rs vStates) minElement ps
                   heap' = Heap.sort (next_elements ++ rest)
               in --trace (show minElement ++ "\n") 
                        best_first_h heap' ps rs endState ((getState minElement):vStates)
best_first_h [] _ _ _ _ = [] --should not happen

{-generate a start-element for the best-first tree
help-function startElement_h: generate the start-state by inserting 1 or 0 as index into the startState. If it is a subprocess, the index should be 0, else 1
-}
startElement :: Input_Program -> [Relation] -> HeapElement
startElement ps rs = let startState = startElement_h [] ps rs
                         space = getSpaceOfState startState ps
                         mspace = space
                     in HeapE mspace space startState []
                     where
                     
                     startElement_h startState ((pid, spaces):ps) rs
                        = if isSubprocess pid then startElement_h (1:startState) ps rs
                          else startElement_h (0:startState) ps rs
                          where isSubprocess x = not $ any (\(Rel _ _ subPs) -> elem x subPs) rs
                                
                     startElement_h startState [] _ = reverse startState

test1 = startElement p5 r5

--generate the end-state, which contains the last operation-index of every process
endState :: Input_Program -> State
endState ps = map (\(pid,sps) -> length sps) ps

test2 = endState p5
--generate all possible next states of a state
nextStates :: State -> State -> [HeapElement] -> [Relation] -> [State] -> [State]
nextStates state endState heap relations vStates
        = let nextStates_temp = generate_next state
          in filter (\state' -> check_state_rs state' relations && --not violate the restrictions
                               (not $ elem state' $ map getState heap) && --not already in heap
                               (not $ any (\[oid',oid] -> oid' > oid) $ transpose [state', endState]) && --no index exceed
                               (not $ elem state' vStates) &&
                               state' /= state --not the original state
                    ) nextStates_temp
          where
          generate_next (oid:rest) = [a:b | a <- [oid,oid+1], b <- generate_next rest]
          generate_next [] = [[]]
          --check if a state is valid based on a list of relations        
          check_state_rs state' rList = all (\rel -> check_state state' rel) rList
            --check if a state is valid based on a relation
          check_state state' (Rel pid oid subPlist)
                = let index_main_o = state !! (pid-1) --index of op of the main process in the original state
                      index_main_n = state' !! (pid-1) --index of op of the main process in a new state
                      index_main_l = endState !! (pid-1) --last index of main p
                  in (all (\spid -> (index_main_n <= oid && state' !! (spid-1) == 0) ||
                                    (index_main_n > oid && state' !! (spid-1) > 0)
                           ) subPlist
                      ) ||
                      --special case, when the op which generates subprosess is the last op
                     (index_main_n == oid && index_main_o == oid &&
                      index_main_n == index_main_l &&
                      (all (\spid -> state' !! (spid-1) > 0) subPlist)
                     )
                                    

--test3 = nextStates (getState test1) test2 [] r5

--convert all next states to heap-elements
toElements :: [State] -> HeapElement -> Input_Program -> [HeapElement]
toElements states element ps =
  let mspace_before = getMSpace element --selected min-element
      path = getPath element --path of min-element
  in map (\state -> let space = getSpaceOfState state ps--sum of space of a state
                    in HeapE (max mspace_before space) space state (clearPath element:path)
          ) states

--test4 = toElements test3 test1 p5



-------------------------------------------------------------------------------------------------------------------
--basic functions

--calculate the sum of space of a state
getSpaceOfState :: State -> Input_Program -> Int
getSpaceOfState state ps = getSpaceOfState_h state ps 0
                           where
                           getSpaceOfState_h (oid:sRest) ((pid, sps):pRest) sum
                             = if oid == 0 then getSpaceOfState_h sRest pRest sum
                               else let spOfOp = sps!!(oid-1)
                                    in getSpaceOfState_h sRest pRest (sum+spOfOp)
                           getSpaceOfState_h [] [] sum = sum

--HeapElement = HeapE MSpace Space State [HeapElement]
getState :: HeapElement -> State
getState (HeapE _ _ state _) = state

clearPath :: HeapElement -> HeapElement
clearPath (HeapE msp sp state path) = (HeapE msp sp state [])

getMSpace :: HeapElement -> Int
getMSpace (HeapE mspace _ _ _) = mspace

getPath :: HeapElement -> [HeapElement]
getPath (HeapE _ _ _ path) = path

--print a list of anything that can be showed
print_list list = putStrLn $ toString_list list 
toString_list [] = ""
toString_list (x:rest) = (show x) ++ "\n\n" ++ toString_list rest
-------------------------------------------------------------------------------------------------------------------
p5 = [(1,[2,8,4,16,1]),(2,[5,1,2]),(3,[8,2,2,1]),(4,[5,1,9,2,1]),(5,[8,8,1]),(6,[3,1,3])]
r5 =[(Rel 1 2 [2,3]),(Rel 4 1 [5]),(Rel 2 2 [6])]
test_p5 = print_best_first p5 r5

p6 = [(1,[4,3,5,6]),(2,[8,1,2]),(3,[4,1,2,4]),(4,[2,4,1])]
r6 = [(Rel 1 1 [2]),(Rel 3 1 [4])]
test_p6 = print_best_first p6 r6

p7 = [(1, [2,3,1,4,2]), (2,[2,6,1,7,2,4,3]), (3,[3,4,2,7,1,6,2])]
r7 = []
test_p7 = print_best_first p7 r7

p8 = [(1,[2,3,1,4,2,4,1,3,2]),(2,[2,6,1,7,1,6,2]),(3,[3,4,2,7,2,4,3])]
test_p8 = print_best_first p8 r7

p9 = [(1,[2,5,10,3]),(2,[5,10]),(3,[10,3,8,4,2]),(4,[3,8,4,2,4,5,1,6,5,4]),(5,[8,4,2])]
r9 = [Rel 3 2 [2],Rel 4 5 [3],Rel 5 1 [4],Rel 5 2 [1]]
test_p9 = print_best_first p9 r9
