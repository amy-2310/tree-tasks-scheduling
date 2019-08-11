module OptSpaceRecursive (spopt_schedule, print_spopt_schedule, spoptnr, toProgram, print_list) where
import TopoSort
import Data.List
import Data.Maybe
import Data.Heap (Heap)
import qualified Data.Heap as Heap
import Debug.Trace

data Operation = Op PId OId Space [Operation] [SubProcess] [Operation] [Operation]
        deriving (Show,Eq, Ord)
{-test
The first list contains the Operations happening at the same time in an optimal schedule (a state in the schedule)
The second list contains the Sub-processes generated by the operation
The third list contains the operations which were in front of the op, were removed durng standardizing, and to which the op is a peak in a pattern.These operations will later be inserted back to the opimal schedule.
The forth list contains the ops which were in the back of the op (analog to the third list)
-}

--data type for output
data Operation_out = OP PId OId Space deriving (Show, Eq, Ord)

type SubProcess = Process
type PId = Int
type OId = Int
type Space = Int
type Program = [Process]
type Process = [Operation]
type Input_Process = (PId, [Space])
type Input_Program = [Input_Process]
type Output_State = [Operation_out] --a state in a schedule
type Output_Schedule = [(Space, Output_State)]
---------------------------------------------------------------------------------------------------------------
{-
convert a list of inputs into a program. The sub-processes will be inserted into the corresponding main-process according to the relations (data Relation = Rel PId OId [PId]) -> The operation with PId OId generates the processes with PId 
-}
toProgram :: Input_Program -> [Relation] -> Program 
toProgram ls rs = Heap.sort $ toProgram_h (map toProcess ls) (topo_sort rs)
toProgram_h ls ((Rel pid oid ps):r_rest)
        = let main_p = fromJust $ find (\p -> (get_pid $ head p) == pid) ls
              sub_ps = filter (\p -> elem (get_pid $ head p) ps) ls
              other_ps = filter (\p -> (not $ elem (get_pid $ head p) ps) &&
                                       (not $ (get_pid $ head p) == pid)
                                ) ls
              main_p_new = map (\o -> if get_oid o == oid
                                      then add_subps o sub_ps
                                      else o) main_p
              ls_new = main_p_new:other_ps
          in toProgram_h ls_new r_rest
toProgram_h ls [] = ls

--convert an Input-Process into a Process
toProcess :: Input_Process -> Process
toProcess (pid,ls) = toProcess_h ls pid 1
toProcess_h (x:xs) pid oid = (Op pid oid x [] [] [] []):toProcess_h xs pid (oid+1)
toProcess_h [] _ _ = []
--------------------------------------------------------------------------
--final functions
spopt_schedule :: Input_Program -> [Relation] -> Output_Schedule
spopt_schedule inputProgram inputRelation = recover $ spoptnr $ toProgram inputProgram inputRelation

print_spopt_schedule :: Input_Program -> [Relation] -> IO ()
print_spopt_schedule inputProgram inputRelation = print_list $ recover $ spoptnr $ toProgram inputProgram inputRelation
------------------------------------------------------------------------------------------------------------------
--recover schedule
{-
convert a merged process (the result of spoptnr) back into a complete schedule with space consumption at each step
-}
recover :: Process -> Output_Schedule
recover procs = recover_h procs []

{-
recover_h:: Process -> Output_Schedule -> Output_Schedule
In a merged process, every op is actually a state in an optimal schedule. This state is saved in the first list of the op. For every state, there might be operations needed to be recovered and inserted before or after the state. These operations are saved in the third or in the forth list respectively. The function goes through the states RECURSIVELY. In every round, it checks RECURSIVELY if there are operations needed to be recover between two neighboring states. A new state to be inserted is calculated by substituting an op in the first state with the op which is to be recovered. These two ops should have the same pid but different oid. The new state will then be the first state for the next round and so on.
-}
--case that there is at least one operation after op1 to recover
recover_h (op1@(Op _ _ _ ops1 _ _ after1@(a:as)):op2:rest) schedule
        = let --take ops out of a if a is a merged op
              ops_of_a = if null (get_ops a) then [a] else get_ops a
              
              --calculate the state to be recovered based on the state of op1 (ops1) and its memorized operation(s) (ops_of_a) which should happen after op1
              state_recovered = sort ((filter (\x -> not $ elem (get_pid x) (map get_pid ops_of_a)) ops1)
                                       ++ ops_of_a
                                      )
              --op1 for the next round, which has the newly recovered state and the remaining memorized operations. The pid oid and space is not relevant so just give them a dummy value 0.
              op1' = Op 0 0 0 state_recovered [] [] as
              
              --transform the state of op1 into a state of schedule for output and insert it into the schedule-list. Such state is a 2-tupel: (space cunsumption of the state, [ops of the state])
              state = ((foldl (\x y -> (get_space y) + x) 0 ops1), sort $ map op_to_op_out ops1)
          in recover_h (op1':op2:rest) (state:schedule)

--case that there is no operation after op1, but there is at least one operation before op2 to recover
recover_h (op1@(Op _ _ _ ops1 _ _ []):op2@(Op _ _ _ ops2 _ before2@(b:bs) after2):rest) schedule
        = let --take ops out of b if b is a merged op
              ops_of_b = if null (get_ops b) then [b] else get_ops b
              
              --calculate the state to be recovered based on the state of op1 (ops1) and the memorized operation(s) (ops_of_b) which should happen befort op2
              state_recovered = sort ((filter (\x -> not $ elem (get_pid x) (map get_pid ops_of_b)) ops1)
                                       ++ ops_of_b
                                      )
              --op1 for the next round, which has the newly recovered state
              op1' = Op 0 0 0 state_recovered [] [] []
              
              --op2 for the next round, which has the original state and the remaining memorized operations.
              op2' = Op 0 0 0 ops2 [] bs after2
              
              state = (foldl (\x y -> get_space y + x) 0 ops1, sort $ map op_to_op_out ops1)
          in recover_h (op1':op2':rest) (state:schedule)

--case that there is no operation after op1 and nor before op2 to recover
recover_h (op1@(Op _ _ _ ops1 _ _ []):op2@(Op _ _ _ _ _ [] _):rest) schedule
        = let state = (foldl (\x y -> get_space y + x) 0 ops1, sort $ map op_to_op_out ops1)
          --all states between op1 and op2 are recovered. check operations between op2 and op3 in the next round.
          in recover_h (op2:rest) (state:schedule)

--case that there is only one op in the process: this op should not have any after-operation and should be the last state of the schedule. Transform the state into a state for output and insert it to the schedule. Done.
recover_h (op@(Op _ _ _ ops1 _ _ _):[]) schedule
        = let state = (foldl (\x y -> get_space y + x) 0 ops1, sort $ map op_to_op_out ops1)
          in reverse (state:schedule)
-------------------------------------------------------------------------------------------------------------------
--SPOPTN-recursive
{-
Firstly, by optimizing n tree-like processes, we get n linear merged processes. (map f procs)
Then, by optimizing the n linear merged processes, we get one merged process. (spoptn_trans)
-}
spoptnr :: [Process] -> Process 
spoptnr procs = spoptn_trans $ map f procs

{-
Calculate the schedule with minimal space consumption of one tree-like process and transform
the schedule into a single process.
The function checks every operation recursively: if an operation generates no sub-processes,
it will not be changed because there is nothing to calculate. If an operation has sub-processes,
the remaining process and the sub-processes will be optimized and transformed into a single process.
Because these processes can also be tree-like, the function has to be applied again on each of them.
-}
f :: Process -> Process
f [] = []
f (op@(Op pid oid sp ops ps front back):rest)
  = --trace (show rest) $
          if null ps then op:f rest --the op does not generate any sub-processes
          else --the op generate at least one sub-process
            if null rest --if the op is the last one of the main-process
            then (Op pid oid sp ops [] front back): (spoptn_trans $ map f ([(Op pid oid sp ops [] front back)]:ps))--the op must runs till the end. The sub-process-list must be emptied so that the break condition can be met.
            else (Op pid oid sp ops [] front back): (spoptn_trans $ map f (rest:ps))
----------------------------------------------------------------------------------------------------------------            
--optimize parallel processes and transform the opimal schedule into a single process
spoptn_trans :: [Process] -> Process
spoptn_trans procs = transform $ snd $ spoptn procs
{-
Transform a schedule into a process. A state in the Schedule is transformed into an operation.
All operations in a state are packed into the first list of the new operation.
The space consumption of the new operation is the sum of space consumption of all operations in the state.
All front-removed and back-removed ops of all ops in a state are pack into the third and forth list of the new op.
The pid and oid of the first operation in the state are used as the pid and oid of the new operation.
-}
--transform a schedule into a process
transform :: [[Operation]] -> Process
transform [] = []
transform (state:rest) = transform_op state : transform rest

--transform a state into a operation
transform_op :: [Operation] -> Operation
transform_op state@((Op pid oid sp ops ps front back):rest)
        = let sp_new = foldl (\x y -> x + (get_space y)) 0 state --add up all the space usages
              ops_new = foldr (\x y -> if null (get_ops x) --if op does not have ops happening at the same time
                                       then (clear_ops_fb x):y
                                       else (map clear_ops_fb $ get_ops x) ++ y) [] state
              front_new = foldr (\x y -> (get_ops_f x) ++ y) [] state
              back_new = foldr (\x y -> (get_ops_b x) ++ y) [] state
          in Op pid oid sp_new ops_new ps front_new back_new
                                                
-------------------------------------------------------------------------------------------------------------------
--SPOPTN Alg: calculate the minimal space consumption and its corresponding schedule of parallel processes

spoptn :: [Process] -> (Int, [[Operation]])
spoptn ps = let --standardize the processes and take elements out of the 7-er tupel
                result_std = standardize ps 
                procs_std = fst_7 result_std --stardardized processes
                m_start = snd_7 result_std --sum of space of all start ops
                m_end = forth_7 result_std --sum of space of all end ops
                m_one = sixth_7 result_std --sum of space of all ops in one-op-processes
                ops_one = last_7 result_std --all ops in one-op-processes, they were removed
                starts = third_7 result_std --all start-ops
                ends = fifth_7 result_std --all end-ops
                valleys = searchValleys procs_std
                --scan the processes
                result_scan_lr = scan procs_std valleys --A Tupel(space, schedule), which is the result of left to right scan.
                result_scan_rl = scan (map reverse procs_std) valleys
                
                spmin = maximum [fst result_scan_lr + m_one, fst result_scan_rl + m_one, m_start, m_end]
                schedule = snd result_scan_lr ++ drop 1 (reverse (snd result_scan_rl))
                
                --recover operations of one-element processes
                schedule' = map (++ ops_one) schedule
                --if the first state of the schedule is different from the list of all start ops, it means some start peaks has been remove. In that case, recover it.
                schedule''= if (sort starts) == (sort $ head schedule') then schedule'
                            else starts:schedule'
                --same for the last state of schedule
                schedule''' = if (sort ends) == (sort $ last schedule'') then schedule''
                              else schedule'' ++ [ends]
             in (spmin, schedule''')


--SPOPTN Alg - standardize
standardize :: [Process] -> ([Process],Int,[Operation],Int,[Operation],Int,[Operation])
{-
Remove patterns M1-M4, start-peaks and end-peaks in all Processes, remove all one-element processes
Output: (standardized Processes in a list, sum of all start ops, all start ops,
,sum of all end ops, all end ops,
sum of all ops of one-op-processes, these ops in a list)
-}
standardize procs = standardize_h procs [] 0 [] 0 [] 0 []

--break condition
standardize_h [] proc_std m_start state_s m_end state_e m_one ops_one
        = (reverse proc_std, m_start, reverse state_s, m_end, reverse state_e, m_one, reverse ops_one)

standardize_h (proc:rest) procs_std m_start state_s m_end state_e m_one ops_one
 |length_proc' == 1
    = standardize_h rest procs_std (m_start + start_sp) (start:state_s)
                                   (m_end + end_sp) (end:state_e)
                                   (m_one + one_sp) (start:ops_one)
 |length_proc' == 2
    = --only two ops left and the first one is the peak
      if start_sp >= end_sp
      then standardize_h rest procs_std (m_start + start_sp) (start:state_s)
                                         (m_end + end_sp) (end:state_e)
                                         (m_one + one_sp) (end:ops_one)
      --only two ops left and the second one is the peak
      else standardize_h rest procs_std (m_start + start_sp) (start:state_s)
                                         (m_end + end_sp) (end:state_e)
                                         (m_one + one_sp) (start:ops_one)
 |otherwise --more than three ops left
    = --both start and end peaks exist
      if start_sp >= sec_sp && end_sp >= vor_end_sp
      then if length proc' == 3
           then standardize_h rest procs_std
                               (m_start + start_sp) (start:state_s)
                               (m_end + end_sp) (end:state_e)
                               (m_one + one_sp) (sec:ops_one)
           else
                               standardize_h rest --add the proc without start and end peaks to the result list
                               ((drop 1 $ init proc'):procs_std)
                               (m_start + start_sp) (start:state_s)
                               (m_end + end_sp) (end:state_e)
                               (m_one + one_sp) (ops_one)
      else
        if start_sp >= sec_sp --only start peak exists
        then standardize_h rest ((drop 1 proc'):procs_std) --add the proc without start peak to the result list
                                 (m_start + start_sp) (start:state_s)
                                 (m_end + end_sp) (end:state_e)
                                 (m_one + one_sp) (ops_one)
        else
          if end_sp >= vor_end_sp --only end peak exists
          then standardize_h rest ((init proc'):procs_std) --add the proc without end peak to the result list
                                   (m_start + start_sp) (start:state_s)
                                   (m_end + end_sp) (end:state_e)
                                   (m_one + one_sp) (ops_one)
          --no start or end peaks
          else standardize_h rest (proc':procs_std)
                                   (m_start + start_sp) (start:state_s)
                                   (m_end + end_sp) (end:state_e)
                                   (m_one + one_sp) (ops_one)
   where
        proc' = removeMs proc
        start = head proc'
        start_sp = get_space start
        sec = head $ drop 1 proc'
        sec_sp = get_space sec
        end = last proc'
        end_sp = get_space end
        vor_end = last $ init proc'
        vor_end_sp = get_space vor_end
        length_proc' = length proc'
        one_sp | length_proc' == 1 = start_sp
               | length_proc' == 2 = min start_sp end_sp
               | otherwise = 0
          

--remove patterns M1-M4 in a Process, time = O(number of operations of a Process)
removeMs :: Process -> Process
removeMs = removeMs_h []
removeMs_h p_new p_old@(oi:oi1:oi2:oi3:rest) -- note that p_new is saved in reverse order
        | matchM1 oi oi1 oi2 = removeMs_h p_new' p_old_matchM1
        | matchM2 oi oi1 oi2 = removeMs_h p_new' p_old_matchM2
        | matchM3 oi oi1 oi2 oi3 = removeMs_h p_new' p_old_matchM3
        | matchM4 oi oi1 oi2 oi3 = removeMs_h p_new' p_old_matchM4
        | otherwise = removeMs_h (oi:p_new) (oi1:oi2:oi3:rest) 
         where
         p_new' = drop 2 p_new --in paper go back 3, but 2 should be enough?
         ops_return = reverse $ take 2 p_new
         
         p_old_matchM1 = let ops_toAdd = get_ops_f oi1 ++ [clear_ops_fb oi1] ++ get_ops_b oi1
                             --let oi memorize oi1 and its neiboring ops which are removed
                             oi' = add_ops_b oi ops_toAdd
                         in ops_return ++ (oi':oi2:oi3:rest)
         
         p_old_matchM2 = let ops_toAdd = get_ops_f oi1 ++ [clear_ops_fb oi1] ++ get_ops_b oi1
                             oi2' = add_ops_f oi2 ops_toAdd
                         in ops_return ++ (oi:oi2':oi3:rest)
         
         p_old_matchM3 = let ops_toAdd = get_ops_f oi1 ++ [clear_ops_fb oi1] ++ get_ops_b oi1 ++
                                         get_ops_f oi2 ++ [clear_ops_fb oi2]++ get_ops_b oi2
                             oi' = add_ops_b oi ops_toAdd
                         in ops_return ++ (oi':oi3:rest)
                         
         p_old_matchM4 = let ops_toAdd = get_ops_f oi1 ++ [clear_ops_fb oi1] ++ get_ops_b oi1 ++
                                         get_ops_f oi2 ++ [clear_ops_fb oi2]++ get_ops_b oi2
                             oi3' = add_ops_f oi3 ops_toAdd
                         in ops_return ++ (oi:oi3':rest)
         
removeMs_h p_new p_old@[oi,oi1,oi2]
        | matchM1 oi oi1 oi2 = removeMs_h p_new' p_old_matchM1
        | matchM2 oi oi1 oi2 = removeMs_h p_new' p_old_matchM2
        | otherwise = removeMs_h (oi:p_new) [oi1,oi2]
        where
         p_new' = (drop 2 p_new)
         ops_return = reverse $ take 2 p_new
         p_old_matchM1 = let ops_toAdd = (get_ops_f oi1 ++[clear_ops_fb oi1]++ get_ops_b oi1)
                             oi' = add_ops_b oi ops_toAdd
                         in ops_return ++ [oi',oi2]
         p_old_matchM2 = let ops_toAdd = (get_ops_f oi1 ++[clear_ops_fb oi1]++ get_ops_b oi1)
                             oi2' = add_ops_f oi2 ops_toAdd
                         in ops_return ++ [oi,oi2']

removeMs_h p_new p_old
        = (reverse p_new) ++ p_old

matchM1 (Op _ _ sp1 _ _ _ _) (Op _ _ sp2 _ _ _ _)(Op _ _ sp3 _ _ _ _)
        = (sp1 >= sp2 && sp2 >= sp3)
        
matchM2 (Op _ _ sp1 _ _ _ _) (Op _ _ sp2 _ _ _ _)(Op _ _ sp3 _ _ _ _)
        = (sp1 <= sp2 && sp2 <= sp3)

matchM3 (Op _ _ sp1 _ _ _ _) (Op _ _ sp2 _ _ _ _)(Op _ _ sp3 _ _ _ _) (Op _ _ sp4 _ _ _ _)
        = (sp1 > sp2 && sp2 < sp3 && sp3 > sp4 && sp1 >= sp3 && sp2 >= sp4)

matchM4 (Op _ _ sp1 _ _ _ _) (Op _ _ sp2 _ _ _ _)(Op _ _ sp3 _ _ _ _) (Op _ _ sp4 _ _ _ _)
        = (sp1 < sp2 && sp2 > sp3 && sp3 < sp4 && sp1 <= sp3 && sp2 <= sp4)

--SPOPTN Alg --scan
scan:: [Process] -> [Operation]-> (Int, [[Operation]])  --(spmin, schedule)
scan procs valleys = let heap = toHeap procs
                         s =  foldl (\x y -> x + (get_space $ head y)) 0 procs
                         m = s
                         first_state = map head procs
                     in scan_h heap procs valleys s m [first_state]

scan_h :: [(Int,Int)] -> [Process] -> [Operation] -> Int -> Int -> [[Operation]] -> (Int, [[Operation]])
--arguments: heap, processes, valleys, whether all valleys are passed, space, max. space, schedule
--output: (max. Space, schedule)

{-
For the case that all valleys are already in schedule (Heap becomes empty for the first time),
if there are still unhandled processes in the list, start a new heap-sort to complete the schedule 
-}
{-scan_h [] procs valleys False s m schedule
        = let procs' = map (\x -> if length x == 1 then replicate 3 (head x) else x) procs
                            -- littel trick for keeping the index of processes unchanged
              heap = toHeap procs'
          in if all (\x -> length x == 1) procs --no more unhandled proccesses
             then scan_h [] procs valleys True s m schedule
             else scan_h heap procs' valleys True s m schedule
-}
--The heap became empty. The schedule is complete.
scan_h [] procs valleys s m schedule = (m, reverse schedule)


scan_h (h:rest) procs valleys s m schedule
        = let -- h ist the first element of min-heap, and is a tupel consisting of the next incline of a process and the index of the process
              index_h = snd h
              dist_h = fst h
              
              
              --The process whose next peak has the fewest incline
              selected_proc = procs!!index_h
              
              --space usage and max sapce usage for the next recursion
              s_new = if (not isGlobalValley)
                      then s + (get_space (selected_proc !! 2) - get_space (selected_proc !! 0))
                      else s
              m_new = if (not isGlobalValley)
                      then max m (s + dist_h)
                      else m
              
              --list of processes for the next recursion
              procs' = map (\x -> if ((procs !! index_h) == x) &&
                                     (not isGlobalValley)
                                  then drop 2 x else x) procs
              
              --The first and second elements of the seleced process
              selected_ops = [selected_proc !! 2,selected_proc !! 1]
              isGlobalValley = valleys !! index_h `elem` [selected_proc !! 0,selected_proc !! 1]
              --The first element of other processes.
              ops_not_changed = [map head $ filter (\x -> procs!!index_h /= x) procs]
              
              --generate two states for schedule
              states = [Heap.sort(p:ps) | p <- selected_ops, ps <- ops_not_changed]
              
              schedule_new = if not isGlobalValley
                             then states ++ schedule
                             else schedule
              heap_new = if (length selected_proc >= 4) && (not isGlobalValley)
                         then let elem_new = ((get_space(selected_proc!!3)- get_space(selected_proc!!2)), index_h)
                              in Heap.sort $ elem_new:rest
                         else rest
              {-schedule_new = if head states == last states --just to remove duplicate states
                             then if head schedule == head states
                                  then schedule
                                  else (head schedule):schedule
                             else states ++ schedule
              -}
          in scan_h heap_new procs' valleys s_new m_new schedule_new

searchValleys :: [Process] -> [Operation]
searchValleys ps = map searchValley ps 

searchValley :: Process -> Operation
searchValley (o:rest) = searchValley_h rest o
searchValley_h [] valley = valley
searchValley_h (o:rest) valley = if get_space o <= get_space valley then searchValley_h rest o else searchValley_h rest valley             
              
--heap-sort the defferences of 2nd and 1st element of all processes 
toHeap :: [Process] -> [(Int,Int)]
toHeap = Heap.sort . toHeap_h 0
toHeap_h _ [] = []
toHeap_h index (p:rest)= ((get_space (p!!1) - get_space (p!!0)),index):toHeap_h (index+1) rest
------------------------------------------------------------------------------------------------------------------
--some basic functions

--functions for data type Operation
get_pid (Op pid _ _ _ _ _ _) = pid
get_oid (Op _ oid _ _ _ _ _) = oid
get_space (Op _ _ sp _ _ _ _) = sp
get_ops (Op _ _ _ ops _ _ _) = ops
get_subps (Op _ _ _ _ subps _ _) = subps
get_ops_f (Op _ _ _ _ _ ops_f _) = ops_f
get_ops_b (Op _ _ _ _ _ _ ops_b) = ops_b
add_subps (Op pid oid sp ops subps front back) subps' = (Op pid oid sp ops (subps ++ subps') front back)
add_ops_f (Op pid oid sp ops ps ops_f ops_b) ops_f_toAdd = (Op pid oid sp ops ps (ops_f_toAdd ++ ops_f) ops_b)
add_ops_b (Op pid oid sp ops ps ops_f ops_b) ops_b_toAdd = (Op pid oid sp ops ps ops_f (ops_b ++ ops_b_toAdd))
clear_ops_fb (Op pid oid sp ops ps ops_f ops_b) = (Op pid oid sp ops ps [] [])

--get-functions for 7er-tupel
fst_7 (x,_,_,_,_,_,_) = x
snd_7 (_,x,_,_,_,_,_) = x
third_7 (_,_,x,_,_,_,_) = x
forth_7 (_,_,_,x,_,_,_) = x
fifth_7 (_,_,_,_,x,_,_) = x
sixth_7 (_,_,_,_,_,x,_) = x
last_7 (_,_,_,_,_,_,x) = x
--convert Operation to Operation_out
op_to_op_out (Op pid oid sp _ _ _ _) = OP pid oid sp

--print a list of anything that can be showed
print_list list = putStrLn $ toString_list list 
toString_list [] = ""
toString_list (x:rest) = (show x) ++ "\n\n" ++ toString_list rest
-----------------------------------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------                 
p5 = [(1,[2,8,4,16,1]),(2,[5,1,2]),(3,[8,2,2,1]),(4,[5,1,9,2,1]),(5,[8,8,1]),(6,[3,1,3])]
r5 = [(Rel 1 2 [2,3]),(Rel 4 1 [5]),(Rel 2 2 [6])]                        
test_p5 = print_spopt_schedule p5 r5

--subtree under op 2 2
p5_sub_22 = [(1,[2]),(6,[3,1,3])]
test_p5_sub_22 = print_spopt_schedule p5_sub_22 []
--optimum = 5, space = [5,3,5]

--subtree under op 1 2
p5_sub_12 = [(2,[5,1,5,3,5]), (1,[4,16,1]), (3,[8,2,2,1])]
test_p5_sub_12 = print_spopt_schedule p5_sub_12 []
--opt = 18, space = [17,11,11,6,18,3,7,5,7]

--subtree under op 4 1
p5_sub_41 =  [(4,[1,9,2,1]),(5,[8,8,1])]
test_p5_sub_41 = print_spopt_schedule p5_sub_41 []
--opt = 10, space = [9,9,2,10,3,2]

--optimize two main processes p1 and p4
p5_mp_14 = [(1,[2,8,17,11,11,6,18,3,7,5,7]),(4,[5,9,9,2,10,3,2])]
test_p5_mp_14 = print_spopt_schedule p5_mp_14 []
--opt = 20
------------------------------------------------------------------------------------------------
p6_s1 = [(1,[2,6,5,2,4]),(2,[3,1])]
test_p6_s1 = print_spopt_schedule p6_s1 []

p6 = [(1,[2,5,4,1]),(2,[1,3]),(3,[3,1])]
r6 = [Rel 1 1 [2]]
test_p6 = print_spopt_schedule p6 r6
-----------------------------------------------
--TODO: debug!
p:: [(Int, [Int])]
p = [(1,[1,3,2,3]),(2,[3]),(3,[2,3,1]),(4,[3,1]),(5,[1,5,4])]
r = [Rel 4 1 [3],Rel 5 1 [2],Rel 5 2 [1,4]]

p1::[(Int,[Int])]
p1 = [(1,[1,2]),(2,[3,4]),(3,[5,6,5])]
r1 = [Rel 1 1 [2],Rel 2 1 [3]]    

p1_t1::[(Int,[Int])]
p1_t1 = [(2,[4]),(3,[5,6,5])]