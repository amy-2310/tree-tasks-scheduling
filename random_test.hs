import OptSpaceRecursive
import OptSpaceBF
import TopoSort --ony the data type Relation is needed, not the toposort
import Data.List
import System.Random
--------------------------------------------------------------------------------
{-The two main functions, test and print_test, randomly generate trees
and compare the optimal-space consumption of the schedule delivered by spopt_schedule
and best_first: True if the spaces are the same, otherwise false.

Note that a tree here is not a recursive data type but rather just a list of processes
and a list of relations.

Input: num. of trees, num. of processes, max length of a process, max.
space of an operation, an integer seed for random-generator

Output is a tupel containing 2 lists: a list of trees and a list of results
([(Id of tree, tree, relations)], [(Id of tree, result of comparison)])
-}
test::Int -> Int -> Int -> Int -> Int
      -> ([(Int, [Input_Process], [Relation])], [(Int, Bool)])

test nT nP maxL maxS seed =
    test_h nT nP maxL maxS (mkStdGen seed) 1 [] []
    where
      test_h :: Int -> Int -> Int -> Int -> StdGen -> Int
                -> [(Int, [Input_Process], [Relation])]-> [(Int, Bool)]
                -> ([(Int, [Input_Process], [Relation])], [(Int, Bool)])

      test_h nT nP maxL maxS g counter trees results
        = if counter <= nT --number of tested trees has not met nT
          then let --generate a random tree and a new random generator
                   (p,r,g') = generateTree nP maxL maxS g
                   
                   --opt. space of the tree, calculated by spopt_schedule
                   maxSpace_r = fst $ last $ sort $ spopt_schedule p r
                   
                   --opt. space of the tree, calculated by best_first
                   maxSpace_b = fst $ last $ sort $ best_first p r
               
               --pack the tree into the tree-list,
               --pack the result of comparison of spaces into the result-list,
               --increase the counter by one and go to the next round.
               in  test_h nT nP maxL maxS g' (counter+1) --new random generator is used
                          ((counter, p,r):trees)
                          ((counter, maxSpace_r == maxSpace_b):results)
          else (reverse trees, reverse results)

--print the trees and the results in ghci
print_test :: Int -> Int -> Int -> Int -> Int -> IO()
print_test nT nP maxL maxS seed = 
        let (trees, results) = test nT nP maxL maxS seed
        in do print_list trees
              print_list results
--------------------------------------------------------------------------------
--help-functions:

{-
generateP generates a list of random integers.
Argument: maximal length of a list, max. value of integers, a random number
generator.
Output: (a list of random integers, a new generator)
-}
generateP :: Int -> Int -> StdGen -> ([Int], StdGen)
generateP maxLength maxSp generater =
    let --get a random length and a new generator
        (lengthP, g2) = randomR (1,maxLength) generater
        --get another new generator
        g3 = snd $ next g2
    --Take lengthP elements from the infinite list of random integers generated 
    --from g2. Pack the list and the new generator g3 in a Tupel and return.
    in (take lengthP $ randomRs (1,maxSp) g2, g3)

{-
generatePs generates a list of random processes.
Argument: number of processes, max. length of any process, max. space of any
process, a random number generator.
Output: (a list of random processes, a new generator)
-}
generatePs :: Int -> Int -> Int -> StdGen -> ([Input_Process], StdGen)
generatePs nOfPs maxL maxSp g =
    generatePs_h nOfPs maxL maxSp g [] 1
    where
     --generate processes recursively
     generatePs_h :: Int -> Int -> Int -> StdGen -> [Input_Process] -> Int
                     -> ([Input_Process], StdGen)
     
     generatePs_h nOfPs maxL maxSp g psList counter =
        if counter <= nOfPs --number of processes has not been met
        then --generate a random process and a new generator
             let (spList, g2) = generateP maxL maxSp g
             
             --pack the process in the process-list, increase counter by 1,
             --go to next round
             in generatePs_h nOfPs maxL maxSp g2 ((counter,spList):psList)
                             (counter+1)
        --enough processes generated. Return the process-list and the generator.
        else (reverse psList, g)

{-
generateRs generates a list of random relations for a list of processes.
Arguments: a list of processes, a generator
Output: (a list of relations, a new generator)
-}
generateRs :: [Input_Process] -> StdGen -> ([Relation],StdGen)
generateRs psList g = generateRs_h psList g [] 1

{-help-function for generateRs which goes through all processes recursively.
In each round, it randomly decides if a process i is a sub-process or not.
If that is the case, a process having higher pid and one of its op are randomly
selected to be the process and operation which generate i.
-}
generateRs_h :: [Input_Process] -> StdGen -> [Relation] -> Int
                -> ([Relation],StdGen)

generateRs_h psList g rsList counter
   = let --number of processes
         length_psList = length psList
         
         --a help-function which calculates the length of the process having pid
         length_p pid =
            length $ snd $ head $ filter (\(pid',list) -> pid' == pid) psList
     
     in if counter < length_psList --it has not gone through all processes
        then let 
               --for process i, randomly select a process from i+1 to
               --n to be the process which generates process i.
               (pid_main, g2) = randomR (counter+1, length_psList) g
               
               --randomly select an operations from the selected process to be
               --the operation which generates process i.
               (oid_main, g3) = randomR (1, (max 1 $ (length_p pid_main))) g2
                
                --define a relation
               r = Rel pid_main oid_main [counter]
             
             in --process i has 50% chance to be a sub-process
               if fst (randomR (True,False) g) == True
                
                --pack the defined relation into the relation-list, go to next round.
               then generateRs_h psList g3 (r:rsList) (counter+1)
                
                --do not pack the difined relation, go to next round
               else generateRs_h psList g3 (rsList) (counter+1)
        else --it has gone through all processes.
             let rsList' = sort rsList
                 
                 --Help-function: merge the relations having the same pid and oid.
                 --The pid of their sub-processes are packed into the list of
                 --the merged relation.
                 mergeSubPList ((r1@(Rel pid1 oid1 ls1)):(r2@(Rel pid2 oid2 ls2)):rest)
                               mergedList
                    = if pid1 == pid2 && oid1 == oid2
                      then mergeSubPList ((Rel pid1 oid1 (ls1 ++ ls2)):rest)
                                         mergedList
                      else mergeSubPList  (r2:rest) (r1:mergedList)
                 mergeSubPList (r:[]) mergedList = mergeSubPList [] (r:mergedList)
                 mergeSubPList [] mergedList = (reverse mergedList, g)
             in mergeSubPList rsList' []

{-generate a tree and a new generater.
-}
generateTree :: Int -> Int -> Int -> StdGen
                   -> ([Input_Process], [Relation], StdGen)
generateTree nOfPs maxL maxSp g
        = let (psList, g') = generatePs nOfPs maxL maxSp g
              (rsList, g'') = generateRs psList g'
          in (psList, rsList, g'')

