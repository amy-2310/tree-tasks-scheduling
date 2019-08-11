import OptSpaceRecursive
import OptSpaceBF
import TopoSort --ony the data type Relation is needed, not the toposort
import Data.List
import System.Random

generateP maxLength maxSp generater = let (lengthP, g2) = randomR (1,maxLength) generater
                                          g3 = snd $ next g2
                                      in (take lengthP $ randomRs (1,maxSp) g2, g3)

generatePs nOfPs maxL maxSp g
        = generatePs_h nOfPs maxL maxSp g [] 1
          where generatePs_h nOfPs maxL maxSp g psList counter
                        = if counter <= nOfPs
                          then let (spList, g2) = generateP maxL maxSp g 
                               in generatePs_h nOfPs maxL maxSp g2 ((counter,spList):psList) (counter+1)
                          else (reverse psList, g)

generateRs psList g = generateRs_h psList g [] 1

generateRs_h psList g rsList counter
   = let length_psList = length psList
         length_p pid = length $ snd $ head $ filter (\(pid',list) -> pid' == pid) psList
     in if counter < length_psList
        then let (pid_main, g2) = randomR (counter+1, length_psList) g
                 (oid_main, g3) = randomR (1, (max 1 $ (length_p pid_main))) g2 --TODO?
                 r = Rel pid_main oid_main [counter]
             in if fst (randomR (True,False) g) == True
                then generateRs_h psList g3 (r:rsList) (counter+1)
                     --process with index = counter becomes sub-process
                else generateRs_h psList g3 (rsList) (counter+1)
                     --the process remains a main-process
        else --merge the relations having the same main pid and oid together. The pid of their sub-processes are packed into a list
             let rsList' = sort rsList
                 mergeSubPList ((r1@(Rel pid1 oid1 ls1)):(r2@(Rel pid2 oid2 ls2)):rest) mergedList
                        = if pid1 == pid2 && oid1 == oid2
                          then mergeSubPList ((Rel pid1 oid1 (ls1 ++ ls2)):rest) mergedList
                          else mergeSubPList  (r2:rest) (r1:mergedList)
                 mergeSubPList (r:[]) mergedList = mergeSubPList [] (r:mergedList)
                 mergeSubPList [] mergedList = (reverse mergedList, g)
             in mergeSubPList rsList' []

generateProgram nOfPs maxL maxSp g
        = let (psList, g') = generatePs nOfPs maxL maxSp g
              (rsList, g'') = generateRs psList g'
          in (psList, rsList, g'')

test_one nP maxL maxS seed = let g = mkStdGen seed
                                 (p,r,g') = generateProgram nP maxL maxS g
                                 maxSpace_r = fst $ last $ sort $ spopt_schedule p r
                                 maxSpace_b = fst $ last $ sort $ best_first p r
                             in ((p,r), maxSpace_r == maxSpace_b)

-- Input: n of test-programs, n of processes, max length, max space, seed for random-generator
-- Output: list of test-programs, result of comparison

--test::Int -> Int -> Int -> Int -> Int -> ([([(Int,[Int])],[Relation])], [(Int,Bool)])
test nT nP maxL maxS seed = test_h nT nP maxL maxS (mkStdGen seed) 1 [] []
test_h nT nP maxL maxS g counter programs results
        = if counter <= nT
          then let (p,r,g') = generateProgram nP maxL maxS g
                   maxSpace_r = fst $ last $ sort $ spopt_schedule p r
                   maxSpace_b = fst $ last $ sort $ best_first p r
               in  test_h nT nP maxL maxS g' (counter+1) ((counter, p,r):programs) ((counter, maxSpace_r == maxSpace_b):results)
          else (reverse programs, reverse results)

print_test nT nP maxL maxS seed = 
        let (programs, results) = test nT nP maxL maxS seed
        in do print_list programs
              print_list results
----------------------------------------------------------------------------------------------------

p:: [(Int, [Int])]
p = [(1,[1,3,2,3]),(2,[3]),(3,[2,3,1]),(4,[3,1]),(5,[1,5,4])]
r = [Rel 4 1 [3],Rel 5 1 [2],Rel 5 2 [1,4]]

p1::[(Int,[Int])]
p1 = [(1,[1,2]),(2,[3,4]),(3,[5,6,5])]
r1 = [Rel 1 1 [2],Rel 2 1 [3]]    

p1_t1::[(Int,[Int])]
p1_t1 = [(2,[4]),(3,[5,6,5])]
  
