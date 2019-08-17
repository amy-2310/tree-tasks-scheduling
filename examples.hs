import OptSpaceRecursive
import OptSpaceBF
import TopoSort --ony the data type Relation is needed, not the toposort
--------------------------------------------------------------------------------
--example tree in figure 1 in the thesis
p1 = [(1,[2,5,4,1]),(2,[3,1]),(3,[1,3])]
r1 = [Rel 1 1 [3]]
test1a = print_spopt_schedule p1 r1
test1b = print_best_first p1 r1
--------------------------------------------------------------------------------                 
--another example tree for testing
p2 = [(1,[2,8,4,16,1]),(2,[5,1,2]),(3,[8,2,2,1]),(4,[5,1,9,2,1]),(5,[8,8,1]),(6,[3,1,3])]
r2 = [(Rel 1 2 [2,3]),(Rel 4 1 [5]),(Rel 2 2 [6])]                        
test2a = print_spopt_schedule p2 r2
test2b = print_best_first p2 r2
