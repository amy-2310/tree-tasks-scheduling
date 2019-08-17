import OptSpaceRecursive
import OptSpaceBF
import TopoSort --ony the data type Relation is needed, not the toposort
import RandomTreeTest
--------------------------------------------------------------------------------
--calculate a space-optimal schedule of the example tree in figure 1 in the
--thesis by using print_spopt_schedule and print_best_first respectively
p1 = [(1,[2,5,4,1]),(2,[3,1]),(3,[1,3])] --three processes
r1 = [Rel 1 1 [3]] --one relation: the first op of process 1 generates process 3
ex1a = print_spopt_schedule p1 r1
ex1b = print_best_first p1 r1
                
--calculate the same thing of another example tree by using the two functions
p2 = [(1,[2,8,4,16,1]),(2,[5,1,2]),(3,[8,2,2,1]),(4,[5,1,9,2,1]),(5,[8,8,1]),(6,[3,1,3])]
r2 = [(Rel 1 2 [2,3]),(Rel 4 1 [5]),(Rel 2 2 [6])]                        
ex2a = print_spopt_schedule p2 r2
ex2b = print_best_first p2 r2
--------------------------------------------------------------------------------
{-examples of random testing which compares the optimal space consumption
delivered by spopt_schedule with that by best_first. If two values match with
each other, True will be returned, otherwise False.

input for print_test: num. of trees, num. of processes of a tree,
max length of a process, max. space of an operation, an integer seed for making
a random-generator
-}
ex3 = print_test 5 5 5 10 102394

ex4 = print_test 1 7 7 15 8593
