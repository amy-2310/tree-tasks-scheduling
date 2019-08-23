# tree-tasks-scheduling optimizing space consumption
This is a project for my bachelor thesis with the title *Analysis and implementation of variants algorithms computing the minimal space consumption of tree-like processes*. The goal is to find an efficient algorithm computing a space-optimal schedule for tree-like processes.<br />
Definition of tree-like processes: all main-processes start at the same time; each process can generate one or
more new processes; all processes end at the same time.<br />
Such tree-like processes can be modled by a directed tree: the root node represents the start of executing the set of processes; other nodes represent tasks (operations) and each of them is marked with its space consumption; an edge pointing from node A to node B means that B should
happen directly after A was executed.<br />
 
Two algorithms are designed, analyzed and implemented in Haskell. The first one, *recursive SPOPTN*, uses an already existing Algorithm *SPOPTN* [1] and the concept of divide-and-conquer to solve the problem in polinomial time. Although its correctness has not yet been formally proven, the results of random testing show that the schedule delivered by *recursive SPOPTN* is likely space-optimal. The main function `print_spopt_schedule` is defined in module `OptSpaceRecursive`:
```
print_spopt_schedule :: [Input_Process] -> [Relation] -> IO ()
```
The second algorithm, *best-first-search*, delivers the exact optimum in exponential time and serves as a standard during random testing. The main function `print_best_first` is defined in module `OptSpaceBF`:
```
print_best_first :: [Input_Process] -> [Relation] -> IO()
```
For the type declarations of the above arguments, see the beginning of the modules.<br />
Beside the two implemented algorithms, a function for random testing is defined in module `RandomTreeTest`:
```
print_test :: Int -> Int -> Int -> Int -> Int -> IO()
```
It generates random trees and compares the space consumption delivered by `spopt_schedule` with that by `best_first`.
The five arguments are: number of trees, number of processes of a tree, max length of a process, max.
space of an operation, an integer seed for random-generator.
The positive results of random testing can be reproduced by executing the function `tests` in `tests.hs`. Note that due to the catastrophic run time of `best_first`, a test can become quite inefficient when the number of processes or the length of a process >= 6.

To use all main functions (including testing), load the file `examples.hs` in ghci. The file also contains several examples showing how to use the functions.

Ref.<br />
[1]  Schmidt-Schauss, M., and Dallmeyer, N. Optimizing space of parallel processes. Electronic Proceedings in Theoretical Computer Science 289 (Feb 2019), 53-67.
