# tree-tasks-scheduling
This is a project for my bachelor thesis with the title *Analysis and implementation of variants algorithms computing the minimal space consumption of tree-like processes*. The goal is to find an efficient algorithm computing a space-optimal schedule for tree-like processes.
Two algorithms are designed, analysed and implemented in Haskell. The first one, *recursive SPOPTN*, solves the problem in polinomial time. However, its correctness has not yet been proven. The main function `print_spopt_schedule` is defined in module `OptSpaceRecursive`:
```
print_spopt_schedule :: [Input_Process] -> [Relation] -> IO ()
```
The second algorithm, *best-first-search*, delivers the exact optimum in exponential time and serves as a standard during random testing. The main function `print_best_first` is defined in module `OptSpaceBF`:
```
print_best_first :: [Input_Process] -> [Relation] -> IO()
```
For the type declarations of the above arguments, see the beginning of the modules.
Beside the two implemented algorithms, a function for random testing is defined in module `RandomTreeTest`:
```
print_test :: Int -> Int -> Int -> Int -> Int -> IO()
```
It generates random trees and compares the space consumption delivered by `spopt_schedule` with that by `best_first`.
The five arguments are: number of trees, number of processes of a tree, max length of a process, max.
space of an operation, an integer seed for random-generator.

To use all main functions (including testing), load the file `examples.hs` in ghci. The file also contains several examples showing how to use the functions.
