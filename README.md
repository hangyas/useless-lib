# useless-lib

Implementations of some algorithms for the sake of me understanding them. I chose scala because it fits any programming style and paradigm, I feel best for a given algorithm. No consistency between files. No optimizations. Most of it's is experimenting.

# Index

## Compression

 - [01-burrows-wheeler-transform](01-burrows-wheeler-transform) - rearranges a character string into runs of similar characters, used in bzip2 and DNA compression
 - [03-wavelet-tree](03-wavelet-tree) - succinct data structure to store strings in compressed space, using `rank` and `select` methods. Information about a given range can be extracted as well

## Search trees

 - [02-avl-tree](02-avl-tree) - self-balancing binary search tree
 - [04-b-tree](04-b-tree) - self-balancing data structure used in databases and file systems

# Todo

 - [x] Wavelet Tree
 - [ ] B-Tree
 - [ ] B+-Tree
 - [ ] Finger Tree
 - [ ] Rabin-Karp
 - [ ] Knuth-Morris-Pratt
 - [ ] Rope
 - [ ] RRB Trees (good for parallel computing) (Relaxed Radix Balanced Trees)
 - [ ] Write some shared code to print trees (with typeclasses)

# Test

This library is only meant to demonstrate the top-level ideas of algorithms, as such, I don't have the intention to write optimized and/or well-tested implementations. I might add proper test cases to some files when I feel like it.