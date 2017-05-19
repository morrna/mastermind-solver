Mastermind Solver
=================

This is a simple solver for the board game [Mastermind][mmwiki].
I wrote it in Haskell, using list filtering to home in on the winning pattern.
Thanks to Haskell's lazy evaluation,
it shouldn't take too much memory to run this brute force solution.

Mastermind game not included!

## How to build

This package is built using [Stack][hastack].
With stack installed, you should be able to build and execute with

    cd mastermind-solver
    stack setup
    stack build
    stack exec mastermind-solver-exe

## Author info

Nathan Morrison, [https://github.com/morrna/](https://github.com/morrna)

[mmwiki]: https://en.wikipedia.org/wiki/Mastermind_(board_game)
[hastack]: https://docs.haskellstack.org/en/stable/README/
