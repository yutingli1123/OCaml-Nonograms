# Problem Description

## Background

Nonograms, also known as Picross, Griddlers, or Hanjie, are logic puzzles that originated in Japan and became widely popular in England around 1994. These puzzles involve revealing a hidden image in a grid by filling in cells based on numerical clues for rows and columns. Each clue describes the lengths of contiguous groups of filled cells in the respective row or column, separated by at least one blank cell.

The challenge for the solver is to deduce the correct pattern of filled and empty cells using logic alone, ensuring that the clues for all rows and columns are satisfied.

## Problem Statement

The objective of this project is to implement an Nonogram solver in OCaml. The solver will take as input the numerical clues for the rows and columns of a grid and output a valid solution that satisfies all the constraints. The project should be capable of handling grids of various sizes, including large puzzles (e.g., 25x20), and should ensure the solution is unique.

## Input and Output Specification

### Input

Two lists of lists of integers:

- rows: A list where each sublist corresponds to the clues for a row, given in top-to-bottom order.
- columns: A list where each sublist corresponds to the clues for a column, given in left-to-right order.
  
### Output

A 2D grid represented as a list of lists of strings:

- "Filled" represents a filled cell.
- "Empty" represents an empty cell.

