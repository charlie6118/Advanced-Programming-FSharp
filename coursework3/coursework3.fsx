(*

  ITT8060 -- Advanced Programming 2018
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 3: Discriminated unions, higher-order functions

  ------------------------------------
  Name: Shang-Yi Yu 184242IV
  Tallinn University of Technology Student ID: 184242IV
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060-2018 under your name, into a file coursework3/coursework3.fsx.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.

  Your solution should not contain the keywords 'rec' and 'mutable'.

  Deadline for submission is Sunday, October 7.
*)


// A list can contain items of the same type.
// We use a Cell to collect things that can be slightly different.

type Cell = Empty | Value of int | Pair of (int * int)

let cellList = [Empty ; Value 2; Value 4; Pair (2,5); Empty; Value 4; Empty; Pair (1, 1)]
 
// 1. Define a function
//
// noEmptyCells : Cell list -> Cell list
//
// that discards all cells that are Empty.
//
// Use List.filter

let emptyReturnFalse inputCell =
  match inputCell with
  | Empty -> false
  | Value _ -> true
  | Pair (_) -> true

let noEmptyCells inputlist: Cell list = List.filter (emptyReturnFalse) inputlist

noEmptyCells cellList
// 2. Define a function
//
// filterGreaterThan : int -> Cell list -> Cell list
//
// that discards all cells with value less than or equal to n.
// This means that all Empty cells should be discarded,
// Value v should be discarded when v is less than or equal to n,
// Pair (x, y) should be discarded when both x and y are less than or equal to n.
//
// Use List.filter

let lessThenNReturnFalse n inputCell =
  match inputCell with
  | Empty -> false
  | Value x when x <= n -> false
  | Value _ -> true
  | Pair (x, y) when (x <= n) && (y <= n) -> false
  | Pair (_) -> true

let filterGreaterThan n inputlist: Cell list = List.filter (lessThenNReturnFalse n) inputlist

filterGreaterThan 3 cellList

// 3. Define a function
//
// increaseCells : int -> Cell list -> Cell list
//
// that increases the values in the cells by the given amount.
// Empty should stay Empty and for Pairs you should increase
// both of the values in the pair.
//
// Use List.map

let increaseN n inputCell =
  match inputCell with
  | Empty -> Empty
  | Value x -> Value (x+n)
  | Pair (x, y) -> Pair (x+n, y+n)

let increaseCells n inputlist: Cell list = List.map (increaseN n) inputlist

increaseCells 2 cellList

// 4. Define a function
//
// transformPairs : (int -> int -> Cell) -> Cell list -> Cell list
//
// that replaces the Pair cells in the list
// with the result of applying the given operation to the two integer values in the pair.
//
// 'transformPairs f xs' should replace a 'Pair (x, y)' with 'f x y'.
//
// Use List.map

let mapFunctionToPair inputFunction inputCell =
  match inputCell with
  | Empty -> Empty
  | Value x -> Value x
  | Pair (x, y) -> inputFunction x y 

let transformPairs inputFunction (inputList: Cell list) = List.map (mapFunctionToPair inputFunction) inputList

transformPairs (fun x y -> Value (x+ y)) cellList

// 5. Define a function
//
// pairsToEmpty : Cell list -> Cell list
//
// that replaces all Pairs with Empty cells.
    

let mapping5 inputCell =
  match inputCell with
  | Empty -> Empty
  | Value x -> Value x
  | Pair (x, y) -> Empty 
  
let pairsToEmpty (inputList: Cell list) = List.map mapping5 inputList

pairsToEmpty cellList


// 6. Define a function
//
// replicateCells : Cell list -> Cell list
//
// that replicates each cell in the list n times, where n is
// 0             for Empty
// max 0 v       for Value v
// max 0 (x + y) for Pair (x, y)
//
// Use List.collect

// Question: How to define a recursive function that could take the input value as another input value? 

// let rec mapping inputCell n =
//   match (inputCell, n) with
//   | Empty, _ -> []
//   | Value _, 0 -> []
//   | Value x, n -> Value x :: mapping (Value x) (n-1)
//   | Pair (_), 0 -> []
//   | Pair (x, y), n -> Pair (x, y) :: mapping (Pair (x, y)) (n-1)


let mappingFunction inputCell =
  match inputCell with
  | Empty -> []
  | Value x -> [for _ in 1..x -> Value x]
  | Pair (x, y) -> [for _ in 1..(x+y) -> Pair (x, y)]

let replicateCells inputList: Cell list = List.collect mappingFunction inputList

replicateCells cellList

// 7. Define a function
//
// flattenPairs : Cell list -> Cell list
//
// that replaces a Pair (x, y) with Value x followed by Value y.
//
// Use List.collect.

let mapping7 inputCell =
  match inputCell with
  | Empty -> [Empty]
  | Value x -> [Value x]
  | Pair (x, y) -> [Value x ; Value y]

let flattenPairs (inputList: Cell list) = List.collect mapping7 inputList

flattenPairs cellList

// 8. Define a function
//
// countCells : Cell list -> int * int * int
//
// which counts the number of Empty, Value, and Pair cells in the list.
//
// If 'countCells xs' is (1, 2, 3) then
// 1 is the number of Empty cells
// 2 is the number of Value cells
// 3 is the number of Pair  cells
//
// Use List.fold

let mapping8 inputTripple inputCell =
  match inputTripple, inputCell with
  | (x, y, z), Empty -> (x + 1, y, z)
  | (x, y, z), Value _ -> (x, y + 1, z)
  | (x, y, z), Pair (_) -> (x, y, z + 1)

let countCells (inputList: Cell list) = List.fold mapping8 (0, 0, 0) inputList

countCells cellList

// 9. Define a function
//
// cellsToString : Cell list -> string
//
// that constructs a string representation of the cells.
//
// Empty       is represented as "."
// Value v     is represented as "v"
// Pair (x, y) is represented as "x,y"
//
// Use "|" as the separator symbol.
// The result must start and end with the separator.
// You can convert an int to a string by the function 'string'.
//
// Use List.fold
   
let mapping9 (inputString: string) inputList =
  match inputString, inputList with
  | str, Empty -> str + ".|"
  | str, Value x -> str + string x + "|"
  | str, Pair (x, y) ->  let strPair = string x + "," + string y 
                         str + strPair + "|"
let cellsToString (inputList: Cell list) = List.fold mapping9 "|" inputList

cellsToString cellList

"abc" |> List.ofSeq

let strs = ["abc"; "safda"; "gh"]
List.collect List.ofSeq strs