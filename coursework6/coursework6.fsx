(*

  ITT8060 -- Advanced Programming 2018
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------------------

  Coursework 6: Tail recursion

  ------------------------------------------------
  Name: Shang-Yi Yu
  Student ID: 184242IV
  ------------------------------------------------


  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework6.fsx in directory coursework6.

  The deadline for completing the above procedure is Friday, November 11, 2018.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:

  Write a function
  satisfiesPInList : (float -> float -> bool) -> (float * float) list -> float * float
  that returns the first element in list passed as the second argument of the
  function that satsifies the predicate passed as the first argument. The first
  element in the pair is passed as the first argument and the second as the second.
  Make sure your implementation uses explicit tail recursion.
*)

let rec satisfiesPInList f (floatPair: (float * float) list) : (float * float) =
  match floatPair with
  | [] -> failwith "No pair match"
  | (x, y) :: tail -> if (f x y) then (x, y) else satisfiesPInList f tail

satisfiesPInList (fun x y -> (x < y)) [(1.0, 2.0); (4.0, 5.0)]

(*
  Task 2:

  Write a function
  createPairsOfList : 'a -> 'a list -> ('a * 'a) list
  that takes a list of 'a-s and returns a list of pairs of 'a-s that are taken
  sequentially from the list passed as the second argument to the function.
  In case the list has odd number of elements make the first argument of the
  function be the second element in the pair. 
  Make sure your implementation uses explicit tail recursion.
*)

// let rec createPairsOfList element (list: 'a list) : ('a * 'a) list =
//   match list with
//   | [] -> []
//   | [last] -> [(last, element)] 
//   | x :: y :: tail -> (x, y) :: (createPairsOfList element tail)

let rec createPairsOfList element (list: 'a list) : ('a * 'a) list =
  let rec tailAccForT2 element (list, acc) = 
    match list with
    | [] -> acc
    | [last] -> (last, element) :: acc
    | x :: y :: tail -> tailAccForT2 element (tail, (x, y) :: acc)
  List.rev (tailAccForT2 element (list,[]))

(*
  Task 3:

  Write a function
  createPairsOfListFold : 'a -> 'a list -> ('a * 'a) list
  that takes a list of 'a-s and returns a list of pairs of 'a-s that are taken
  sequentially from the list passed as the second argument to the function. In case
  the list has odd number of elements make the first argument of the function be the
  second element in the pair. 
  Make sure your implementation uses List.fold or List.foldBack appropriately.
  Test yourself if this implementation appears to be tail recursive.
*)

let mapping' acc (x:'a) = 
  match acc with
  | (a, None) :: tail -> (a, Some x) :: tail
  | (Some a, Some b) :: tail -> (Some x, None) :: acc 
  | [] -> [(Some x, None)]

let optionToA e option =
  match option with
  | (Some a, Some b) -> a, b
  | (Some a, None) -> a, e

let createPairsOfListFold element list = List.rev (List.map (optionToA element) (List.fold (mapping') [] list))

createPairsOfListFold 2 [1; 2]
createPairsOfListFold 2 [1; 2; 3; 4]
createPairsOfListFold 2 [1; 2; 3; 4; 5; 6]
createPairsOfListFold 2 [1; 2; 3; 4; 5; 6; 7; 8; 9]

(*
  Task 4:

  Below you find the definition of a type Tree of leaf-labeled trees. Write a
  function medianInTree : float Tree -> float that returns the median label in the
  given tree, i.e. the difference in counts of elements to the right and left is
  either 0 or the count of elements to the right is exactly 1 greater than the
  count of elements to the left.
  Use continuation-passing style in your implementation.
*)

type 'a Tree =
  | Leaf   of 'a
  | Branch of 'a Tree * 'a Tree


let rec getTreeList (tree: float Tree) cont acc =
  let temp =
    match tree with
    | Leaf (f) -> cont (f :: acc)
    | Branch(left, right) -> getTreeList right (fun restR -> getTreeList left (fun restL -> cont(restL) ) restR ) acc
  temp

let rec medianInTree (tree: float Tree) =
  let treeList = getTreeList tree id []
  let listLength = List.length treeList
  let index = if (listLength) % 2 = 0 then (listLength / 2) - 1 else ((listLength - 1) / 2) 
  treeList.[index]


medianInTree (Branch (Branch (Leaf (2.0), Leaf (5.0)), Leaf (0.0)))
medianInTree (Branch (Branch (Leaf (2.0), Leaf (5.0)), Branch(Leaf (7.0),Leaf (0.0))))

getTreeList (Branch (Branch (Leaf (2.0), Leaf (5.0)), Leaf (0.0))) id []
getTreeList (Branch (Branch (Leaf (2.0), Leaf (5.0)), Branch(Leaf (7.0),Leaf (1.0)))) id []


// cmd + d