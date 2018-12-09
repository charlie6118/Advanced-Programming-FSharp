(*

  ITT8060 -- Advanced Programming 2018
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 5: bind, option, list

  ------------------------------------
  Name: Shang-Yi Yu
  Student ID: 184242IV
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2018 under your name, into a file
  coursework5/coursework5.fsx by ??WHEN??.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is
  incorrect it will not be graded.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

  NB! Do not delete the stubs we have provided! If you did not manage
  to complete the implementation then keep the incomplete stub and
  make sure that it still is typeable as described in the question.
*)


(*
   A function of type 'a -> 'b option can be thought of as a function
   of type 'a -> 'b that happens to be partial. This means that there
   may be values of type 'a where this function is undefined (we
   cannot produce a value of type 'b). The type 'a -> 'b option
   precisely says that given an 'a this function may produce a value
   of type 'b (failure to produce an output is represented by None).

   A function of type 'a -> 'b list can be thought of as a function of
   type 'a -> 'b that happens to be non-deterministic. This means that
   for some values of type 'a the function cannot produce a value of
   type 'b, for others it might produce exactly one 'b, and for some
   it may have several possible results. The type 'a -> 'b list
   precisely says that given a value of type 'a this function produces
   zero or more values of type 'b.
*)


(*
   Question 1

   By now you should be familiar with map and bind functions of both
   Option and List.

   Option.map : ('a -> 'b) -> 'a option -> 'b option -> apply function when Success, otherwise keep it obtain failure state
   List.map   : ('a -> 'b) -> 'a list   -> 'b list 

   Option.bind  : ('a -> 'b option) -> 'a option -> 'b option
   List.collect : ('a -> 'b list)   -> 'a list   -> 'b list -> function may produce multiple result, List.collect append every result together

   Option.map can be used to apply an ordinary function ('a -> 'b) to
   an optional value to get an optional result.

   Option.bind can be used to apply a partial function ('a -> 'b option)
   to an optional value to get an optional result.

   Similar idea applies to List.map and List.collect.

   Your first task is to implement two functions that allow to apply a
   "funny" function to a "funny" value to get a "funny" result.
   More precisely, you must implement

   applyO : ('a -> 'b) option -> 'a option -> 'b option
   applyL : ('a -> 'b) list   -> 'a list   -> 'b list

   You should use the corresponding 'bind' functions to implement these two.
   id : 'a -> 'a
   i.e. apply0 ((+) 3) (Some 3) -> get 6
   The result of applyO (Some id) (Some 3) must be Some 3. -> Some id: a identity function -> let id x = x

   The result of applyL [f1;...;fm] [v1;...;vn] must be
     [f1 v1;...;f1 vn;
      ...
      fm v1;...;fm vn]
   hint. Use List.collect
   i.e. applyL [(+) 3; (+) 2; (+) 1] [1; 2] -> [4; 5; 3; 4; 2; 3]
   
*)

let applyO mf ma =                // Spend more than 4 hours on this....
  match mf, ma with
  | Some f, Some a -> Some (f a)
  | _ ->  None

let rec applyL mf ma =            // Spend one hour...
  match mf with
  | [f] -> List.map f ma
  | head :: rest -> List.map head ma @ applyL rest ma
  | [] -> []

(*
   Whenever we have a value of type 'a we can view it as an optional
   value that "is there" or a non-deterministic value which happens to
   have exactly one possibility.
*)

let pureO a = Some a

let pureL a = [a]

// Here is a more compact infix notation for the two apply operations
let (<?>) mf ma = applyO mf ma
let (<*>) mf ma = applyL mf ma

// If you implemented applyO correctly then
//   pureO ((+) 1) <?> pureO 3
// should evaluate to
//   Some 4
 

// If you implemented applyL correctly then                 -> bind the first two argument together, then get a list of functions ??
//   [(+); fun x y -> y + x] <*> ["@";"$";"%"] <*> ["a";"b";"c"]
// should evaluate to
//   ["@a"; "@b"; "@c"; "$a"; "$b"; "$c"; "%a"; "%b"; "%c";
//    "a@"; "b@"; "c@"; "a$"; "b$"; "c$"; "a%"; "b%"; "c%"]
//
// Pay attention to the order in which the non-determinism is resolved.


(*
   Question 2

   Implement the function

   sequenceO : 'a option list -> 'a list option

   so that given a list xs of optional values it evaluates to
   * Some xs' when all of the optional values in xs were Some
     and xs' is a list of the values inside
   * None     when there was at least one None in xs.

   sequenceO [Some 1; Some 2] should evaluate to Some [1; 2]

   sequenceO [Some 1; None; Some 2] should evaluate to None

   You should implement this as a List.foldBack over the input list.
   (No need for rec or pattern matching.)
   
   hint. Use <?> and pureO to keep your code compact.
*)

// You may wish to use this to lift the :: constructor to an optional function.
let cons x xs = x :: xs

let applyPureOConsOperatorToListElement acc = pureO (cons) <?> acc // finally!!!!!!!!
// applyPureOConsOperatorToListElement
let sequenceO xs = List.foldBack (<?>) (List.map applyPureOConsOperatorToListElement xs) (pureO [])


// step by step refer
// Some 4, Some [] -> 4 :: [] -> [4] -> Some [4]
// Some 2, Some [4] -> 2 :: [3] -> [2; 3] -> Some [2; 3]
// Some 1, Some [2; 3] -> 1 :: [2; 3] -> [1; 2; 3] -> Some [1; 2; 3]

// step by step refer
// ( (pureO (cons)) <?> (Some 4) )  <?> (Some []) 
// ( (pureO (cons)) <?> (Some 2) )  <?> (Some [4]) 
// ( (pureO (cons)) <?> (Some 1) )  <?> (Some [2;4]) 


(*
   Question 3

   Implement the function

   sequenceL : 'a list list -> 'a list list   -> not an identity function

   which is an analogue of sequenceO (option has been replaced
   with list in the type signature).

   Try to do this by just copying your implementation of sequenceO and
   replacing pureO with pureL and <?> with <*>.

   If you implemented sequenceO in a good way then the result should
   typecheck and work as expected.

   sequenceL xs computes all possibilities to pick a single element
   from each of the lists in xs.

   sequenceL [[1; 2]; [3; 4]]

   should evaluate to

   [[1; 3]; [1; 4]; [2; 3]; [2; 4]]   
*)

let applyPureLConsOperatorToListElement' acc = pureL (cons) <*> acc
// applyPureLConsOperatorToListElement'
let sequenceL xs = List.foldBack (<*>) (List.map applyPureLConsOperatorToListElement' xs) (pureL [])
// sequenceL [[1; 2]; [3; 4]]

// step by step refer
// ( (pureL (cons)) <*> (pureL ([3; 4])) )  <*> (pureL []) 
// ( (pureL (cons)) <*> (pureL ([1; 2])) )  <*> ([[[3; 4]]]) 

(*
   Question 4

   As an example of a "non-deterministic" function we are going to
   consider rolling a dice. Define a function 

   roll : int -> int list

   such that given an integer n it gives all the possible outcomes
   (in increasing order) of rolling an n-sided die. 

   roll 6 should evaluate to [1;2;3;4;5;6]
*)

let roll n = [for x  in 1..n -> x]
// roll 6


(*
   Question 5

   Define a function

   rollTwo : int -> int -> (int * int) list

   such that given two integers m and n it gives all possible results
   of rolling an m-sided die and an n-sided die.

   rollTwo 2 3 should evaluate to
   [(1, 1); (1, 2); (1, 3);
    (2, 1); (2, 2); (2, 3)]

   hint. The <*> operation may be useful here.
*)

let turnListToTuple lst =
  match lst with
  | x :: [y] -> x, y
  | _ -> failwith "turnListToTuple only accept list with two element"

// turnListToTuple [1; 2]

let rollTwo m n = 
  let result = sequenceL [roll m; roll n]
  List.map turnListToTuple result

// let rollTwo m n = 
//   let result = sequenceL [roll m; roll n]
//   [ for element in result -> turnListToTuple element ]
// rollTwo' n = for x in n -> 

(*
   Question 6

   Implement the function

   rollTwo' : int -> (int * int) list

   such that given an integer n you roll an n-sided die
   and based on the outcome x of the roll you then roll an x-sided die.

   rollTwo' 3 should evaluate to
   [(1, 1);
    (2, 1); (2, 2);
    (3, 1); (3, 2); (3, 3)]

   Note that here we have a situation where based on the outcome
   of the first roll we decide what to roll next. This is different
   from the previous question where the two rolls did not depend on
   each other.

   Remember that the type of List.collect is

   ('a -> 'b list) -> 'a list -> 'b list

   Here the function ('a -> 'b list) allows you to do something with
   each 'a in the input list and then collect the results.
   
   create function in first step, then apply to the next value_??
   hint. produce a list of function and apply to the next value ??
*)

// refer, for each number -> map with (num, 1..num)
// 1, 2, 3
// 1, roll 1
// 2, roll 2
// 3, roll 3

// roll 1
let mapping num = [for x in roll num -> (num, x)]

let rollTwo' m = List.collect mapping [1..m] 

// rollTwo' 3

(*
   Question 7

   Implement the function

   rollN : int list -> int list list

   where the argument int list is a description of the dice you have
   and you must compute all possible outcomes of rolling the dice.
   More precisely,

   rollN [1;2;3]

   means that you are given a 1-sided die, a 2-sided die and a 3-sided die
   and you must compute the possible outcomes of rolling these.

   This is similar to rollTwo except that here we are more flexible in the
   number of dice. Hence the result is a list of lists and not tuples.

   The function sequenceL may be useful here.
*)

let rec turnIntListToRollN ds =
  match ds with
  | head :: tail -> (roll head) :: turnIntListToRollN tail
  | [] -> []

let rollN ds = 
  let getListForSequenceL = turnIntListToRollN ds
  sequenceL getListForSequenceL

rollN [1; 2; 3; 4]


(*
   Question 8

   Implement the function

   conditional : ('a -> bool) -> ('a -> bool) -> 'a list -> (int * int)

   such that conditional f g xs evaluates to (m, n) so that thinking
   of (m, n) as a fraction m / n gives the ratio of elements
   satisfying the predicate f among those elements of xs that satisfy
   the predicate g.
*)

let mappingCount result bool  =
  match bool with
  | true -> result + 1
  | false -> result

// List.map (fun (x, y) -> x + y >= x * y) (rollTwo 6 6)


// List.fold mappingCount 0 (List.map (fun (x, y) -> x + y >= x * y) (rollTwo 6 6))
// List.fold mappingCount 0 (List.map (fun _ -> true) (rollTwo 6 6))
let conditional f g xs = 
  let n = List.fold mappingCount 0 (List.map (g) xs)
  if n = 0 then (0, 0) 
  else 
  let m = List.fold mappingCount 0 (List.map (f) xs)
  (m, n)

conditional (fun (x, y) -> x + y >= x * y) (fun _ -> true) (rollTwo 6 6)
  
// This expression should then compute the probability of the sum of
// rolling two 6-sided dice being greater than or equal to their
// product.
//
// conditional (fun (x, y) -> x + y >= x * y) (fun _ -> true) (rollTwo 6 6) -> get (12, 36)
// 
// hint. Could use Regular check here