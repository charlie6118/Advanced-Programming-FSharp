(*

  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 1: Basic operations on lists

  ------------------------------------
  Name: Shang-Yi Yu 184242IV
  TUT Student ID: shanyu
  ------------------------------------


  Answer all the questions below.  You answers to questions should be
  correct F# code written after the question in comments. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere and your result will not be evaluated.

  This coursework will be graded.

  To submit the coursework you will be asked to
  
  1) Check out your  GIT repository
  from the server gitlab.cs.ttu.ee using instructions on page
  https://courses.cs.ttu.ee/pages/ITT8060

  2) Put your solution into a file coursework1/coursework1.fsx
  in the repository. Commit it and push it to the server!
  It is your responsibility to make sure you have pushed the solution
  to the repository!

  NB! It is very important to make sure you use the exact name using
  only small caps. Files submitted under wrong name may not get a grade.

  Also, use the exact function and identifier names with precise types as 
  specified in the question.

  The F# interpreter should be able to load your solution without errors.

  NB! In this coursework you are not allowed to use functions from the
  List module. List processing and recursion has to be done explicitly.
*)


// 1. Associate an identifier "myFirstList" with an empty list of booleans.

let myFirstList = [] : bool list



// 2. Define a function
//
// elem : int -> 'a list -> 'a
//
// so that 'elem i xs' evaluates to the i-th element in the list xs.
//
// The function should throw an exception when i is less than zero or greater than
// the number of elements in xs. You should use 'failwith' to throw an exception.
//
// The function must be implemented via explicit recursion.
//
// Keep in mind that a list can either be empty ([]) or non-empty (x :: xs)
// and you need to define what is the i-th element of the list in both
// cases.
//
// When you have completed this definition then think about the time complexity
// of your implementation.


let sampleList = [2;4;5;6]


let rec elem (inputInt:int) inputList =
  match inputInt, inputList with
  | (_, [])                          -> failwith "The input Integer is less than zero or greater than the number of elements in the input List."
  | (inputInt, head :: tail)         -> if (inputInt = 0) then head else elem (inputInt - 1) tail
  
// elem 0 []
// elem 2 myFirstList
// elem 0 sampleList
// elem 1 sampleList
// elem 2 sampleList
// elem 3 sampleList
// elem 4 sampleList

// // Course Lab code

// let rec intContainsInListOrNot inputInt inputList =
//   match inputList with
//   | []                             -> false
//   | head :: _ when head = inputInt -> true
//   | _ :: tail                      -> intContainsInListOrNot inputInt tail

// intContainsInListOrNot 3 [1;2]


// let rec intContainsInListOrNotTwo inputInt inputList =
//   match inputList with
//   | []                             -> false
//   | head :: tail -> head = inputInt || intContainsInListOrNotTwo inputInt tail
  
// intContainsInListOrNotTwo 3 [1;2]

// intContainsInListOrNotTwo 3 [1;2;3]

// Here is a type synonym for a tuple for representing information about artists.

type Artist = (string * int * bool * string)

// The components of the tuple are:
// * name of the artist
// * year of birth
// * is the artist alive
// * country of birth


// 3. Define a list
//
// artists : Artist list
//
// which contains information about your favourite artists.
// The list should contain at least five unique artists.
//
// You can use this list in the following exercises to test your code.

let artists = [("Novel Harry", 1995, true, "UK") ; ("Movie Porter", 1985, true, "UK") ; ("Painting Penny", 1960, false, "USA") ; ("Physic Shelton", 1888, false, "USA") ; ("Artist Polina", 1990, true, "Ukraine")]
artists

// Access third element: https://docs.microsoft.com/zh-tw/dotnet/fsharp/language-reference/tuples
let getName (name, _, _, _) = name

let getYear (_, year, _, _) = year

let getAlive (_, _, live, _) = live

let getCountry (_, _, _, country) = country




// 4. Define a function
//
// bornLaterThan : int -> Artist list -> Artist list
//
// so that 'bornLaterThan y xs' evaluates to
// all the artists x in xs satisfying the condition (birth year of x is greater than y)
// and nothing else.
//
// The function must preserve the relative ordering of elements in xs.
//
// The function must preserve duplicates.

let rec bornLaterThan inputYear (inputList: Artist list) =
  match inputYear, inputList with
  | (_, [])              -> []
  | (year, head :: tail) -> if (getYear head) > year then head :: bornLaterThan year tail else bornLaterThan year tail

let (artistsExample: Artist list)= [("Novel Harry", 1995, true, "UK") ; ("Movie Porter", 1985, true, "UK") ; ("Painting Penny", 1960, false, "USA") ; ("Physic Shelton", 1888, false, "USA") ; ("Artist Polina", 1990, true, "Ukraine")]

bornLaterThan 1994 artistsExample

// 5. Define a function
//                        
// artistsFrom : string -> Artist list -> Artist list
//                        
// so that 'artistsFrom c xs' evaluates to
// all the artists in xs who were born in c
// and nothing else.
//
// The function must preserve the relative ordering of elements in xs.
//
// The function must preserve duplicates.

let rec artistsFrom inputCountry (inputList: Artist list) =
  match inputCountry, inputList with
  | (_, [])                 -> []
  | (country, head :: tail) -> if (getCountry head) = country then head :: artistsFrom country tail else artistsFrom country tail

artistsFrom "UK" artistsExample




// 6. Define a function
//                          
// names : Artist list -> string list
//                          
// so that 'names xs' evaluates to the names of the artists in xs.
//
// 'xs' and 'names xs' must have the same number of elements.
//
// The element at index i in 'names xs' must be the name of the
// artist at index i in 'xs'.


let rec names (inputList: Artist list) =
  match inputList with
  | [] -> []
  | (head :: tail) -> (getName head) :: names tail

names artistsExample



// 7. Using your solutions to previous exercises define a function
//    
// areFromAndBornLaterThan : string -> int -> Artist list -> string list
//    
// so that 'areFromAndBornLaterThan c y xs' evaluates to the names
// of the artists in xs who are from c and are born later than y.
//
// This should be a one-line definition.

let rec areFromAndBornLaterThan inputCountry inputYear (inputList: Artist list) = names (artistsFrom inputCountry (bornLaterThan inputYear inputList))

areFromAndBornLaterThan "UK" 1900 artistsExample