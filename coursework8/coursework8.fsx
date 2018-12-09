(*
  ITT8060 -- Advanced Programming 2018
  Department of Computer Science
  Tallinn University of Technology
  ------------------------------------------------------------------------------

  Coursework 8: Sequences and computation expressions

  ------------------------------------------------------------------------------
  Name: Shang-Yi Yu
  Student ID: 184242IV
  ------------------------------------------------------------------------------

 
  Answer the questions below. You answers to the questions should be correct F#
  code written after the question. This file is an F# script file; it should be
  possible to load the whole file at once. If you can't, then you have
  introduced a syntax error somewhere.

  This coursework will be graded.

  Commit and push your script part of the solution to the repository as file
  coursework8.fsx in directory coursework8.


  The deadline for completing the above procedure is Friday, December 2, 2018.

  We will consider the submission to be the latest version of the appropriate
  files in the appropriate directory before the deadline of a particular
  coursework.

*)

(*
  Task 1:


  Define a function returning a sequence
  
  sequenceUsing : int -> (int->int) -> int -> seq<int>
   
  that generates a sequence of integers such that:
  * the length of the sequence is defined by
  the first argument;
  * the sequence is computed based on the function give as the
  second argument, and
  * the first element is computed by applying the function
  given in the second argument to the initial value given as 
  the third argument.

  E.g. sequenceUsing 2 ((+) 1) 0 should generate a sequence
  containing the elements 1 and 2.
  
*)

// not produce correct out put
// fun should compute the based on the first

let rec sequenceUsing (count:int) (fn: int->int) (init:int) : seq<int> =
  // seq { for i in 1 .. count -> fn (init + i - 1) }
  seq {
    if count <> 0 then
      let x = fn init
      yield x 
      yield! sequenceUsing (count-1) fn x
    }

sequenceUsing 2 ((+) 1) 0

(*
  Task 2:


  Define a function returning a sequence
  
  pseudoRandom : int -> seq<int> -> seq<int>
  
  that generates pseudo random numbers based on the first argument as seed and 
  second argument as an infinite sequence of values of int type.

  Use the above sequence nat defined in lecture11.fsx as input for testing.
  Use the built in hash function to combine the seed and input integers.
  Experiment with ways how to combine the seed and input value in such a way 
  that the order of the values in the output sequence differs from that of the input
  sequence.
  
*)

let nat = Seq.initInfinite (fun i -> i)

// Implement pseudo random refer from http://www.fssnip.net/16/1
let removeFromSeq n seq = seq |> Seq.filter (fun x -> x <> n)
let rec pseudoRandom  (seed:int) (input:seq<int>) : seq<int> = 
  seq{
    let x = input |> Seq.item 0
    yield hash (x, seed)
    let sqn' = removeFromSeq x input
    if not (Seq.isEmpty sqn') then yield! pseudoRandom seed sqn'
  }

// pseudoRandom 7 nat
// pseudoRandom 10 nat
// pseudoRandom 4 nat


(*
  Task 3:

  Define a function
  
  cacheObserver : seq<'a> -> seq<'a>
  
  that will cache the values of a sequence and print "Cached"
  to standard output every time the value requested from the sequence is actually cached.
 
  implement my own cache 查 "memoization"

*)

// refer memoize from the link: http://www.fssnip.net/mW/title/memoize-

let memoize fn =
  let cache = new System.Collections.Generic.Dictionary<_,_>()
  (fun x -> match cache.TryGetValue x with
            | true, v ->  printfn "cached"
                          v
            | false, _ -> let v = fn (x)
                          cache.Add(x,v)
                          v)


let rec cacheObserver (input:seq<'a>) : seq<'a> = 
  seq{
    let x = input |> Seq.head |> memoize id
    yield x
    let sqn' = input |> Seq.tail
    if not (Seq.isEmpty sqn') then yield! cacheObserver sqn'
  }

cacheObserver nat

let nat' = seq { for i in 1 .. 10 -> if i < 4 then 1 else i }

nat'

cacheObserver nat'

(*
  Task 4:

  A function from a type 'env to a type 'a can be seen as a computation that
  computes a value of type 'a based on an environment of type 'env. We call such
  a computation a reader computation, since compared to ordinary computations,
  it can read the given environment. Below you find the following:

    • the definition of a builder that lets you express reader computations
      using computation expressions

    • the definition of a reader computation ask : 'env -> 'env that returns the
      environment

    • the definition of a function runReader : ('env -> 'a) -> 'env -> 'a that
      runs a reader computation on a given environment

    • the definition of a type Expr of arithmetic expressions

  Implement a function eval : Expr -> Map<string, int> -> int that evaluates
  an expression using an environment which maps identifiers to values.
  
  NB! Use computation expressions for reader computations in your implementation.
  
  Note that partially applying eval to just an expression will yield a function of
  type map <string, int> -> int, which can be considered a reader computation.
  This observation is the key to using computation expressions.

  The expressions are a simplified subset based on
  Section 18.2.1 of the F# 4.1 specification:
  https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf
  Ch18.2

  從lecture6幹所有東西，把計算用computation expression resaderbuilder代掉
  觀察lecture11 sizeCount example
*)

type ReaderBuilder () =
  member this.Bind   (reader, f) = fun env -> f (reader env) env
  member this.Return x           = fun _   -> x

let reader = new ReaderBuilder ()

let ask = id

type Expr =
  | Const  of int          // constant
  | Ident  of string       // identifier
  | Neg    of Expr         // unary negation, e.g. -1
  | Sum    of Expr * Expr  // sum 
  | Diff   of Expr * Expr  // difference
  | Prod   of Expr * Expr  // product
  | Div    of Expr * Expr  // division
  | DivRem of Expr * Expr  // division remainder as in 1 % 2 = 1
  | Let    of string * Expr * Expr // let expression, the string is the identifier.

let rec eval (e:Expr) : (Map<string, int> -> int) = 
  reader{
    match e with
    | Const n -> return n
    | Ident s -> let! env = ask
                 return (Map.find s env)
    | Neg e -> let! env = ask
               return - (eval e env)
    | Sum (e1, e2) -> let! env = ask
                      return (eval e1 env) + (eval e2 env)
    | Diff (e1, e2) -> let! env = ask
                       return (eval e1 env) - (eval e2 env)
    | Prod (e1, e2) -> let! env = ask
                       return (eval e1 env) * (eval e2 env)
    | Div (e1, e2) -> let! env = ask
                      return (eval e1 env) / (eval e2 env)
    | DivRem (e1, e2) -> let! env = ask
                         return (eval e1 env) % (eval e2 env)
    | Let (s, e1, e2) -> let! env = ask
                         let v1 = eval e1 env
                         let env1 = Map.add s v1 env
                         return eval e2 env1
  }

let expr = Let ("a",Const 5, Prod(Sum(Ident("a"),Const 1),Const 6))
let a = 5 in (a + 1) * 6

// let eval' (e:Expr) (env:Map<string, int>) : int = failwith "not yet implemented"


// //Example:
// //keeping in mind the expression: let a = 5 in (a + 1) * 6
// let expr = Let ("a",Const 5, Prod(Sum(Ident("a"),Const 1),Const 6)
// eval expr Map.empty<string,int>
// should return 36     