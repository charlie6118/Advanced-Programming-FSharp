(*

  ITT8060 -- Advanced Programming 2018
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 7: property-based testing

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
  coursework7/coursework7.fsx by Nov 18.
  
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

#if INTERACTIVE 
#r    @".\FsCheck.dll"
#load "FileSystem.fs"
#endif

open FsCheck

open FileSystem

(*
   Question 1

   Define predicates

   fsTreeWf : FsTree -> bool

   pathWf   : string list -> bool

   that check whether the given tree is well-formed as a filesystem
   and whether the path (represented as a list of strings) is well-formed.

   A well-formed filesystem cannot have identical paths leading to
   different nodes in the tree.

   A well-formed path cannot contain nodes with empty names. 

   FsTree is a tree data type that we think of as a filesystem.
   It is peculiar since there is no difference between files and direcotries,
   everything is a node.
*)

let rec getLayerNodeList (fs:FsTree) =
    match fs with
    | Node [] -> []
    | Node ((path, _)::T) -> path :: (getLayerNodeList (Node (T)))

let rec noDuplicateInNameList (nameList: string list) =
    match nameList with
    | [] -> true
    | H :: T -> if List.contains H T then false else noDuplicateInNameList T

let checkLayerNoDuplicate (fs:FsTree) =
    noDuplicateInNameList(getLayerNodeList fs)

let rec fsTreeWf (fs: FsTree) = 
    if checkLayerNoDuplicate fs then
        match fs with
        | Node [] -> true
        | Node ((name, subFs) :: T) when name = "" || name = null -> false 
        | Node ((name, subFs) :: T)  -> if fsTreeWf subFs then fsTreeWf (Node (T)) else false 
    else 
        false

let rec checkPath (p: string list) =
    match p with
    | [] -> true
    | H :: T -> if H = "" || H = null then false else checkPath T

let rec pathWf (p: string list) = 
    if List.length p = 0 
        then false
    else
        checkPath p

(*
   Question 2

   Define a FsCheck property

   createDirWf : string list -> FsTree -> Property

   which checks that creating a well-formed path p (using createDir)
   in a well-formed tree fs (filesystem) results in a well-formed tree.

   Define this using a conditional property (==>).

   Convince yourself that this is a bad way to do testing by observing
   the amount of test inputs that trivially satisfy this property.
*)

let createDirWf (p: string list) (fs: FsTree) = 
    (pathWf p && fsTreeWf fs) ==> fsTreeWf (createDir p fs)
 
(*
   Question 3

   Define a generator

   wfTree : Gen<FsTree>

   that generates only well-formed trees (filesystems).


   Define a generator

   wfPath : Gen<string list>

   that generates only well-formed filesystem paths.


   You may want to use the predicates defined above to check that
   the generated data indeed is well-formed.
*)

let checkStr (str:string) = not (str = "" || str = null)

let wfTree : Gen<FsTree> = 
    let rec buildTree' s =
        match s with
        | 0 -> Gen.elements [Node[]]
        | _ -> 
            let buildSubTree = buildTree' (s/3)
            Gen.map2 (fun str subFs -> (str, subFs))
                     ((Arb.generate<string>) |> Gen.filter(fun x -> checkStr x))
                     (buildSubTree)
            |> Gen.listOf
            |> Gen.map Node
            |> Gen.filter (fun x -> fsTreeWf x)
    Gen.sized buildTree'

// Gen.sample 1 wfTree
// Arb.generate<string>
// Gen.elements possibleNames

let wfPath : Gen<string list> =
    Arb.generate<string>
    |> Gen.listOf
    |> Gen.filter (fun x -> pathWf x)

// Gen.sample 1 wfPath

(*
   Question 4

   Define an FsCheck property

   deleteIsWellFormed : string list -> FsTree -> bool

   which checks that given
   p  : string list
   fs : FsTree
   we have that after deleting p from fs the result is well-formed.

   You may assume here that this property is only used with the
   "well-formed" generators.

   The correct behaviour of delete is that if p is not present in fs
   then fs is returned as is.   
*)

let deleteIsWellFormed' =
    Prop.forAll (Arb.fromGen wfPath) (fun ps -> Prop.forAll (Arb.fromGen wfTree) (fun fs -> fsTreeWf (delete ps fs)))

let deleteIsWellFormed p fs =
   (delete p fs) |> fsTreeWf

// Check.Quick deleteIsWellFormed
// Check.Verbose deleteIsWellFormed'

(*
   Question 5

   Define an FsCheck property

   createDirExists : string list -> FsTree -> bool

   which checks that given
   p  : string list
   fs : FsTree
   we have that the path p is included (exactly once) in the
   result of show after we have created the directory p in fs.

   Here you may assume that this property is used only
   with well-formed generators, i.e., the fs and p that
   are passed in as parameters satisfy the well-formedness condition.

   The correct behaviour of createDir p fs is that it returns
   the given fs if p already exists (as a directory) in fs.
*)

let createDirExists' =
    Prop.forAll (Arb.fromGen wfPath) (fun ps -> Prop.forAll (Arb.fromGen wfTree) (fun fs -> fsTreeWf (createDir ps fs)))


let createDirExists p fs =
    fsTreeWf(createDir p fs)

// Check.Verbose createDirExists'

(*
   Question 6

   Define an FsCheck property

   deleteDeletes : FsTree -> bool

   which checks that given an
   fs : FsTree
   we have that by deleting one by one all of the items in the result of
   show fs we end up with an empty filesystem.
   
*)

let rec recDelete p fs= 
    match p with
    | [last] -> delete last fs
    | pH :: pT -> delete pH fs |> recDelete pT
    | [] -> fs

let deleteDeletes fs = 
    Node[] = recDelete (show fs) fs


let compareWithEmptyNode fs =
    match fs with
    | Node [] -> true
    | _ -> false

let deleteDeletes' =
     Prop.forAll (Arb.fromGen wfTree) (fun tree -> compareWithEmptyNode (recDelete (show tree) tree))

// Check.Verbose deleteDeletes'

(*
   Question 7

   Define an FsCheck property

   createAndDelete : FsTree -> string list -> string list -> Property

   which checks that given
   
   fs : FsTree
   p1 : string list
   p2 : string list

   we have that if p1 is not a prefix of p2 then

   1) creating directory p1 in fs
   2) creating directory p2 in the result
   3) deleting p1 from the result

   gives a filesystem which still contains p2.

   fs, p1, p2 are again from "well-formed" generators.
*)

let rec isPrefix p1 p2 =
   match p1, p2 with
   | [p1], [p2] when p1 = p2 -> true
   | p1H :: p1T, p2H :: p2T when p1H = p2H -> isPrefix p1T p2T
   | _, _ -> false


let createAndDelete (fs : FsTree) (p1 : string list) (p2 : string list) =
   (not (isPrefix p1 p2)) ==> (createDir p1 fs |> createDir p2 |> delete p1 |> fsTreeWf)


let createAndDelete' (fs : FsTree) (p1 : string list) (p2 : string list) = 
    Prop.forAll (Arb.fromGen wfPath) 
                (fun ps1 -> Prop.forAll (Arb.fromGen wfPath) 
                                        (fun ps2 ->  Prop.forAll (Arb.fromGen wfTree)
                                                                 (fun tree -> (not (isPrefix ps1 ps2)) ==> 
                                                                              (createDir ps1 tree 
                                                                              |> createDir ps2 
                                                                              |> delete ps1 
                                                                              |> fsTreeWf))))
// Check.Verbose createAndDelete'