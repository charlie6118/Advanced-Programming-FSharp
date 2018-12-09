(*

  ITT8060 -- Advanced Programming 2018
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists and tuples, recursion, combination of functions

  ------------------------------------
  Name: Shang-Yi Yu 184242IV
  Tallinn University of Technology Student ID: shanyu
  ------------------------------------
 

  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060-2018 under your name, into a file coursework2/coursework2.fsx by September 28, 2018.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.
*)

// 1. Create a type BibliographyItem that has the following structure:
// string list * string * (int * int) * int
// The meaning of the tuple elements is as follows:
// * The first field represents the list of author names where each name is in the format
//   "Lastname, Firstname1 Firstname2" (i.e. listing all first names after comma)
// * The second field represents the title of the publication
// * The third field represents a pair containing the starting page number and ending page number of the paper.
// * The fourth field represents the year of publication

type BibliographyItem = (string list * string * (int * int) * int)

// let bil:BibliographyItem = (["str"], "str", (12,12), 199)
// let BibligraphyList: BibliographyItem list = [["Malan, David J";"Lloyd, Doug";"Chan, Zamyla"], "CS50", (1,200), 1850 ; ["Cheng, Yu Chin";"Lee, Morris";"Chung, Hank"], "Design Pattern", (1,150), 1994]

// 2. Create a value bibliographyData : BibliographyItem list that contains
// at least 7 different publications on your favourite topic from https://dblp.uni-trier.de/ 
// Please note that you need not read the papers, just pick 7 papers that sound interesting to you from the database.
let bibliographyData: BibliographyItem list = [(["Widemann, Baltasar Trancón y";"Lepper, Markus"], "A Practical Study of Control in Objected-Oriented-Functional-Logic Programming with Paisley.", (150, 164), 2017) ; (["Zhang, Qian";"Zhang, Yong-Fei";"Qin, Shi-Yin"], "Modeling and Analysis for Obstacle Avoidance of a Behavior-Based Robot with Objected Oriented Methods.", (295, 302), 2009) ; (["Rundensteiner, Elke A."], "Objected-Oriented View Technology: Challenges and Promises.", (299, 308), 1996) ; (["Henderson-Sellers,	Brian"], "Some Metrics for Objected-Oriented Software Engineering.", (131, 139), 1992) ; (["Huang, Zhi";"Jia, Xiuping"], "A hybrid segmentation technique for objected-based hyperspectral data classification over complex sub-urban landscape.", (1, 4), 2013) ; (["Guelton, Serge"], "Pythran: Crossing the Python Frontier.", (83, 89), 2018) ; (["Marowka,	Ami"], "On parallel software engineering education using python.", (357, 372), 2018)]

// 3. Make a function compareLists : string list -> string list -> int that takes two string lists and
// returns 
// * Less than zero in case the first list precedes the second in the sort order;
// * Zero in case the first list and second list occur at the same position in the sort order;
// * Greater than zero in case the first list follows the second list in the sort order;
// You are encouraged to use String.Compare to compare individual strings. If the first authors are the same
// then the precedence should be determined by the next author.
// A missing author can be considered to be equivalent to an empty string.
// Please note that your implementation should be recursive over the input lists.

let rec compareLists (strList1:string list) (strList2: string list) =
  match (strList1, strList2) with
  | ([], [])  -> 0
  | (x::xs', y :: ys') -> if (x = y) then compareLists xs' ys' else compare x y  
  | (_, [])  -> 1
  | ([], _)  -> -1


// 4. Make a function
// compareAuthors : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors.
// Use solution from task 3.

let compareAuthors (item1:BibliographyItem) (item2:BibliographyItem) =
  match item1, item2 with
  | ((nameList1, _, _, _),(nameList2, _, _, _)) -> compareLists nameList1 nameList2


// 5. Make a function
// compareAuthorsYears : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors and if the authors are 
// the same then according to years.

let compareAuthorsYears (item1:BibliographyItem) (item2:BibliographyItem) =
  match item1, item2 with
  | ((nameList1, _, _, year1),(nameList2, _, _, year2)) -> if (compareLists nameList1 nameList2) = 0 then compare year1 year2 else compareLists nameList1 nameList2

// 6. Make a function 
// sortBibliographyByYear : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the year in ascending order


let rec sort (itemListForSort: BibliographyItem list) : BibliographyItem list=
  match itemListForSort with
  | [] -> []
  | (fstName, fstTitle, fstPage, fstYear) :: [(sndName, sndTitle, sndPate, sndYear)] when (fstYear > sndYear) -> (sndName, sndTitle, sndPate, sndYear) :: [(fstName, fstTitle, fstPage, fstYear)]
  | (fstName, fstTitle, fstPage, fstYear) :: [(sndName, sndTitle, sndPate, sndYear)] when (fstYear <= sndYear) -> (fstName, fstTitle, fstPage, fstYear) :: [(sndName, sndTitle, sndPate, sndYear)]
  | (fstName, fstTitle, fstPage, fstYear) :: (sndName, sndTitle, sndPate, sndYear) :: tail when (fstYear > sndYear) -> (sndName, sndTitle, sndPate, sndYear) :: sort ((fstName, fstTitle, fstPage, fstYear) :: tail)
  | (fstName, fstTitle, fstPage, fstYear) :: (sndName, sndTitle, sndPate, sndYear) :: tail when (fstYear <= sndYear) -> (fstName, fstTitle, fstPage, fstYear) :: sort ((sndName, sndTitle, sndPate, sndYear) :: tail)
  | last -> last

sort bibliographyData

let rec sortBibliographyByYear inputItemList: BibliographyItem list =
  let temp = sort inputItemList
  match temp with
  | [] -> []
  | (fstName, fstTitle, fstPage, fstYear) :: [(sndName, sndTitle, sndPate, sndYear)] -> if fstYear > sndYear then (fstName, fstTitle, fstPage, fstYear) :: [(sndName, sndTitle, sndPate, sndYear)] else (sndName, sndTitle, sndPate, sndYear) :: [(fstName, fstTitle, fstPage, fstYear)]
  | (_, _, _, fstYear)::(_, _, _, sndYear)::_ -> if fstYear > sndYear then sortBibliographyByYear temp else temp
  | last -> last

sortBibliographyByYear bibliographyData


// 7. Make a function 
// sortBibliographyByAuthorYear : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the authors and year in ascending order

let sort2 inputItemList: BibliographyItem list = 
  match inputItemList with
  | [] -> []
  | fst :: [snd] when (compareAuthors fst snd) > 0 -> snd :: [fst]
  | fst :: [snd] when (compareAuthors fst snd) < 0 -> fst :: [snd]
  | fst :: [snd] when (compareAuthors fst snd) = 0 -> if compareAuthorsYears fst snd > 0 then fst :: [snd] else snd :: [fst] 
  | fst :: snd :: tail when (compareAuthors fst snd) > 0 -> snd :: sort (fst :: tail)
  | fst :: snd :: tail when (compareAuthors fst snd) < 0 -> fst :: sort (snd :: tail)
  | fst :: snd :: tail when (compareAuthors fst snd) = 0 ->if compareAuthorsYears fst snd > 0 then fst :: sort (snd :: tail) else snd :: sort (fst :: tail)
  | last -> last

let rec sortBibliographyByAuthorYear inputItemList: BibliographyItem list =
  let temp = sort2 inputItemList
  match temp with
  | [] -> []
  | fst :: [snd] -> if (compareAuthors fst snd) > 0 then fst :: [snd] else snd :: [fst]
  | fst::snd::_ -> if (compareAuthors fst snd) > 0 then sortBibliographyByYear temp else temp
  | last -> last

sortBibliographyByAuthorYear bibliographyData

// 8. Make a function
// groupByAuthor : BibliographyItem list -> (string * BibliographyItem list) list
// where the return list contains pairs where the first element is the name of a single
// author and the second element a list of bibliography items that the author has co-authored.

let rec constructNameList (lst: BibliographyItem list) =
  match lst with
  | [] -> []
  | ((nameList), _, _, _) :: rest -> nameList @ constructNameList rest 

constructNameList bibliographyData

let rec regardDuplicate lst =
  match lst with
  | [] -> []
  | head :: tail -> if List.contains head tail then regardDuplicate tail else head :: regardDuplicate tail

let makeNameList lst = regardDuplicate (constructNameList lst)

makeNameList bibliographyData

let rec findAuthor name (inputItemList:BibliographyItem list) =
  match name, inputItemList with
  | (_, []) -> []
  | (name, (nameList, title, pagePair, year) :: rest ) -> if List.contains name nameList then (nameList, title, pagePair, year) :: findAuthor name rest else findAuthor name rest

let rec group nameList lst = 
  match nameList, lst with
  | [], _ -> ["", []]
  | head :: tail, lst -> (head, findAuthor head lst) :: group tail lst

group (makeNameList bibliographyData) bibliographyData

let groupByAuthor lst = group (makeNameList lst) lst

groupByAuthor bibliographyData

// let rec g (m:Map<string, int list>) lst =
//   match lst with
//   | i::tail when i > 0 -> let m1 = Map.add (string i) tail m
//                           g m1 tail
//   | _::tail -> g m tail
//   | _ -> m

// g Map.empty<string, int list> [-1..5]