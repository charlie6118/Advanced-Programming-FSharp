(*
  ITT8060 -- Advanced Programming 2018
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------
  Coursework 0: Getting started
  ------------------------------------
  Name: Shang-Yi Yu
  Student ID:shanyu 184242IV
  ------------------------------------
  Answer the questions below.  You answers to questions 2--8 should be
  correct F# code written after the question. The F# code for question
  1 is written for you and serves as an example. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.
  This coursework will NOT be graded but we encourage you to do it,
  you will not succeed in this course if you don't practice, and
  there's no time like the present! Also, you may find that parts of
  it appear in later courseworks. *)

// 0. Find your way to the fsharp interactive (fsi) command prompt.
// I.e. log in to a lab machine and start Visual Studio, install
// VS/Mono on your laptop, etc.


// 1. Load the  following function into fsi
let greeting name = printfn "Hello: %s" name


// 2. Run the function 'greeting' and  say hello to yourself.
greeting "Shang-Yi Yu"

// 3. Create a value 'myName : string" that contains your name.
let myName = "Shang-Yi Yu"
myName

// 3.Define
// 'splitAtChar : text:string -> sep:char -> list<string>'

let splitAtChar (text: string) (sep: char) = text.Split sep |> Array.toList

splitAtChar myName 'a'


// 4. Write a function splitAtSpaces in such a way that it uses splitAtChar
// Hint: we defined splitAtSpaces in the lecture, now you need to modify it.

let splitAtSpaces (text: string) = splitAtChar text ' '

splitAtSpaces myName

// 5. Define 'sentenceCount : text:string -> int'

let sentenceCount (text: string) = 
  let count = 0
  count + (splitAtChar text '.').Length - 1

sentenceCount myName

let mySentence = "My first sentence bro bro. My second sentence. My third sentence."

sentenceCount mySentence

// 6. Define 'stats : text:string -> unit'
// which prints the same stats as showWordCount +
// the number of sentences and average length of sentences
// hint: try float: 'int -> float'
let wordCount text =
  let words    = splitAtSpaces text
  let wordSet  = Set.ofList words
  let numWords = words.Length
  let numDups  = numWords - wordSet.Count
  numWords, numDups

let showWordCount text =
  let numWords, numDups = wordCount text
  printfn "--> %d words in text" numWords
  printfn "--> %d duplicate words" numDups
let stats (text: string) =
  let numWords, numDups = wordCount text
  showWordCount text
  printfn "--> %d sentences in text" (sentenceCount text)
  printfn "--> %f average length of sentence in text" ((float numWords) / float (sentenceCount text))

stats mySentence

// 7. Use the 'http' function from the lecture to download the file
// http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt as a string
// NOTE: you cannot use this function in tryfsharp. Instead you can
// paste the text into your file as a string and process it locally

let site1 = "http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt"

open System.IO
open System.Net

let http (url: string) = 
    let req = System.Net.WebRequest.Create url
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    resp.Close()
    html

let request = http site1


// 8. run 'stats' on the downloaded file

stats request