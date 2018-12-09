(*

  ITT8060 -- Advanced Programming 2018
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: Recursive data types

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
  coursework4/coursework4.fsx by October 21, 2018.
  
  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is
  incorrect it will not be graded.

  We will consider the submission to be the latest version of the
  appropriate files in the appropriate directory before the deadline
  of a particular coursework.

*)

// In this coursework you will be required to design a file system data type
// that is able to carry the information about file names and access rights
// (read and/or write).
// The write permission on a directory is required to create or remove files
// in it, but not to write to an existing file in it.
// You will also be asked to create functions that can carry out various tasks
// on a file system.

// The access permissions are given by the following type:

type Permission = Read | Write | Execute | Traverse

// 1. Define a function
// createEmptyFilesystem: unit -> FileSystem
// that will be a function with 0 arguments that will return an
// empty filesystem of the type that you defined.  
// (Permissions are initially assumed to be Read and Write and Traverse for
// the root directory, check task 5)  
// We assume that your file system is defined in a type called FileSystem.
// Please note that you will later be asked to extend the type 

// If 
// FileSystem = unit
// then createFileSystem could be implemented:
// let createEmptyFilesystem () : FileSystem = ()
// If 
// FileSystem = string list or string list list
// then createFileSystem could be implemented:
// let createEmptyFilesystem () : FileSystem = []
// It is useful to have a more complex type for 
// FileSystem to distinguish files and directories
// as demonstrated in the implementation of the show
// function.


type FileSystem = Element list
and Element = | File of string * Permission Set
              | Dir of string * FileSystem * Permission Set

let createEmptyFilesystem () : FileSystem = []

 
// 2. Define a function 
// createDirectory : string list -> FileSystem -> FileSystem
// that will return a new file system containing the directory
// specified by the string list into the file system passed as the
// second argument.
// For example, evaluating
// createDirectory ["Dir1"; "Dir2"] (createEmptyFileSystem ())
// should evaluate to a file system containing "Dir1" and in "Dir1" there should 
// be "Dir2".
// Please note that createDirectory is expected to create all directories in the path
// if they do not exist beforehand.
// (Permissions are initially assumed to be Read, Write and Traverse, check task 5)  

// let rec createDirectory (path : string list) (fs : FileSystem) : FileSystem =
//    match path with
//    | dir :: rest -> Dir (dir,(createDirectory rest (createEmptyFilesystem ())), set[Read; Write; Traverse]) :: fs
//    | []          -> fs

// return a tuple
let rec getDirAndFs dirName fs =
  match fs with 
  | Dir (_, _, permissionSet) :: _ when not (Set.contains Traverse permissionSet) -> failwith "Can't access without Traverse permission"
  | Dir (existingDirName, dfs, permissionSet) :: tail when existingDirName = dirName -> Dir (existingDirName, dfs, permissionSet), tail
  | h :: tail                                                  -> let dir, rest = getDirAndFs dirName tail
                                                                  dir, h :: rest
  | []                                                         -> Dir (dirName, createEmptyFilesystem(), set[Read; Write; Traverse]), []

let rec createDirectory (path : string list) (fs : FileSystem) : FileSystem =
   match path with
   | s :: rest -> let d, dfs = getDirAndFs s fs // if path Exist, enter, otherwise create
                  match d with
                  | Dir (_, _, permission) when not (Set.contains Write permission)-> failwith "Can't create directory without Write permission"
                  | Dir (dirName, subFs, permission) when Set.contains Write permission-> Dir (dirName, createDirectory rest (createEmptyFilesystem() @ subFs ), permission) :: dfs
                  | _ -> d :: dfs
   | []          -> fs

let fs = createDirectory ["Dir1"] (createEmptyFilesystem ())
createDirectory ["Dir3"] fs
createDirectory ["Dir1"] fs

let fs1 = createDirectory ["Dir1";"Dir2"] (createEmptyFilesystem ())

let fs2 = createDirectory ["Dir3"] fs1

let fs4 = createDirectory ["Dir3"] (createEmptyFilesystem ())

let fs5 = createDirectory ["Dir1"; "Dir2"] fs4

// createFile : string list -> FileSystem -> FileSystem
// that will create a file with the path given as the first argument in terms
// of a string list.
// createFile is expected to fail with exception (failwith) if the directory
// where the file is to be created does not exist.
// (Permissions are initially assumed to be Read and Write, check task 5)  

let rec createFile (path: string list) (fs: FileSystem) : FileSystem =
  match path, fs with
  | [fileName], File (fsFileName, _):: _ when fileName = fsFileName -> failwith "Can't create file with the same name" 
  | [fileName], fs -> File (fileName, set[Read; Write]) :: fs 
  | dirName :: tail, Dir (fsDirName, subFs, permission) :: fsTail when dirName = fsDirName &&  Set.contains Traverse permission -> Dir (fsDirName, (createFile tail subFs), permission) :: fsTail 
  | dirName :: _, Dir (fsDirName, _, _) :: _ when dirName = fsDirName -> failwith "Need traverse permission"
  | _, head :: fsTail -> head :: createFile path fsTail
  | dirName :: [tail], Dir (fsDirName, subFs, permission) :: fsTail when dirName = fsDirName && Set.contains Traverse permission && Set.contains Write permission -> Dir (fsDirName, (createFile [tail] subFs), permission) :: fsTail 
  | _, [] -> failwith "path not match"


// let rec createFile (path: string list) (fs: FileSystem) : FileSystem =
//   match path, fs with
//   | [fileName], _ -> File (fileName, set[Read; Write]) :: fs 
//   | dirName :: [_], Dir (fsDirName, _, permission) :: _ when dirName = fsDirName && not (Set.contains Write permission) -> failwith "Not have permission to write the directory"
//   | dirName :: [tail], Dir (fsDirName, subFs, permission) :: fsTail when Set.contains Traverse permission && Set.contains Write permission-> if dirName = fsDirName then Dir (fsDirName, (createFile [tail] subFs), permission) :: fsTail else Dir (fsDirName, subFs, permission) :: createFile path fsTail
//   | dirName :: _, Dir (fsDirName, _, permission) :: _ when dirName = fsDirName && not (Set.contains Traverse permission) -> failwith "Not have permission to access the directory."
//   | dirName :: tail, Dir (fsDirName, subFs, permission) :: fsTail -> if dirName = fsDirName then Dir (fsDirName, (createFile tail subFs), permission) :: fsTail else Dir (fsDirName, subFs, permission) :: createFile path fsTail
//   | _, [File (_)] -> failwith "path not match"
//   | _, [] -> failwith "path not match"
//   | [], _ -> failwith "Can't create the same file"

fs1
createFile ["Dir1"; "Dir2"; "File1"] fs1
fs2
createFile ["Dir1"; "Dir2"; "File1"] fs2
fs5
let fs6 = createFile ["Dir1"; "Dir2"; "File1"] fs5
// let fs7 = createFile ["Dir1"; "Dir2"; "File1"] fs6

// 3. Define a function
// countFiles : FileSystem -> int
// that will recursively count the number of files in the current filesystem.
// (countFiles should not honour permissions).

let rec countFiles (fs:FileSystem) : int =
  match fs with 
  | fsHead :: fsTail -> (isFile fsHead) + (countFiles fsTail)
  | [] -> 0
and isFile element =
  match element with
  | Dir (_, subFs, _) -> countFiles subFs
  | File (_) -> 1

let fsWithFile = [Dir ("Dir1", [Dir ("Dir2", [File ("File1", set[Read; Write])], set[Read; Write; Traverse])], set[Read; Write; Traverse]); Dir ("Dir3", [File ("File2", set[Read; Write])], set[Read; Write; Traverse])]
countFiles fsWithFile
countFiles fs6

// 4. Define a function
// show : FileSystem -> string list list
// That will return a list of files and directories where
// each file and directory is represented as a string list.
// Please note that the grading of several further functions
// depends on the correctness of the show function.
// The show function is expected to output a path of each file and directory
// in the file system regardless of the permissions.

let rec prepend dirName paths =
    match paths with
    | path :: rest -> (dirName :: path) :: prepend dirName rest
    | []           -> [[dirName]]

let rec show (fs :FileSystem) : string list list =
    match fs with
    | Dir (dir, subFs, _) :: restFs -> let res = show subFs
                                       prepend dir res @ show restFs
    | File (file,  _) :: restFs -> [file] :: show restFs
    | []                            -> []

fs2
show fs1
fs1
show fs2
fs2
fsWithFile
show fsWithFile
// 5. Define a function
// changePermissions : Permission set -> string list -> FileSystem -> FileSystem
// that will apply the specified set of permissions to the file or directory
// represented by a string list. For example, list ["Dir1";"Dir2";"File1"]
// represents a structure where "Dir1" is in the root directory, "Dir2" is
// in "Dir1" and "File1" is in "Dir2".
// changePermissions is assumed to work at super user level, i.e. it should be
// possible to change the permissions of every file and directory regardless of their
// previous permissions.

let rec changePermissions (permissionSet: Permission Set) (path: string list) (fs: FileSystem) : FileSystem =
   match path, fs with
   | [fileName], File (name, _) :: tail when fileName = name-> File (name, permissionSet) :: tail
   | element :: rest, Dir (dirName, subFs, _) :: fsTail when element = dirName -> Dir (dirName, (changePermissions permissionSet rest subFs), permissionSet) :: fsTail
   | _, fsHead :: fsTail -> fsHead :: changePermissions permissionSet path fsTail
   | [], []          -> []
   | _, []          -> failwith "Path not exist"
   | _, fs -> fs 

changePermissions (set[Read]) (["Dir1"; "Dir2"; "File1"]) fsWithFile
let noWritePermission = changePermissions (set[Write; Read]) (["Dir3"]) fsWithFile
noWritePermission

fsWithFile

createDirectory ["Dir3"; "Dir1"] fsWithFile


fs1
changePermissions (set[Read; Write; Traverse; Execute]) (["Dir1"; "Dir2"]) fs1

// 6. Modify the implementations of createDirectory and createFile to honor the
// permissions of the current file system level.
// Files can be created in a directory with Traverse and Write permissions. All of the
// above directories must at least have the Traverse permission.
// Directories can be created in a directory with Traverse and Write permissions. All of the
// above directories must at least have the Traverse permission.
// If the permissions do not allow creation of the item, the appropriate function should fail
// with an exception and an appropriate message (that you should formulate yourself).
// Hint: use the built-in failwith function.

compare (set[Write; Read]) (set[])
compare (set[Write; Read]) (set[Write;Read; Traverse; Execute])
compare (set[Traverse; Write]) (set[Write;Read; Traverse; Execute])
Set.contains (Traverse) (set[Write;Read; Traverse; Execute]) && Set.contains (Write) (set[Read; Traverse; Execute])

(set[Write; Read]) = (set[Write;Read])

// 7. Implement the function
// locate : string -> FileSystem -> string list list
// that will locate all files and directories with name matching the first argument
// of the function. The return value should be a list of paths to the files and
// directories. Each path is a list of strings indicating the parent directory
// structure.
// Note that the locate should honor the permissions, i.e. the files from
// directories without the Read permission should not be returned and
// directories without the Traverse permission should not be traversed further.


let rec showByPermission' (fs :FileSystem) (uplayerPermission) : string list list =
    match fs with
    | Dir (dir, subFs, permission) :: restFs when Set.contains Write uplayerPermission && Set.contains Traverse permission && Set.contains Read permission -> let res = showByPermission' subFs permission
                                                                                                                                                              prepend dir res @ showByPermission' restFs uplayerPermission
    | Dir (dir, subFs, permission) :: restFs when Set.contains Traverse permission && not (Set.contains Read permission)-> let res = showByPermission' subFs permission
                                                                                                                           prepend dir res @ showByPermission' restFs uplayerPermission
    | File (file,  permission) :: restFs when Set.contains Read permission         -> [file] :: show restFs
    | _ :: restFs                                                                  -> showByPermission' restFs uplayerPermission
    | _                                                                            -> []


// let rec showByPermission (fs :FileSystem) : string list list =
//     match fs with
//     | Dir (dir, subFs, permission) :: restFs when Set.contains Traverse permission && Set.contains Read permission -> let res = showByPermission subFs
//                                                                                                                       prepend dir res @ showByPermission restFs
//     | Dir (dir, subFs, permission) :: restFs when Set.contains Traverse permission && not (Set.contains Read permission)-> let res = showByPermission subFs
//                                                                                                                            prepend dir res @ showByPermission restFs
//     | File (file,  permission) :: restFs when Set.contains Read permission         -> [file] :: show restFs
//     | _ :: restFs                                                                  -> showByPermission restFs
//     | _                                                                            -> []

let rec checkElementIsLast (elementName: string) (strList: string list) =
  match strList with
  | [element] when  element.Contains(elementName) -> true
  | _ :: tail -> checkElementIsLast elementName tail
  | _ -> false

let locate (elementName: string) (fs: FileSystem) : string list list =
  let getList = showByPermission' fs (set[Write;Read;Traverse])
  List.filter (checkElementIsLast elementName) getList

//List.filter (checkElementIsLast "Dir2") (filterByName "Dir2" fsWithFile)

//filterByName "Dir2" fsWithFile

let fsWithTwoSameDirName = createDirectory ["Dir3"; "Dir2"] fsWithFile
let fsWithTwoSameDirNameAndOneWithoutTraverse = changePermissions (set[Read; Write; Execute]) (["Dir3"; "Dir2"]) fsWithTwoSameDirName
let fsWithTwoSameDirNameAndOneWithout = changePermissions (set[Traverse]) (["Dir1"]) fsWithTwoSameDirName

locate "File" fsWithTwoSameDirNameAndOneWithout


fsWithTwoSameDirName
fsWithTwoSameDirNameAndOneWithoutTraverse
locate "Dir2" fsWithTwoSameDirName
locate "Dir2" fsWithTwoSameDirNameAndOneWithoutTraverse

// 8. Implement the function:
// delete : string list -> FileSystem ->FileSystem
// that will delete a file or directory given as the first argument from a file
// system specified as the second argument.
// In case the item to be deleted is a directory, it needs to honor permissions
// and recursively only delete files with write permissions from directories with 
// write permissions (in addition to Traverse permissions).
// Subdirectories which will become empty need to be deleted as well.
//
// Note that the directory listing of a read only directory cannot be changed, i.e.
// if there is ["Dir1";"Dir2";"File1"] and Dir1 has only Read and Traverse
// permissions while Dir2 and File1 have all permissions, and one tries to delete
// Dir1, only File1 should be deleted, because deleting Dir2 would alter the 
// directory listing of a read only directory Dir1.  

let rec deleteSubFs (fs: FileSystem) (uplayerPermission: Permission Set) : FileSystem =
  match fs with
  | Dir (_, subFs, permission) :: rest when (Set.contains Write uplayerPermission) && Set.contains Write permission && Set.contains Traverse permission-> deleteSubFs subFs permission @ deleteSubFs rest uplayerPermission
  | Dir (_, subFs, permission) :: rest when (Set.contains Write uplayerPermission) && Set.contains Write permission -> subFs @ deleteSubFs rest uplayerPermission
  | Dir (dirName, subFs, permission) :: rest when not (Set.contains Write uplayerPermission) && Set.contains Traverse permission -> Dir (dirName, deleteSubFs subFs permission, permission) :: deleteSubFs rest uplayerPermission
  | Dir (dirName, subFs, permission) :: rest when not (Set.contains Write uplayerPermission) && not (Set.contains Traverse permission) -> Dir (dirName, subFs, permission) :: deleteSubFs rest uplayerPermission
  | File (_, permission) :: rest when (Set.contains Write uplayerPermission) && (Set.contains Write permission) -> deleteSubFs rest uplayerPermission
  | File (fileName, permission) :: rest -> File(fileName, permission) :: deleteSubFs rest uplayerPermission
  | _ -> fs

let rec delete' (path: string list) (fs: FileSystem) (uplayerPermission: Permission Set) : FileSystem =
  match path, fs with
  | [element], Dir (dirName, subFs, permission) :: rest when element = dirName && Set.contains Write uplayerPermission && Set.contains Write permission && Set.contains Traverse permission -> (deleteSubFs subFs permission) @ rest
  | [element], Dir (dirName, subFs, permission) :: rest when element = dirName && Set.contains Traverse permission -> Dir (dirName, deleteSubFs subFs permission, permission) :: rest
  | [element], File (fileName, permission) :: rest when element = fileName && Set.contains Write uplayerPermission && Set.contains Write permission -> rest
  | head :: tail, Dir (dirName, subFs, permission) :: rest when head = dirName && Set.contains Traverse permission -> Dir (dirName,  delete' tail subFs permission, permission) :: rest 
  | _, head :: rest -> head :: delete' path rest uplayerPermission
  | _, _ -> fs

let rec delete (path: string list) (fs: FileSystem) : FileSystem =
  delete' path fs (set[Write; Read; Traverse])

let fsWithFileExample = [Dir ("Dir1", [Dir ("Dir2", [File ("File1", set[Read; Write; Traverse])], set[Read; Traverse])], set[Read; Write; Traverse])]

delete ["Dir1"; "Dir2"] fsWithFileExample 

fs2
delete ["Dir1"; "Dir2"] fs2
fsWithFile
delete ["Dir1"] fsWithFile
delete ["Dir1"; "Dir2"; "File1"] fsWithFile
let noWritePermissionFs = changePermissions (set[Read]) (["Dir1"; "Dir2"; "File1"]) fsWithFile
delete ["Dir1"; "Dir2"] noWritePermissionFs



// let rec deleteSubFs (fs: FileSystem) : FileSystem =
//   match fs with
//   | Dir (dirName, subFs, permission) :: rest when not (Set.contains Write permission) && Set.contains Traverse permission -> Dir (dirName, deleteSubFs subFs, permission) :: deleteSubFs rest
//   | Dir (dirName, subFs, permission) :: rest when Set.contains Write permission && Set.contains Traverse permission -> Dir (dirName, deleteSubFsLayer2 subFs, permission) :: deleteSubFsLayer2 rest
//   | head :: rest  -> head :: (deleteSubFsLayer2 rest)
//   | _ -> fs
// and deleteSubFsLayer2 (fs: FileSystem) : FileSystem =
//   match fs with
//   | Dir (_, _, permission) :: rest when Set.contains Write permission  -> deleteSubFs rest
//   | File (_, permission) :: rest when Set.contains Write permission -> deleteSubFs rest
//   | _ -> fs

// let rec delete (path: string list) (fs: FileSystem) : FileSystem =
//   match path, fs with
//   | [element], Dir (dirName, _, permission) :: rest when element = dirName && Set.contains Write permission -> rest
//   | [element], Dir (dirName, subFs, permission) :: rest when element = dirName && Set.contains Traverse permission -> Dir (dirName, deleteSubFs subFs, permission) :: rest
//   | [element], File (fileName, permission) :: rest when element = fileName && Set.contains Write permission -> rest
//   | head :: tail, Dir (dirName, subFs, permission) :: rest when head = dirName && Set.contains Traverse permission && Set.contains Write permission -> Dir (dirName,  delete tail subFs, permission) :: rest 
//   | _, head :: rest -> head :: delete path rest  
//   | _, _ -> fs
