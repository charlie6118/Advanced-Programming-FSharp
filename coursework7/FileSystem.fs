module FileSystem

    type FsTree = Node of (string * FsTree) list

    let rec prepend dirName paths =
        match paths with
        | path :: rest -> (dirName :: path) :: prepend dirName rest
        | []           -> [[dirName]]

    let rec show (fs : FsTree) : string list list =
        match fs with
        | Node ((path, subFs) :: restFs) -> let res = show subFs
                                            prepend path res @ show (Node restFs)
        | Node []                            -> []



    let rec createDir (p : string list) (fs : FsTree) : FsTree = 
        match p, fs with
        | [last], Node (n) -> Node ((last, Node []) ::n)
        | pH :: pT, Node((nodeName, subFs) :: nT) when pH = nodeName -> Node((nodeName, createDir pT subFs) :: nT) 
        | p, Node((nodeName, subFs) :: nT) -> Node((nodeName, subFs) :: checkNodeList p nT)
        | _, Node [] -> Node []
    and  checkNodeList (p: string list) (node: (string * FsTree) list) =
        match p, node with
        | [last], node -> (last, Node []) :: node
        | pH :: pT, (nodeName, subFs) :: nodeT when pH = nodeName -> (nodeName, createDir pT subFs) :: nodeT
        | p, nodeH :: nodeT -> nodeH :: (checkNodeList p nodeT)
        | _, [] -> []

    let rec delete (p : string list) (fs : FsTree) : FsTree =
        match p, fs with
        | [last], Node((fsName, subFs) :: fsT) when last = fsName -> Node (fsT)
        | pH::pT, Node((fsName, subFs) :: fsT) when pH = fsName -> Node((fsName, delete pT subFs) :: fsT)
        | p, Node(fsH :: fsT) -> Node(fsH :: deleteNodeList p fsT)
        | _, Node [] -> Node []
    and deleteNodeList (p : string list) (node: (string* FsTree) list) =
        match p, node with
        | [last], (nodeName, subFs) :: nT when last = nodeName -> nT
        | pH :: pT, (nodeName, subFs) :: nT when pH = nodeName -> (nodeName, delete pT subFs) :: nT
        | p, nH :: nT -> nH :: deleteNodeList p nT
        | _, [] -> []