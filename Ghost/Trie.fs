namespace Ghost

module Trie = 
    type Node = { word: string option
                  children: Map<char, Node> }

    /// An empty word Trie    
    let empty = { word = None; children = Map.empty}  

    /// Inserts "word" into the trie "node"
    let insert word node =
        
        /// Returns a new node with char pointing to child. If char already points to
        /// some other child that binding is overwritten.
        let add char child node =
            { node with children = Map.add char child node.children}

        // Recursive helper
        let rec recursiveInsert node chars =
            match chars with
            | [] -> {node with word = Some(word)}
            | hd :: tl -> 
                match Map.tryFind hd node.children with
                | None -> add hd (recursiveInsert empty tl) node
                | Some child -> add hd (recursiveInsert child tl) node
        
        recursiveInsert node (word |> Seq.toList)

    /// Returns Some(node) if walking the sequence of chars leads to a valid Node, and if
    /// At all node on the way there, pred evaluates to true. Includes the last node
    /// but not the first. If the path is incomplete or the pred is false, returns None
    let rec canWalkAndAll chars node pred =
        match chars with
        | [] -> if pred node then Some(node) else None
        | hd :: tl ->
            match Map.tryFind hd node.children with
                | None -> None
                | Some child -> if (pred child) then (canWalkAndAll tl child pred) else None

    /// Returns true if any descendant causes the predicate to evaluate to true.
    /// Includes the last node but not the first.
    let rec anyDescendant node pred =
        let recPred _ child =
            pred child || anyDescendant child pred // Depth-First
        Map.exists recPred node.children

    /// Returns true if fragment doesn't have any words in its ancestors AND fragment 
    /// is not a word itself AND fragment also has at least one word in its children
    let isValidFragment fragment trie =
        let isWord node = node.word.IsSome
        let isNotWord node = node.word.IsNone
        let chars = Seq.toList fragment
        
        match canWalkAndAll chars trie isNotWord with
        | Some n -> (isNotWord n) && (anyDescendant n isWord)
        | None -> false // Either an ancestor was a word or the path doesnt exist