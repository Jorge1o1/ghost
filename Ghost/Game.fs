namespace Ghost

module Game =
    type Move =
        | AddLetter of string
        | Challenge
    type IPlayer =
        abstract member Name: string
        abstract member Prompt: string -> Move
    type State =
        { Fragment: string
          Winner: IPlayer option
          CurrentPlayer: IPlayer
          OtherPlayer: IPlayer }

    let corpusTrie =
        let wordlist = Set(System.IO.File.ReadLines("data/corpus.txt"))
        Set.foldBack Trie.insert wordlist Trie.empty 

    let addLetter state letter =
        { state with
              Fragment = state.Fragment + letter
              CurrentPlayer = state.OtherPlayer
              OtherPlayer = state.CurrentPlayer }

    let challenge state =
        if Trie.isValidFragment state.Fragment corpusTrie then
            printf "%s IS a valid fragment! " state.Fragment
            { state with
                  Winner = Some(state.OtherPlayer) }
        else
            printf "%s IS NOT a valid fragment! " state.Fragment
            { state with
                  Winner = Some(state.CurrentPlayer) }        
    
    let isLegal move state =
        match move with
        | AddLetter _ -> true
        | Challenge -> state.Fragment.Length > 3

    let rec advance state =
        let move = state.CurrentPlayer.Prompt(state.Fragment)
        if not (isLegal move state) then
            printfn "That move is not legal."
            advance state 
        else 
            let nextState =
                match move with
                | AddLetter l -> addLetter state l
                | Challenge -> challenge state
        
            match nextState.Winner with
            | None -> advance nextState
            | Some(winner) -> winner