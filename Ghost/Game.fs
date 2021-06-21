namespace Ghost

module Game =
    type Move =
        | AddLetter of string
        | Challenge
    type State =
        { Fragment: string
          Winner: IPlayer option
          CurrentPlayer: IPlayer
          OtherPlayer: IPlayer }
    and IPlayer =
        abstract member Name: string
        abstract member Prompt: State -> Move

    let alphabet = [ 'A' .. 'Z' ]
    let MINLENGTH = 0

    let corpusTrie =
        let wordlist = Set(System.IO.File.ReadLines("data/corpus.txt"))
        Set.foldBack Trie.insert wordlist Trie.empty

    // Returns true if a move is legal
    let isLegal move state =
        match move with
        | AddLetter _ -> true
        | Challenge -> state.Fragment.Length > MINLENGTH

    // All the legal moves (not necessarily good ones)
    let legalMoves state = 
        let moves = (List.map (string >> AddLetter) alphabet)
        if state.Fragment.Length > MINLENGTH then Challenge :: moves else moves

    let addLetter state letter =
        { state with
              Fragment = state.Fragment + letter
              CurrentPlayer = state.OtherPlayer
              OtherPlayer = state.CurrentPlayer }

    let challenge state =
        if Trie.isValidFragment state.Fragment corpusTrie then
            { state with
                  Winner = Some(state.OtherPlayer) }
        else
            { state with
                  Winner = Some(state.CurrentPlayer) }        

    let advance state move =
        if not (isLegal move state) then
            printfn "That move is not legal. %A %A" move state
            state
        else 
            match move with
            | AddLetter l -> addLetter state l
            | Challenge -> challenge state

    let rec loop state = 
        let move = state.CurrentPlayer.Prompt(state)
        let nextState = advance state move
        match nextState.Winner with
        | None -> loop nextState
        | Some winner when winner = state.CurrentPlayer -> 
            printfn "%s IS NOT a valid fragment! " state.Fragment
            winner
        | Some winner ->
            printfn "%s IS a valid fragment! " state.Fragment
            winner