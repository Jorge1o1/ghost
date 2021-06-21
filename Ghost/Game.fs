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

    let ALPHABET = [ 'A' .. 'Z' ]
    let CORPUSPATH = "data/corpus2.txt"

    let corpusTrie =
        let wordlist = Set(System.IO.File.ReadLines(CORPUSPATH))
        Set.foldBack Trie.insert wordlist Trie.empty

    // All the legal moves (not necessarily good ones)
    let legalMoves = 
        Challenge :: (List.map (string >> AddLetter) ALPHABET)

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