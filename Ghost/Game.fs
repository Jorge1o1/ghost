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

    let wordlist =
        lazy (Set(System.IO.File.ReadLines("corpus.txt")))

    let addLetter state letter =
        { state with
              Fragment = state.Fragment + letter
              CurrentPlayer = state.OtherPlayer
              OtherPlayer = state.CurrentPlayer }

    let challenge state =
        match wordlist.Value.Contains(state.Fragment) with
        | true ->
            printf "%s IS a word! " state.Fragment
            { state with
                  Winner = Some(state.CurrentPlayer) }
        | false ->
            printf "%s IS NOT a word! " state.Fragment
            { state with
                  Winner = Some(state.OtherPlayer) }

    let rec advance state =
        let move = state.CurrentPlayer.Prompt(state.Fragment)

        let nextState =
            match move with
            | AddLetter l -> addLetter state l
            | Challenge -> 
                if state.Fragment.Length >= 3 then
                    challenge state
                else
                    printfn "You can't challenge until 3 letters have been written"
                    state
        
        match nextState.Winner with
        | None -> advance nextState
        | Some(winner) -> winner