namespace Ghost

module AIPlayer =
    open Ghost.Game

    /// Heuristic for minimax
    let evalFragment fragment = 10.0 ** float((String.length fragment))

    let rec minimax depth state aiPlayer =
        if depth < 1 then raise (System.ArgumentOutOfRangeException())
        
        let evalWinner winner  = if winner = aiPlayer then 100.0 else -100.0 
        let rec minimaxHelper depth maximize state move =
            match state.Winner with
            | Some winner -> (evalWinner winner, move)
            | None when depth = 0 -> (0.0, move)
            | None ->
                let moves = legalMoves state
                let possibleStates = List.map (advance state) moves
                let moveValuePairs = List.map2 (minimaxHelper (depth-1) (not maximize)) possibleStates moves
                if maximize then
                    let best = (Seq.map (fun (v, m) -> v) moveValuePairs) |> Seq.max
                    (best, move)
                else
                    let best = (Seq.map (fun (v, m) -> v) moveValuePairs) |> Seq.min
                    (best, move)
        
        let moves = legalMoves state
        let possibleStates = List.map (advance state) moves
        let moveValuePairs = List.map2 (minimaxHelper (depth-1) (false)) possibleStates moves
        let (best, move) = Seq.maxBy (fun (v, m) -> v) moveValuePairs
        printfn "Fragment %s / Depth %i / Maximizing %b / Best Move %f %A" state.Fragment depth true best move
        (best, move)


    type AIPlayer =
        { Name: string }

        member this.Prompt(state) =
            let (v, m) = minimax 5 state this
            m
        
        interface Game.IPlayer with
            member this.Name = this.Name
            member this.Prompt(state) = this.Prompt(state)
