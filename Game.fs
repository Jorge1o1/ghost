namespace Ghost

module Game =
    open Ghost.Model

    type GameState =
        | InProgress
        | Winner of IPlayer

    type Game =
        { Fragment: string
          State: GameState
          CurrentPlayer: IPlayer
          OtherPlayer: IPlayer }

    let wordlist =
        lazy (Set(System.IO.File.ReadLines("corpus.txt")))

    let addLetter game letter =
        { game with
              Fragment = game.Fragment + letter
              CurrentPlayer = game.OtherPlayer
              OtherPlayer = game.CurrentPlayer }

    let rec advance game =
        let move = game.CurrentPlayer.Prompt(game.Fragment)

        let next =
            match move with
            | Letter l -> addLetter game l
            | Challenge ->
                match wordlist.Value.Contains(game.Fragment) with
                | true ->
                    { game with
                          State = Winner(game.CurrentPlayer) }
                | false ->
                    { game with
                          State = Winner(game.OtherPlayer) }

        match next.State with
        | InProgress -> advance next
        | Winner w -> w
