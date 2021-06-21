namespace Ghost

module HumanPlayer =
    open System
    open System.Text.RegularExpressions
    open Ghost.Game

    type HumanPlayer =
        { Name: string }

        member this.Prompt(state) =
            printf "%s : %s : " this.Name state.Fragment
            let input = Console.ReadLine()

            match input.ToUpper() with
            | x when Regex.Match(x, "^[A-Z]$").Success -> AddLetter(x)
            | "CHALLENGE" -> Challenge
            | _ ->
                printfn "%s: Bad Input" this.Name
                this.Prompt(state)

        interface IPlayer with
            member this.Name = this.Name
            member this.Prompt(state) = this.Prompt(state)
