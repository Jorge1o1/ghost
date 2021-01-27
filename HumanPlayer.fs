namespace Ghost

module HumanPlayer =
    open System
    open System.Text.RegularExpressions
    open Ghost.Model

    type HumanPlayer =
        { Name: string }

        member this.Prompt(fragment) =
            printf "%s : %s : " this.Name fragment
            let input = Console.ReadLine()

            match input.ToUpper() with
            | x when Regex.Match(x, "^[A-Z]$").Success -> Letter(x)
            | "CHALLENGE" -> Challenge
            | _ ->
                printfn "%s: Bad Input" this.Name
                this.Prompt(fragment)

        interface IPlayer with
            member this.Name = this.Name
            member this.Prompt(fragment) = this.Prompt(fragment)
