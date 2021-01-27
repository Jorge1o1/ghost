namespace Ghost

module TriePlayer =
    open Ghost.Model

    type TriePlayer =
        { Name: string }
        interface IPlayer with
            member this.Name = this.Name
            member this.Prompt(fragment) = Challenge
