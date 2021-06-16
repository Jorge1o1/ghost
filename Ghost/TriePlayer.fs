namespace Ghost

module TriePlayer =
    open Ghost.Game

    type TriePlayer =
        { Name: string }
        interface IPlayer with
            member this.Name = this.Name
            member this.Prompt(fragment) = Challenge
