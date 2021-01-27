open Ghost.HumanPlayer
open Ghost.TriePlayer
open Ghost.Game

[<EntryPoint>]
let main argv =
    let game =
        { Fragment = ""
          State = InProgress
          CurrentPlayer = { HumanPlayer.Name = "Jorge" }
          OtherPlayer = { HumanPlayer.Name = "Valerie" } }

    printfn "To play: Either write a letter or write the word CHALLENGE"
    let winner = advance game
    printfn "Congratulations %s!" winner.Name
    1

// efficient tree building
// 26-tree
// that hellman coding thing?
// binary search on a sorted list?
