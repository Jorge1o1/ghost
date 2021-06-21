open Ghost.HumanPlayer
open Ghost.AIPlayer
open Ghost.Game

[<EntryPoint>]
let main argv =
    let game =
        { Fragment = ""
          Winner = None
          CurrentPlayer = { AIPlayer.Name = "QT-1" }
          OtherPlayer = { HumanPlayer.Name = "Jorge" } }

    printfn "To play: Either write a letter or write the word CHALLENGE"
    let winner = loop game
    printfn "Congratulations %s!" winner.Name
    1

// efficient tree building
// 26-tree
// that hellman coding thing?
// binary search on a sorted list?
