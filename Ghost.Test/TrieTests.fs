namespace Ghost.Test

open Ghost.Trie
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TrieTests () =
  
    // A Trie with DOG and DO as words
    let dogNode = { word = Some("DOG"); children = Map.empty}
    let doNode = { word = Some("DO"); children = Map.add 'G' dogNode Map.empty}
    let dNode = { word = None; children = Map.add 'O' doNode Map.empty}
    let dogTrie = { word = None; children = Map.add 'D' dNode Map.empty} 

    // A Trie with A, AT, AN, and ANT as words
    let antNode = { word = Some("ANT"); children = Map.empty}
    let anNode = { word = Some("AN"); children = Map.add 'T' antNode Map.empty}
    let atNode = { word = Some("AT"); children = Map.empty }
    let aNode = { word = Some("A"); children = Map.add 'T' atNode (Map.add 'N' anNode Map.empty) }
    let antTrie = { word = None; children = Map.add 'A' aNode Map.empty}

    // A trie with ILL as the only word
    let illNode = { word = Some("ILL"); children = Map.empty }
    let ilNode = { word = None; children = Map.add 'L' illNode Map.empty }
    let iNode = { word = None; children = Map.add 'L' ilNode Map.empty }
    let illTrie = { word = None; children = Map.add 'I' iNode Map.empty }

    [<TestMethod>]
    member this.TestInsertSingleLetterIntoEmpty () =
        let terminal = { word = Some("A"); children = Map.empty }
        let branch = { word = None; children = Map.add 'A' terminal Map.empty }
        let actual = insert "A" empty
        Assert.AreEqual(branch, actual)

    [<TestMethod>]
    member this.TestInsertDoThenDog () =
        let actual =
            insert "DO" empty
            |> insert "DOG"
        Assert.AreEqual(dogTrie, actual)
    
    [<TestMethod>]
    member this.TestInsertDogThenDo () =
        let actual =
            insert "DOG" empty
            |> insert "DO"
        Assert.AreEqual(dogTrie, actual)
    
    [<TestMethod>]
    member this.TestInsertAntWords () =
        let actual = 
            insert "ANT" empty
            |> insert "AT"
            |> insert "AN"
            |> insert "A"
        Assert.AreEqual(antTrie, actual)

    [<TestMethod>]
    member this.TestAnyDescendantOfAnShouldBeWord () =
        let isWord node = node.word.IsSome 
        Assert.IsTrue(anyDescendant anNode isWord)

    [<TestMethod>]
    member this.TestNoDescendantOfAntShouldBeWord () =
        let isWord node = node.word.IsSome 
        Assert.IsFalse(anyDescendant atNode isWord)

    [<TestMethod>]
    member this.TestAllAncestorsOfAntShouldBeWord () =
        let isWord node = node.word.IsSome 
        let chars = Seq.toList "ANT"
        let opt = canWalkAndAll chars antTrie isWord
        Assert.IsTrue(opt.IsSome)

    [<TestMethod>]
    member this.TestNoAncestorOfIllIsWord () =
        let isNotWord node = node.word.IsNone 
        let chars = Seq.toList "IL" // Note, canWalkAndAll includes the last
        let opt = canWalkAndAll chars illTrie isNotWord
        Assert.IsTrue(opt.IsSome)

    [<TestMethod>]
    member this.TestIlIsValidFragment () =
        Assert.IsTrue(isValidFragment "IL" illTrie)

    [<TestMethod>]
    member this.TestDogIsNotValidFragment () =
        Assert.IsFalse(isValidFragment "DOG" dogTrie)
    
    [<TestMethod>]
    member this.TestDoIsNotValidFragment () =
        Assert.IsFalse(isValidFragment "DO" dogTrie)

    [<TestMethod>]
    member this.TestDigIsNotValidFragment () =
        Assert.IsFalse(isValidFragment "DIG" dogTrie)