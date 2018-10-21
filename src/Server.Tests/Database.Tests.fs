module Database.Tests
open Expecto
open System.IO
open Database
open Types

[<Tests>]
let testFisher =
    testList "Database" [
        testList "load file" [
            testCaseAsync "Test1" <| async {
                use file = File.OpenRead("./Maple_Oak.txt")
                let! subject = Database.read file
                Expect.equal subject.FeaturesCount 64 ""
                Expect.isTrue (subject.Features |> Map.containsKey "Acer") ""
                Expect.isTrue (subject.Features |> Map.containsKey "Quercus") ""
                Expect.equal ((subject.Features |> Map.tryFind "Acer").Value.Length) 176 ""
                Expect.equal ((subject.Features |> Map.tryFind "Quercus").Value.Length) 608 ""
            }
        ]
]