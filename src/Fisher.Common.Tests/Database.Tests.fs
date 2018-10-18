module Database
open Expecto
open System.IO
open Fisher.Common


[<Tests>]
let testFisher =
    testList "Database" [
        testList "load file" [
            testCase "Test1" <| fun _ ->
                use file = File.OpenRead("./Maple_Oak.txt")
                let subject = Database.read file
                Expect.equal subject.FeaturesCount 64 ""
                Expect.isTrue (subject.Features |> Map.containsKey "Acer") ""
                Expect.isTrue (subject.Features |> Map.containsKey "Quercus") ""
        ]
    ]
