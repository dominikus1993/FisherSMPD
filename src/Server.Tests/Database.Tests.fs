module Database
open Expecto
open System.IO
open Fisher.Common


[<Tests>]
let testFisher =
    testList "Database" [
        testList "load file" [
            testCaseAsync "Test1" <| async {
                use file = File.OpenRead("./Maple_Oak.txt")
                let! subject = Database.read file |> Async.AwaitTask
                Expect.equal subject.FeaturesCount 64 ""
                Expect.isTrue (subject.Features |> Map.containsKey "Acer") ""
                Expect.isTrue (subject.Features |> Map.containsKey "Quercus") ""
            }
        ]
]