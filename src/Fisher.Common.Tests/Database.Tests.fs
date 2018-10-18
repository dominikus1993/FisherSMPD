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
                let! subject = Database.readAsync file |> Async.AwaitTask
                Expect.equal subject.Features 64 ""
            }
        ]
    ]
