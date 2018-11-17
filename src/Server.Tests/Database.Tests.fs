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
                Expect.isTrue (subject.Objects |> Array.filter(fun o -> o.ClassName = "Acer") |> Array.length = 176) ""
                Expect.isTrue (subject.Objects |> Array.filter(fun o -> o.ClassName = "Quercus") |> Array.length = 608) ""
                Expect.equal (subject.Objects |> Array.filter(fun o -> o.Features |> Array.length = 64) |> Array.length) (608 + 176) ""
            }
        ]
]