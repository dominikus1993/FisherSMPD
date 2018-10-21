module Service.Tests
open Expecto
open System.IO
open Database
open Types
open Service

[<Tests>]
let testFisher =
    testList "Service" [
        testList "getFisherFactor" [
            testCaseAsync "Test for one dimension" <| async {
                use file = File.OpenRead("./Maple_Oak.txt")
                do! Service.uploadDatabaseFile file
                let! subject = Service.getFisherFactor 1
                Expect.equal subject.index 2 ""
                Expect.equal subject.value 12. ""
            }
        ]
]