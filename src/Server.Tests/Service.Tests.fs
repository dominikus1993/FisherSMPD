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
                Expect.equal subject.index [(30, 30)] ""
                Expect.floatClose Accuracy.low subject.value 0.741833 ""
            }
            testCaseAsync "Test for two dimension" <| async {
                use file = File.OpenRead("./Maple_Oak.txt")
                do! Service.uploadDatabaseFile file
                let! subject = Service.getFisherFactor 2
                Expect.equal subject.index [(15, 15); (33, 33)] ""
                Expect.floatClose Accuracy.low subject.value 67458992.995955 ""
           }
        ]
]