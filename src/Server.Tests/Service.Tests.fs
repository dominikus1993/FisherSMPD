module Service.Tests
open Expecto
open System.IO
open Shared

[<Tests>]
let testFisher =
    testList "Service" [
        testList "getFisherFactor" [
            testCaseAsync "Test for one dimension" <| async {
                use file = File.OpenRead("./Maple_Oak.txt")
                do! Service.uploadDatabaseFile file
                let! subject = Service.getFisherFactor 1 Fisher
                Expect.equal subject.index [(30)] ""
                Expect.floatClose Accuracy.low subject.value 0.741833 ""
            }
            testCaseAsync "Test for two dimension" <| async {
                use file = File.OpenRead("./Maple_Oak.txt")
                do! Service.uploadDatabaseFile file
                let! subject = Service.getFisherFactor 2 Fisher
                Expect.equal subject.index [(15); (33)] ""
                Expect.floatClose Accuracy.low subject.value 67458992.995955 ""
           }
        ]
        testList "sfs" [
            testCaseAsync "Test for one dimension" <| async {
                use file = File.OpenRead("./Maple_Oak.txt")
                do! Service.uploadDatabaseFile file
                let! subject = Service.getFisherFactor 1 Sfs
                Expect.equal subject.index [(30)] ""
                Expect.floatClose Accuracy.low subject.value 0.741833 ""
            }
            testCaseAsync "Test for two dimension" <| async {
                use file = File.OpenRead("./Maple_Oak.txt")
                do! Service.uploadDatabaseFile file
                let! subject = Service.getFisherFactor 2 Sfs
                Expect.equal subject.index [(15); (30)] ""
                Expect.floatClose Accuracy.low subject.value 30043623.226422 ""
           }
        ]
    ]