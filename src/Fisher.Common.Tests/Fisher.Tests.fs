module Fisher
open Expecto
open MathNet.Numerics.LinearAlgebra

[<Tests>]
let testFisher =
    testList "Fisher" [
        testList "average vector" [
            testCase "Test1" <| fun _ ->
                let m = matrix [[ 2.0; 3.0; 4.0 ]
                                [ 3.0; 4.0; 5.0 ]]
                let subject = m |> Fisher.Common.Fisher.getAverageVector
                let arrays = subject |> Matrix.toRowArrays |> Array.toSeq
                Expect.sequenceEqual arrays ([|[|3.0; 3.0; 3.0|];[|4.0; 4.0; 4.0|]|]) ""
        ]
    ]