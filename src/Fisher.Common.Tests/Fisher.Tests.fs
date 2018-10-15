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
        testList "S" [
            testCase "sa" <| fun _ ->
                let matrix = matrix [[ 0.0; 1.0; 1.0; 2.0]
                                     [ -3.0; -2.0; -2.0; -1.0 ]]
                let avgMat = matrix |> Fisher.Common.Fisher.getAverageVector
                let subject = Fisher.Common.Fisher.getS matrix avgMat |> Matrix.toRowArrays |> Array.toSeq
                Expect.sequenceEqual subject ([|[|2.0; 2.0;|];[|2.0; 2.0;|]|]) ""

        ]
    ]