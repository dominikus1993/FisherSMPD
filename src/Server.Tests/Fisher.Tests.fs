module Fisher.Tests
open Expecto
open MathNet.Numerics.LinearAlgebra
open FisherMath

[<Tests>]
let testFisher =
    testList "Fisher" [
        testList "F" [
            testCase "test1" <| fun _ ->
                let subject = FisherMath.F (vector [0.0;1.0;1.0;2.0]) (vector [1.0; 1.0;1.0;1.0])
                Expect.equal subject 0. ""
            testCase "test2" <| fun _ ->
                let subject = FisherMath.F (vector [0.;1.;1.;2.]) (vector [1.;2.;2.;2.])
                Expect.floatClose Accuracy.low subject 0.569694 ""
        ]
        testList "average vector" [
            testCase "Test1" <| fun _ ->
                let m = matrix [[ 2.0; 3.0; 4.0 ]
                                [ 3.0; 4.0; 5.0 ]]
                let subject = m |> FisherMath.getAverageVector
                let arrays = subject |> Matrix.toRowArrays |> Array.toSeq
                Expect.sequenceEqual arrays ([|[|3.0; 3.0; 3.0|];[|4.0; 4.0; 4.0|]|]) ""
        ]
        testList "S" [
            testCase "sa" <| fun _ ->
                let matrix = matrix [[ 0.0; 1.0; 1.0; 2.0]
                                     [ -3.0; -2.0; -2.0; -1.0 ]]
                let avgMat = matrix |> FisherMath.getAverageVector
                let subject = FisherMath.getCovarianceMatrix matrix avgMat |> Matrix.toRowArrays |> Array.toSeq
                Expect.sequenceEqual subject ([|[|1.0; 1.0;|];[|1.0; 1.0;|]|]) ""

        ]
    ]

[<Tests>]
let testProb =
    testList "Probability" [
        testList "getPossibleCombinations" [
            testCase "Test combinations" <| fun _ ->
                let subject = FisherMath.getPossibleCombinations 2 4
                Expect.sequenceEqual subject [[0;1]; [0;2]; [0;3]; [1;2]; [1;3]; [2;3]] ""
        ]
]