namespace Fisher.Common

open System
open MathNet.Numerics.Statistics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Complex

module Fisher =
    let F(matrixA: double seq)(matrixB: double seq)  =
        (Statistics.Mean(matrixA) - Statistics.Mean(matrixB)) / (Statistics.StandardDeviation(matrixA) / Statistics.StandardDeviation(matrixB))

    let getAverageVector(matrix: Matrix<float>) =
        matrix
            |> Matrix.mapRows(fun x v ->
                        let mean = v |> Statistics.Mean
                        v |> Vector.map(fun _ -> mean))

    let FMD(matrixA: Matrix<float>)(matrixB: Matrix<float>) =
        let ua = matrixA
                    |> Matrix.mapRows(fun x v ->
                                let mean = v |> Statistics.Mean
                                v |> Vector.map(fun _ -> mean))
        let ub = matrixB
                    |> Matrix.mapRows(fun x v ->
                                let mean = v |> Statistics.Mean
                                v |> Vector.map(fun _ -> mean))
        2
