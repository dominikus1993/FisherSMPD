namespace Fisher.Common

open System
open MathNet.Numerics.Statistics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Complex

module Probability =
    let getPossibleCombinations m k =
        let list = [0..m] |> List.map(fun x -> [0..m])
        [[1; 2]]

module Fisher =
    let F(matrixA: Vector<float>)(matrixB: Vector<float>)  =
        (Statistics.Mean(matrixA) - Statistics.Mean(matrixB)) / (Statistics.StandardDeviation(matrixA) / Statistics.StandardDeviation(matrixB))

    let getAverageVector(matrix: Matrix<float>) =
        matrix
            |> Matrix.mapRows(fun x v ->
                        let mean = v |> Statistics.Mean
                        v |> Vector.map(fun _ -> mean))

    let getS (matrix: Matrix<float>)(matrixAvg: Matrix<float>) =
        let diff = matrix - matrixAvg
        diff * diff.Transpose()


    let FMD(matrixA: Matrix<float>)(matrixB: Matrix<float>) =
        let ua = matrixA |> getAverageVector
        let ub = matrixB |> getAverageVector
        let diffrenceA = (matrixA - ua)
        let diffrenceB = (matrixB - ub)
        let sa = diffrenceA * diffrenceA.Transpose()
        let sb = diffrenceB * diffrenceB.Transpose()
        2