module FisherMath

open System
open MathNet.Numerics.Statistics
open MathNet.Numerics.LinearAlgebra

let getPossibleCombinations m n =
    let rec fC prefix m from = seq {
        let rec loopFor f = seq {
            match f with
            | [] -> ()
            | x::xs ->
                yield (x, fC [] (m-1) xs)
                yield! loopFor xs
        }
        if m = 0 then yield prefix
        else
            for (i, s) in loopFor from do
                for x in s do
                    yield prefix@[i]@x
    }
    fC [] m [0..(n-1)]

let buildArrayFromListOfIndexes (m: Matrix<_>) (indexes: int list) =
    let res = seq {
                    for x in indexes do
                    yield m.Row(x).ToArray()
                } |> Seq.toArray
    matrix res

let F(matrixA: Vector<float>)(matrixB: Vector<float>)  =
    ((Statistics.Mean(matrixA) - Statistics.Mean(matrixB)) |> Math.Abs) / (Statistics.StandardDeviation(matrixA) + Statistics.StandardDeviation(matrixB))

let getAverageVector(matrix: Matrix<float>) =
    matrix
        |> Matrix.mapRows(fun x v ->
                    let mean = v |> Statistics.Mean
                    v |> Vector.map(fun _ -> mean))

let getCovarianceMatrix (matrix: Matrix<float>)(matrixAvg: Matrix<float>) =
    let diff = matrix - matrixAvg
    (1. / (matrix.ColumnCount |> float)) * (diff * diff.Transpose())

let distance (u1: Vector<float>) (u2: Vector<float>) =
    let res = u1 |> Vector.fold2(fun acc y z -> Math.Pow(y - z, 2.0) + acc) (0.0) u2
    Math.Sqrt(res)

let FMD(matrixA: Matrix<float>)(ua: Matrix<float>)(matrixB: Matrix<float>)(ub: Matrix<float>) =
    let sa = getCovarianceMatrix matrixA ua
    let sb = getCovarianceMatrix matrixB ub
    let det = (sa + sb) |> Matrix.determinant
    let dist = ua.Column(0) |> distance (ub.Column(0))
    dist / det