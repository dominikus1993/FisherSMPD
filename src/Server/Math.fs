module FisherMath

open System
open MathNet.Numerics.Statistics
open MathNet.Numerics.LinearAlgebra
open Shared

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

let sfsCombinations d featureCount indexes =
    let len = d - (indexes |> List.length)
    getPossibleCombinations d featureCount |> Seq.filter(fun x -> (Set.ofList x) - (Set.ofList indexes) |> Set.count = len) |> Seq.toList

let sfs (matrixA: Matrix<float>)(matrixB: Matrix<float>)(dimension: int)(featureCount: int) =
    let mean1, mean2 = matrixA |> getAverageVector, matrixB |> getAverageVector
    let rec f (m1: Matrix<float>)(m2: Matrix<float>) d fisher =
        if d = 1 then
            let (fisher, index) = m1.ToRowArrays() |> Array.zip (m2.ToRowArrays() |> Array.indexed) |> Array.map(fun ((index, x1), x2) -> F (vector x1)(vector x2), index) |> Array.maxBy(fun (f, _) -> f)
            f m1 m2 (d + 1) ({ index = [index]; value = fisher })
        elif d > 1 && d <= dimension then
            let combinations = sfsCombinations d featureCount fisher.index
            let matrixCombinations1 = combinations |> List.map(fun x -> struct (x, buildArrayFromListOfIndexes m1 x, buildArrayFromListOfIndexes mean1 x)) |> Seq.toList
            let matrixCombinations2 = combinations |> List.map(fun x -> struct (x, buildArrayFromListOfIndexes m2 x, buildArrayFromListOfIndexes mean2 x)) |> Seq.toList
            let struct (i, _, fisher) = matrixCombinations1 |> List.zip(matrixCombinations2) |> List.map(fun (struct (c1, ma1, m1), struct (c2, ma2, m2)) -> struct (c1, c2, FMD ma1 m1 ma2 m2)) |> List.maxBy(fun struct (_, _, f) -> f)
            f m1 m2 (d + 1) ({ index = i; value = fisher })
        else
            fisher
    f matrixA matrixB 1 { index = []; value = 0.0 }


let fs (matrixA: Matrix<float>)(matrixB: Matrix<float>)(dimension: int)(featureCount: int) =
    if dimension = 1 then
        let (i, j, f) =
            seq {
                for ((i, m1), (j, m2)) in matrixA.ToRowArrays() |> Array.indexed |> Array.zip(matrixB.ToRowArrays() |> Array.indexed) do
                    yield (i, j, F (vector m1) (vector m2) )
            } |> Seq.maxBy(fun (_, _, fisher) -> fisher)
        { index = [i] ; value = f }
    else
        let mean1, mean2 = matrixA |> getAverageVector, matrixB |> getAverageVector
        let combinations = getPossibleCombinations dimension featureCount |> Seq.toList
        let matrixCombinations1 = combinations |> List.map(fun x -> struct (x, buildArrayFromListOfIndexes matrixA x, buildArrayFromListOfIndexes mean1 x)) |> Seq.toList
        let matrixCombinations2 = combinations |> List.map(fun x -> struct (x, buildArrayFromListOfIndexes matrixB x, buildArrayFromListOfIndexes mean2 x)) |> Seq.toList
        let struct (i, _, f) = matrixCombinations1 |> List.zip(matrixCombinations2) |> List.map(fun (struct (c1, ma1, m1), struct (c2, ma2, m2)) -> struct (c1, c2, FMD ma1 m1 ma2 m2)) |> List.maxBy(fun struct (_, _, f) -> f)
        { index =  i; value = f }