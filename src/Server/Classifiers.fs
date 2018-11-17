module Classifiers
open Types
open System
open MathNet.Numerics.Statistics
open MathNet.Numerics.LinearAlgebra
open Types

let extractTraingSet (state: State) percent =
    let take = (percent * (state.Objects |> Array.length)) / 100
    printfn "Take %A" take
    let trainingset = state.Objects
                        |> Array.take take
    let rest = state.Objects
                        |> Array.skip take
    { FeaturesCount = 64; Objects = trainingset }, { FeaturesCount = 64; Objects = rest }

module Enchancments =
    let bootstrap  (state: State) iterations =
        let arr = state.Objects
        let rnd = Random(iterations)
        let res = seq {
            for _ in [0..iterations] do
                yield arr.[rnd.Next(0, arr.Length - 1)]
        }
        { FeaturesCount = 64; Objects = res |> Seq.toArray }


module KNN =
    let distF (arr1) (arr2) =
        arr1 |> Array.zip arr2 |> Array.map((fun (i, j) -> (i - j) ** 2.0) >> fun x -> Math.Sqrt(x)) |> Array.sum

    let clssify (distf: float array -> float array -> float) k (trainingState: State) (element: Object) =
        let (c, _) = trainingState.Objects
                            |> Array.map(fun o -> distf o.Features element.Features, o.ClassName)
                            |> Array.sortBy(fun (dist, _) -> dist)
                            |> Array.take(k)
                            |> Array.groupBy(fun (_, k) -> k)
                            |> Array.maxBy(fun (_, f) -> f |> Array.length)
        c

    let nn = clssify distF 1

    let knn = clssify distF

module KNM =

    let clssify k (state: State) (element: Object)  =
        let means = state.Objects
                        |> Array.map(fun o -> o.ClassName, o.Features |> Statistics.Mean)
        let mean = element.Features |> Statistics.Mean
        let (c, _) = means
                        |> Array.map((fun (key, f) -> key, (f - mean) ** 2.0) >> fun (key, x) -> key, Math.Sqrt(x))
                        |> Array.sortBy(fun (_, f) -> f)
                        |> Array.take(k)
                        |> Array.groupBy(fun (k, f) -> k)
                        |> Array.maxBy(fun (k, f) -> f |> Array.length)
        c
    let nm = clssify 1

    let knm = clssify