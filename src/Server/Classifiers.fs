module Classifiers
open Types
open System
open MathNet.Numerics.Statistics
open MathNet.Numerics.LinearAlgebra

let extractTraingSet (state: State) percent =
    let trainingProbes = state.Features |> Map.toArray |> Array.collect(fun (key, arr) -> arr |> Array.map(fun x -> key, x))
    let take = (percent * (trainingProbes |> Array.length)) / 100
    printfn "Take %A" take
    let trainingset = trainingProbes
                        |> Array.take take
                        |> Array.groupBy(fun (k, _) -> k)
                        |> Array.map(fun (k, x) -> k, x |> Array.map(fun (k, y) -> y))

    let rest = trainingProbes
                        |> Array.skip take
                        |> Array.groupBy(fun (k, _) -> k)
                        |> Array.map(fun (k, x) -> k, x |> Array.map(fun (k, y) -> y))
    { FeaturesCount = 64; Features = trainingset |> Map.ofArray }, { FeaturesCount = 64; Features = rest |> Map.ofArray}

module Enchancments =
    let bootstrap  (state: State) iterations =
        let arr = state.Features |> Map.toArray |> Array.collect(fun (key, arr) -> arr |> Array.map(fun x -> key, x))
        let rnd = Random(iterations)
        printfn "%A" iterations
        let res = seq {
            for _ in [0..iterations] do
                yield arr.[rnd.Next(0, arr.Length - 1)]
        }
        printfn "EEEE %A" res
        let trainingset = res
                            |> Seq.toArray
                            |> Array.groupBy(fun (k, f) -> k)
                            |> Array.map(fun (k, x) -> k, x |> Array.map(fun (k, y) -> y))
        printfn "%A" trainingset
        { FeaturesCount = 64; Features = trainingset |> Map.ofArray }


module KNN =
    let distF (arr1) (arr2) =
        arr1 |> Array.zip arr2 |> Array.map((fun (i, j) -> (i - j) ** 2.0) >> fun x -> Math.Sqrt(x))

    let clssify (distf) k (state: State) (element: float array) =
        let (c, _) = state.Features
                            |> Map.toList
                            |> List.collect(fun (key, f) -> f |> Array.collect(fun x -> distf x element) |> Array.map(fun x -> key, x) |> Array.toList)
                            |> List.sortBy(fun (_, f) -> f)
                            |> List.take(k)
                            |> List.groupBy(fun (k, f) -> k)
                            |> List.maxBy(fun (k, f) -> f |> List.length)
        c

    let nn = clssify distF 1

    let knn = clssify distF

module KNM =

    let clssify k (state: State) (element: float array)  =
        let means = state.Features
                        |> Map.toList
                        |> List.collect(fun (k, f) -> f |> Array.map((fun x -> x |> Statistics.Mean) >> (fun x -> k, x)) |> Array.toList)
        let mean = element |> Statistics.Mean
        let (c, _) = means
                        |> List.map((fun (key, f) -> key, (f - mean) ** 2.0) >> fun (key, x) -> key, Math.Sqrt(x))
                        |> List.sortBy(fun (_, f) -> f)
                        |> List.take(k)
                        |> List.groupBy(fun (k, f) -> k)
                        |> List.maxBy(fun (k, f) -> f |> List.length)
        c
    let nm = clssify 1

    let knm = clssify