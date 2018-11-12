module Classifiers
open Types
open System
open MathNet.Numerics.LinearAlgebra.VectorExtensions
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.Statistics
open MathNet.Numerics.Statistics

let extractTraingSet (state: State) percent=
    let trainingProbes = state.Features |> Map.toList |> List.sortBy(fun _ -> Guid.NewGuid())
    let take = (percent * (trainingProbes |> List.length)) / 100
    { FeaturesCount = 64; Features = trainingProbes |> List.take take |> Map.ofList }, { FeaturesCount = 64; Features = trainingProbes |> List.skip take |> Map.ofList}

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