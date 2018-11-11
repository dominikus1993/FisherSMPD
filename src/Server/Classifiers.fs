module Classifiers
open Types
open System

let extractTraingSet (state: State) percent=
    let trainingProbes = state.Features |> Map.toList |> List.sortBy(fun _ -> Guid.NewGuid())
    let take = (percent * (trainingProbes |> List.length)) / 100
    { FeaturesCount = 64; Features = trainingProbes |> List.take take |> Map.ofList }, { FeaturesCount = 64; Features = trainingProbes |> List.skip take |> Map.ofList}