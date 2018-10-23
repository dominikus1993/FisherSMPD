module Service
open System.IO
open Types
open Database
open FisherMath
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open System
open Hopac.Extensions
open Hopac.Extensions

type Msg =
    | Store of state: State
    | Get of AsyncReplyChannel<State>
    | GetPossibleDimension of AsyncReplyChannel<int>

let Agent =
    MailboxProcessor.Start(fun inbox ->
        let rec loop state =
            async {
                match! inbox.Receive() with
                | Store s ->
                    return! loop s
                | Get reply ->
                    reply.Reply(state)
                    return! loop state
                | GetPossibleDimension reply ->
                    reply.Reply(state.FeaturesCount)
                    return! loop state
             }
        loop { FeaturesCount = 0; Features = [] |> Map.ofList })

type FisherResponse = { index: (int * int) list; value: float }

let uploadDatabaseFile (stream: Stream) =
    async {
        let! state = read stream
        Agent.Post(Store(state))
    }

let getFisherFactor dimension =
    async {
        let! state = Agent.PostAndAsyncReply(fun ch -> Get(ch))
        let keys = state.Features |> Map.toList |> List.map(fun (k, _) -> k) |> List.take 2
        let! possibleDimensions = Agent.PostAndAsyncReply(fun ch -> GetPossibleDimension(ch))
        if dimension = 1 then
           match keys with
           | [first; second] ->
               match state.Features |> Map.tryFind(first), state.Features |> Map.tryFind(second) with
               | Some(f1), Some(f2) ->
                   let (i, j, f) = f1
                                   |> Array.indexed
                                   |> Array.collect(fun (i, x) -> f2 |> Array.indexed |> Array.map(fun (j, y) -> (i, j, FisherMath.F (vector x) (vector y) )))
                                   |> Array.maxBy(fun (_, _, fisher) -> fisher)
                   return { index = [(i, j)] ; value = f }
               | _ ->
                  return { index = []; value = 0.0 }
           | _ ->
                return { index = []; value = 0.0 }
        else
           match keys with
           | [first; second] ->
               match state.Features |> Map.tryFind(first), state.Features |> Map.tryFind(second) with
               | Some(f1), Some(f2) ->
                    let matrix1, matrix2 = matrix f1 |> Matrix.transpose, matrix f2 |> Matrix.transpose
                    let mean1, mean2 = matrix1 |> FisherMath.getAverageVector, matrix2 |> FisherMath.getAverageVector
                    let combinations = getPossibleCombinations dimension possibleDimensions |> Seq.toList
                    let matrixCombinations1 = combinations |> List.map(fun x -> x, buildArrayFromListOfIndexes matrix1 x, buildArrayFromListOfIndexes mean1 x) |> Seq.toList
                    let matrixCombinations2 = combinations |> List.map(fun x -> x, buildArrayFromListOfIndexes matrix2 x, buildArrayFromListOfIndexes mean2 x) |> Seq.toList
                    let struct (i, j, f) = matrixCombinations1 |> List.collect(fun (c1, ma1, m1) -> matrixCombinations2 |> List.map(fun (c2, ma2, m2) -> struct (c1, c2, FisherMath.FMD ma1 m1 ma2 m2))) |> List.maxBy(fun struct (_, _, f) -> f)
                    return { index = List.zip i j |> List.map(fun (x, y) -> x, y); value = f }
               | _ ->
                    return { index = []; value = 0.0 }
           | _ ->
                return { index = []; value = 0.0 }
    }
