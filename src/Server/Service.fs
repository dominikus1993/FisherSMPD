module Service
open System.IO
open Types
open Database
open FisherMath
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open System
open Hopac.Extensions
open Hopac
open System.Linq
open Hopac.Extensions
open Shared

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

let uploadDatabaseFile (stream: Stream) =
    async {
        let! state = read stream
        Agent.Post(Store(state))
    }

let getFisherFactor dimension mode =
    async {
        let! state = Agent.PostAndAsyncReply(fun ch -> Get(ch))
        let keys = state.Features |> Map.toList |> List.map(fun (k, _) -> k) |> List.take 2
        let! possibleDimensions = Agent.PostAndAsyncReply(fun ch -> GetPossibleDimension(ch))
        match keys with
            | [first; second] ->
               match state.Features |> Map.tryFind(first), state.Features |> Map.tryFind(second) with
               | Some(f1), Some(f2) ->
                    let matrix1, matrix2 = matrix f1 |> Matrix.transpose, matrix f2 |> Matrix.transpose
                    match mode with
                    | Fisher ->
                        if dimension = 1 then
                            let (i, j, f) =
                                seq {
                                    for ((i, m1), (j, m2)) in matrix1.ToRowArrays() |> Array.indexed |> Array.zip(matrix2.ToRowArrays() |> Array.indexed) do
                                        yield (i, j, FisherMath.F (vector m1) (vector m2) )
                                } |> Seq.maxBy(fun (_, _, fisher) -> fisher)
                            return { index = [i] ; value = f }
                        else
                            let mean1, mean2 = matrix1 |> FisherMath.getAverageVector, matrix2 |> FisherMath.getAverageVector
                            let combinations = getPossibleCombinations dimension possibleDimensions |> Seq.toList
                            let matrixCombinations1 = combinations |> List.map(fun x -> struct (x, buildArrayFromListOfIndexes matrix1 x, buildArrayFromListOfIndexes mean1 x)) |> Seq.toList
                            let matrixCombinations2 = combinations |> List.map(fun x -> struct (x, buildArrayFromListOfIndexes matrix2 x, buildArrayFromListOfIndexes mean2 x)) |> Seq.toList
                            let struct (i, _, f) = matrixCombinations1 |> List.zip(matrixCombinations2) |> List.map(fun (struct (c1, ma1, m1), struct (c2, ma2, m2)) -> struct (c1, c2, FisherMath.FMD ma1 m1 ma2 m2)) |> List.maxBy(fun struct (_, _, f) -> f)
                            return { index =  i; value = f }
                    | Sfs ->
                        return FisherMath.sfs matrix1 matrix2 dimension possibleDimensions
               | _ ->
                  return { index = []; value = 0.0 }
            | _ ->
                return { index = []; value = 0.0 }
    }
