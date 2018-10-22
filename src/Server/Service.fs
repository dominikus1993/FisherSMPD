module Service
open System.IO
open Types
open Database
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open System
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
                    match state.Features |> Map.toList |> List.map(fun (k, _) -> k) with
                    | keys ->
                        let sizes = keys |> List.map(fun key -> match state.Features.TryFind(key) with | Some k -> k |> Array.length | None -> Int32.MaxValue)
                        reply.Reply(sizes |> List.min)
                    | _ ->
                        reply.Reply(0)
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
                   let size1, size2 = f1 |> Array.length, f2 |> Array.length
                   let smallerSize = if size1 > size2 then size2 else size1
                   let combiantions1  = FisherMath.getPossibleCombinations dimension smallerSize |> Seq.map(fun i -> i, matrix (FisherMath.buildArrayFromListOfIndexes f1 i)) |> Seq.toList
                   let combinations2 =  FisherMath.getPossibleCombinations dimension smallerSize |> Seq.map(fun i -> i, matrix (FisherMath.buildArrayFromListOfIndexes f2 i)) |> Seq.toList
                   let (i, j, fisher) = combiantions1
                                           |> List.collect(fun (i, arr1) -> combinations2 |> List.map(fun (j, arr2) ->
                                                                                               (i, j, FisherMath.FMD (arr1) (arr2))
                                                                                           ))
                                           |> List.maxBy(fun (_, _, fisher) -> fisher)
                   return { index = i |> List.zip j; value = fisher }
               | _ ->
                  return { index = []; value = 0.0 }
           | _ ->
                return { index = []; value = 0.0 }
    }
