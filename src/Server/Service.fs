module Service
open System.IO
open Types
open Database
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open System

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
                        let sizes = keys |> List.map(fun key -> match state.Features.TryFind(key) with | Some k -> k |> List.length | None -> Int32.MaxValue)
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
                                    |> List.indexed
                                    |> List.collect(fun (i, x) -> f2 |> List.indexed |> List.map(fun (j, y) -> (i, j, FisherMath.F (Vector<float>.Build.SparseOfArray(x |> List.toArray)) (Vector<float>.Build.SparseOfArray(y |> List.toArray)) )))
                                    |> List.maxBy(fun (i, j, fisher) -> fisher)
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
                    let size1, size2 = f1 |> List.length, f2 |> List.length
                    let combiantions  = FisherMath.getPossibleCombinations dimension (if size1 > size2 then size1 else size2)
                    return { index = []; value = 0.0 }
                | _ ->
                   return { index = []; value = 0.0 }
            | _ ->
                return { index = []; value = 0.0 }
    }
