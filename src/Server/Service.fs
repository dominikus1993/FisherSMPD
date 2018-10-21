module Service
open System.IO
open Types
open Database
open System.Numerics
open MathNet.Numerics.LinearAlgebra

type Msg =
    | Store of state: State
    | Get of AsyncReplyChannel<State>

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
        if dimension = 1 then
            let keys = state.Features |> Map.toList |> List.map(fun (k, _) -> k) |> List.take 2
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
            return { index = []; value = 0.0 }
    }
