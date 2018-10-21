module Service
open System.IO
open Types
open Database

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

type FisherResponse = { index: int; value: float }

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
                match state.Features.TryFind(first), state.Features.TryFind(second) with
                | Some(f1), Some(f2) ->

                    return { index = -1; value = 0.0 }
                | _ ->
                   return { index = -1; value = 0.0 }
            | _ ->
                return { index = -1; value = 0.0 }
        else
            return { index = -1; value = 0.0 }
    }
