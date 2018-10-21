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

let uploadDatabaseFile (stream: Stream) =
    async {
        let! state = read stream
        Agent.Post(Store(state))
    }

let getFisherFactor dimension =
    async {
        let! state = Agent.PostAndAsyncReply(fun ch -> Get(ch))
        if dimension = 1 then
            let keys = state.Features |> Map.toList |> List.map(fun (k, _) -> k)
            3.
        else
            3.
    }
