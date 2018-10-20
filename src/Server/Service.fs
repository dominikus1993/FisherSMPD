module Service
open System.IO
open Types

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
    let state = 2
    3