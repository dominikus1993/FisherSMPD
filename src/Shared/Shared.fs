namespace Shared

open System.IO
type Counter = int

type FisherResponse = { index: (int * int) list; value: float }

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type ICounterApi =
    { initialCounter : unit -> Async<Counter>; uploadDatabase: byte[] -> Async<unit>  }


