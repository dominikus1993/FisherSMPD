namespace Fisher.Common

open Hopac
module Database =
    open System.IO

    let readAsync (stream: Stream) =
        async {
            let j = Job.using stream
            return { Features = 64 }
        } |> Async.StartAsTask