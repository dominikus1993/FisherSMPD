namespace Fisher.Common

open Hopac
module Database =
    open System.IO

    let readAsync (stream: Stream) =
        async {
            let j = Job.using stream
            return stream.Length;
        } |> Async.StartAsTask