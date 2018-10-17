namespace Fisher.Common

module Database =
    open System.IO

    let readAsync (stream: Stream) =
        async {
            return stream.Length;
        } |> Async.StartAsTask