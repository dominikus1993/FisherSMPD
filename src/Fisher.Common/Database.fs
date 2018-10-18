namespace Fisher.Common

open FSharp.Control
module Database =
    open System.Text.RegularExpressions
    open System.IO
    open System.Linq
    open System


    type LineType =
        | MetaData of numberOfFeatures: int
        | FeatureData of name: string * features: int seq
        | Nothing

    let (|MetaData|FeatureData|Nothing|) input =
        if String.IsNullOrEmpty input then
            Nothing
        else
            let firstLineMatch = Regex.Match(input, """^(\d+)""")
            let nextMatch = Regex.Matches(input ,"^(\w+ \w+)|(\d\.\d+|\d+)")
            if firstLineMatch.Success then
                MetaData(Int32.Parse(firstLineMatch.Value))
            elif nextMatch.Count > 0 then
                let name = nextMatch.[0]
                let rest = nextMatch.Cast<Match>() |> Seq.skip 1 |> Seq.map(fun x -> Double.Parse(x.Value)) |> Seq.toList
                FeatureData(name.Value.Split(' ').[0], rest)
            else
                Nothing

    let read (stream: Stream) =
        // let result = Seq.unfold (fun _ ->
        //                         let lineT = reader.ReadLineAsync()
        //                         lineT.Wait()
        //                         Some(lineT.Result, ())
        //                     ) ()
        //             |> Seq.take 10000
        //             |> Seq.fold(fun acc x ->  match x with
        //                                       | MetaData(d) ->
        //                                           { acc with FeaturesCount = d}
        //                                       | FeatureData(n, values) ->
        //                                           match acc.Features.TryFind(n) with
        //                                           | Some(features) ->
        //                                               { acc with Features = acc.Features |> Map.add n (values::features)}
        //                                           | None ->
        //                                               { acc with Features = acc.Features |> Map.add n [values]}
        //                                       | _ -> acc) { FeaturesCount  = 0; Features = [] |> Map.ofList }
        async {
            let reader = new StreamReader(stream)
            let asyncTasks = Seq.unfold (fun _ ->
                                let lineT = reader.ReadLineAsync() |> Async.AwaitTask
                                Some(lineT, ())) ()
            let! num = asyncTasks |> Async.Parallel
            return num;
        } |> Async.StartAsTask
