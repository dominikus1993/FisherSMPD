namespace Fisher.Common

open Hopac
module Database =
    open System.Text.RegularExpressions
    open System.IO
    open System.Linq
    open System

    open Hopac
    open Hopac

    open System.IO
    type LineType = 
        | MetaData of numberOfFeatures: int
        | FeatureData of name: string * features: int seq
        | None 
    
    let (|MetaData|FeatureData|None|) input = 
        let firstLineMatch = Regex.Match(input, """^(\d+)""")
        let nextMatch = Regex.Matches(input ,"^(\w+ \w+)|(\d\.\d+|\d+)")
        if firstLineMatch.Success then
            MetaData(Int32.Parse(firstLineMatch.Value))
        elif nextMatch.Count > 0 then
            let name = nextMatch.[0]
            let rest = nextMatch.Cast<Match>() |> Seq.skip 1 |> Seq.map(fun x -> Int32.Parse(x.Value))
            FeatureData(name.Value, rest)
        else 
            None
    
    let readAsync (stream: Stream) =
        async {
            let a = Job.using <| new StreamReader(stream) <| fun reader ->
                        Stream.indefinitely <| job { return! reader.ReadLineAsync() }
                        |> Stream.take 10000L
                        |> Stream.foldFun(fun acc x -> match x with MetaData(d) -> { acc with Features = d} | _ -> acc) { Features  = 0}     
                        |> Job.start
                                                              
            match line2 with
            | MetaData(c) ->
                return { Features = c }
            | FeatureData(n, fatures) ->          
                return { Features = 3213321 }
            | None -> 
               return { Features = 1313 } 
        } |> Async.StartAsTask