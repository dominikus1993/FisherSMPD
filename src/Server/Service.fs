module Service
open System.IO
open Types
open Database
open FisherMath
open System.Numerics
open MathNet.Numerics.LinearAlgebra
open System
open Hopac.Extensions
open Hopac
open System.Linq
open Hopac.Extensions
open Shared
open Classifiers
open Newtonsoft.Json
open Hopac.Extensions
open Hopac.Extensions
open Hopac.Extensions

type Msg =
    | Store of state: State
    | DivideState of enc: Enhancements
    | Get of AsyncReplyChannel<MailboxState>
    | GetPossibleDimension of AsyncReplyChannel<int>

let Agent =
    MailboxProcessor.Start(fun inbox ->
        let rec loop state =
            async {
                match! inbox.Receive() with
                | Store s ->
                    return! loop { state with State = s}
                | Get reply ->
                    reply.Reply(state)
                    return! loop state
                | GetPossibleDimension reply ->
                    reply.Reply(state.State.FeaturesCount)
                    return! loop state
                | DivideState enc ->
                    printfn "Witam"
                    match enc with
                    | Bootstrap iterations ->
                        let res = Enchancments.bootstrap state.State iterations
                        printfn "Zegnam"
                        return! loop { state with TrainingSet = res; ForClassificationSet = state.State}
                    | NoneEnc percent ->
                        let (trainingSet, forClassificationSet) = extractTraingSet state.State percent
                        printfn "Zegnam"
                        return! loop { state with TrainingSet = trainingSet; ForClassificationSet = forClassificationSet }
             }
        loop(MailboxState.Zero()))

let uploadDatabaseFile (stream: Stream) =
    async {
        let! state = read stream
        Agent.Post(Store(state))
    }

let getFisherFactor dimension mode =
    async {
        let! state = Agent.PostAndAsyncReply(fun ch -> Get(ch))
        let keys = state.State.Objects |> Array.groupBy(fun x -> x.ClassName) |> Array.map(fun (k, a) -> a) |> Array.take 2
        let! possibleDimensions = Agent.PostAndAsyncReply(fun ch -> GetPossibleDimension(ch))
        if keys |> Array.length = 2 then
            let matrix1, matrix2 = matrix (keys.[0] |> Array.map(fun x -> x.Features)) |> Matrix.transpose, matrix (keys.[1] |> Array.map(fun x -> x.Features)) |> Matrix.transpose
            match mode with
            | Fisher ->
                return FisherMath.fs matrix1 matrix2 dimension possibleDimensions
            | Sfs ->
                return FisherMath.sfs matrix1 matrix2 dimension possibleDimensions
        else
            return { index = []; value = 0.0 }
    }

let enchance enc =
    Agent.Post(DivideState(enc))

let classify classificationMode =
    async {
        try
            let! state = Agent.PostAndAsyncReply(fun ch -> Get(ch))
            printfn "%A" classificationMode
            let func = match classificationMode with
                       | NN -> Classifiers.KNN.nn
                       | KNN k -> Classifiers.KNN.knn k
                       | NM -> Classifiers.KNM.nm
                       | KNM k -> Classifiers.KNM.knm k
            let count = state.TrainingSet.Objects |> Array.length
            let elements = state.TrainingSet.Objects
                            |> Array.map(fun o ->
                                            let classificationResult = func state.ForClassificationSet o
                                            o.ClassName = classificationResult
                                        )
                            |> Array.filter(fun x -> x)
                            |> Array.length
            return { PercentPositiveResults = elements * 100 / count }
        with
        | ex ->
            printfn "%A" ex
            return{ PercentPositiveResults = 0 }

    }