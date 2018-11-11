module Classifiers.Tests
open Expecto
open MathNet.Numerics.LinearAlgebra
open Types

[<Tests>]
let testFisher =
    testList "Classifiers" [
        testList "extractTraingSet" [
            test "1" {
                let state = { FeaturesCount = 1; Features = [("Acer", [|[|1.|]|]); ("Test", [|[|1.|]|])] |> Map.ofList }
                let (test, stat) = Classifiers.extractTraingSet state 50
                Expect.equal test.Features.Count 1 ""
                Expect.equal stat.Features.Count 1 ""
            }
        ]
    ]