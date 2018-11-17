module Classifiers.Tests
open Expecto
open MathNet.Numerics.LinearAlgebra
open Types

[<Tests>]
let testFisher =
    testList "Classifiers" [
        testList "extractTraingSet" [
            test "1" {
                let state = { FeaturesCount = 1; Objects = [|{ ClassName = "Acer"; Features = [|1.|]}; { ClassName = "Test"; Features = [|1.|] } |] }
                let (test, stat) = Classifiers.extractTraingSet state 50
                Expect.equal test.Objects.Length 1 ""
                Expect.equal stat.Objects.Length 1 ""
            }
        ]
    ]