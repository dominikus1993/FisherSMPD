namespace Fisher.Common

open System
open MathNet.Numerics.Statistics
open MathNet.Numerics.Statistics
open MathNet.Numerics.Statistics

module Fisher =
    let F(matrixA: double seq)(matrixB: double seq)  =
        (Statistics.Mean(matrixA) - Statistics.Mean(matrixB)) / (Statistics.StandardDeviation(matrixA) / Statistics.StandardDeviation(matrixB))

    let FMD(matrixA: double seq)(matrixB: double seq)  =
