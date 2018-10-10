namespace Fisher.Common

module Say =
    open MathNet.Numerics.LinearAlgebra
    let m = matrix [[ 1.0; 2.0 ]
                    [ 3.0; 4.0 ]]
    let det = m.Determinant()
    let hello name =
        printfn "Hello %s" name
