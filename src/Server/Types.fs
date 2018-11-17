module Types
open MathNet.Numerics.LinearAlgebra

type DeviceType = { Name: string }

type Object = { ClassName: string; Features: float array }

type State = { FeaturesCount: int; Objects: Object array }
with
    static member Zero () =
        { FeaturesCount = 0; Objects = Array.empty }

type MailboxState = { State: State; TrainingSet: State; ForClassificationSet: State }
with
    static member Zero() =
        { State = State.Zero(); TrainingSet = State.Zero(); ForClassificationSet = State.Zero() }