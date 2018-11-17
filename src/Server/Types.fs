module Types
open MathNet.Numerics.LinearAlgebra

type DeviceType = { Name: string }

type Object = { className: string; Features: float array }

type State = { FeaturesCount: int; Features: Object array }
with
    static member Zero () =
        { FeaturesCount = 0; Features = Array.empty }

type MailboxState = { State: State; TrainingSet: State; ForClassificationSet: State }
with
    static member Zero() =
        { State = State.Zero(); TrainingSet = State.Zero(); ForClassificationSet = State.Zero() }