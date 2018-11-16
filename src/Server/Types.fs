module Types

type DeviceType = { Name: string }

type State = { FeaturesCount: int; Features: Map<string, float array array> }
with
    static member Zero () =
        { FeaturesCount = 0; Features = [] |> Map.ofList }

type MailboxState = { State: State; TrainingSet: State; ForClassificationSet: State }
with
    static member Zero() =
        { State = State.Zero(); TrainingSet = State.Zero(); ForClassificationSet = State.Zero() }