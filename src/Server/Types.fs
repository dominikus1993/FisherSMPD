module Types

type DeviceType = { Name: string }

type Classifier =
    | NN
    | KNN
    | NM

type State = { FeaturesCount: int; Features: Map<string, float array array> }