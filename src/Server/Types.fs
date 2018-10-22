module Types

type DeviceType = { Name: string }

type State = { FeaturesCount: int; Features: Map<string, float array array> }