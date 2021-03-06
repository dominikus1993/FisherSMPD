namespace Shared

type Counter = int

type FisherResponse = { index: int list; value: float }

type FeatureExtract =
    | Fisher
    | Sfs

type Classifier =
    | NN
    | KNN of k: int
    | NM
    | KNM of k: int

type Enhancements =
    | Bootstrap of iterations: int
    | NoneEnc of percen: int

type ClassificationMode = { ClassifierType: Classifier;}

type ClassificationResult = { PercentPositiveResults: int }

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol between client and server
/// to learn more, read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
type IFisherApi =
    { getFisherForDimension: int -> FeatureExtract-> Async<FisherResponse>
      generateTrainingData: Enhancements -> Async<unit>
      classify: Classifier -> Async<ClassificationResult> }


