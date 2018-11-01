module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Fable.PowerPack
open Fable.Core.JsInterop
open Thoth.Json
open Shared
open Fable.Core.DynamicExtensions
open Fulma
open System.IO
open Fable.Import.Browser
open System.Net.Http
open Fulma.Extensions

// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = { Counter: Counter option; Mode: FeatureExtract; FileName: string; Result: FisherResponse option; IsLoading: bool }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
| Increment
| Decrement
| FileUpload of File: File
| FileUploadSuccess of string
| GetFisherFactor
| GetFisherFactorSuccess of FisherResponse
| InitialCountLoaded of Result<Counter, exn>
| ChangeMode of mode: FeatureExtract
| Error of exn


module Server =

    open Shared
    open Fable.Remoting.Client

    /// A proxy you can use to talk to server directly
    let api : IFisherApi =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<IFisherApi>


let sendFile (formData: FormData) =
    promise {
        let defaultProps =
            [ RequestProperties.Method HttpMethod.POST
            ; RequestProperties.Body <| unbox(formData)]
        let! res = fetch "http://localhost:8085/upload" defaultProps
        if res.Ok then
            return! res.text()
        else
            return failwith "file upload error"
    }
let getFisherFactor (dimension, mode) =
    async {
        let! result = Server.api.getFisherForDimension(dimension)(mode)
        return result
    }
let sendFileCmd (query : FormData) = Cmd.ofPromise sendFile query FileUploadSuccess Error
let getFisherFactorCmd(dimension: int, mode: FeatureExtract) = Cmd.ofAsync getFisherFactor (dimension, mode) GetFisherFactorSuccess Error
// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { Counter = Some(1); FileName = ""; Result = None; IsLoading = false; Mode = Fisher }
    initialModel, Cmd.none

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.Counter, msg with
    | Some x, Increment ->
        let nextModel = { currentModel with Counter = if x = 64 then Some(64) else Some (x + 1) }
        nextModel, Cmd.none
    | Some x, Decrement ->
        let nextModel = { currentModel with Counter = if x = 1 then Some(1) else Some (x - 1) }
        nextModel, Cmd.none
    | _, InitialCountLoaded (Ok initialCount)->
        let nextModel = { Counter = Some initialCount; FileName = ""; Result = None; IsLoading = false; Mode = Fisher }
        nextModel, Cmd.none
    | _, FileUpload file ->
        let formData = FormData.Create()
        formData.append(file.name, file)
        currentModel, sendFileCmd(formData)
    | _, FileUploadSuccess filename ->
        {currentModel with FileName = filename }, Cmd.none
    | Some(dimension), GetFisherFactor ->
        {currentModel with IsLoading = true }, getFisherFactorCmd(dimension, currentModel.Mode)
    | _, GetFisherFactorSuccess resp ->
        {currentModel with Result = Some(resp); IsLoading = false }, Cmd.none
    | _, ChangeMode mode ->
        {currentModel with Mode = mode}, Cmd.none
    | _, Error e ->
        printf "%A" e
        currentModel, Cmd.none
    | _ -> currentModel, Cmd.none


let safeComponents =
    let components =
        span [ ]
           [
             a [ Href "https://github.com/giraffe-fsharp/Giraffe" ] [ str "Giraffe" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io/elmish/" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://mangelmaxime.github.io/Fulma" ] [ str "Fulma" ]
             str ", "
             a [ Href "https://zaid-ajaj.github.io/Fable.Remoting/" ] [ str "Fable.Remoting" ]
           ]

    p [ ]
        [ strong [] [ str "SAFE Template" ]
          str " powered by: "
          components ]

let show = function
| { Counter = Some x } -> string x
| { Counter = None   } -> "Loading..."


let showResult = function
| { Result = Some x } -> sprintf "Wynik to %A dla indeksów %A" x.value x.index
| { Result = None   } -> ""


let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "SAFE Template" ] ] ]

          Container.container []
              [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Fulma.Screen.All, TextAlignment.Centered) ] ]
                    [ Heading.h3 [] [ str ("Wybierz ilosc cech: " + show model) ] ]
                Columns.columns []
                    [ Column.column [] [ button "-" (fun _ -> dispatch Decrement) ]
                      Column.column [] [ button "+" (fun _ -> dispatch Increment) ]
                       ] ]
          form [] [ Field.div [ ]
                [ Fulma.File.file [ Fulma.File.HasName ]
                    [ Fulma.File.label [ ]
                        [ Fulma.File.input [ Props([ OnChange (fun x -> FileUpload(x.target?files.["0"] :?> File) |> dispatch ) ])]
                          Fulma.File.cta [ ]
                            [ Fulma.File.label [ ]
                                [ str "Choose a file..." ] ]
                          Fulma.File.name [ ]
                            [ str model.FileName ] ] ] ]
          ]
          Field.div [ ] [
                if model.IsLoading then
                    yield button "Obliczam" (fun _ -> ())
                else
                    yield button "Oblicz" (fun _ -> dispatch GetFisherFactor)
          ]
          Field.div [ ]
            [ yield! Checkradio.radioInline [ Checkradio.Name "Fisher"; Checkradio.Checked(match model.Mode with | Fisher -> true | Sfs -> false); Checkradio.OnChange(fun _ -> dispatch (ChangeMode(Fisher)) ) ] [ str "One" ]
              yield! Checkradio.radioInline [ Checkradio.Name "Sfs"; Checkradio.Checked(match model.Mode with | Fisher -> false | Sfs -> true); Checkradio.OnChange(fun _ -> dispatch (ChangeMode(Fisher)) ) ] [ str "Two " ] ]
          Field.div [ ] [
                h1 [] [ str (showResult model) ]
          ]
          Footer.footer [ ]
                [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Fulma.Screen.All, TextAlignment.Centered) ] ]
                    [ safeComponents ] ] ]


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
