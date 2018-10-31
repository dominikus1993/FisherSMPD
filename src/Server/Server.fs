open System
open System.IO
open System.Threading.Tasks
open Microsoft.AspNetCore.Cors
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection

open FSharp.Control.Tasks.V2
open Giraffe
open Shared
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Microsoft.AspNetCore.Http.Features
open System.Threading

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us

let getFisherForDimension (dimension: int)  (t: FeatureExtract): Async<FisherResponse> = Service.getFisherFactor(dimension) t
let counterApi = {
    getFisherForDimension = getFisherForDimension
}

let remoting =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue counterApi
    |> Remoting.buildHttpHandler

let fileUploadHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let formFeature = ctx.Features.Get<IFormFeature>()
            let! form = formFeature.ReadFormAsync CancellationToken.None
            do! Service.uploadDatabaseFile(form.Files.[0].OpenReadStream()) |> Async.StartAsTask
            return! (form.Files.[0].Name |> text) next ctx
        }
let webApp =

    choose [
        route "/upload" >=> fileUploadHandler
        remoting
    ]

let configureApp (app : IApplicationBuilder) =
    app.UseDefaultFiles()
       .UseStaticFiles()
       .UseCors(fun builder ->
                    builder.AllowAnyHeader() |> ignore
                    builder.AllowAnyMethod() |> ignore
                    builder.AllowAnyOrigin() |> ignore
                    )
       .UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    services.AddGiraffe() |> ignore
    services.AddCors() |> ignore

WebHost
    .CreateDefaultBuilder()
    .UseWebRoot(publicPath)
    .UseContentRoot(publicPath)
    .Configure(Action<IApplicationBuilder> configureApp)
    .ConfigureServices(configureServices)
    .UseUrls("http://0.0.0.0:" + port.ToString() + "/")
    .Build()
    .Run()