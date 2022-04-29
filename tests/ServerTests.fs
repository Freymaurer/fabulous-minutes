module ServerTests

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.AspNetCore.Http

open Expecto
open Expecto.Logging
open Expecto.Logging.Message

open FabulousMinutes.Core

type IServer = {
    getLength : string -> Async<int>
}

type ILoggingServer = {
    helloWorld : unit -> Async<string>
    // primitive types
    simpleUnit : unit -> Async<int>
    getLength : string -> Async<int>
    echoString : string -> Async<string>
}

let server (ctx: HttpContext) : IServer  = {
    getLength = fun input -> async { return input.Length }
}

let loggingServer (ctx: HttpContext) : ILoggingServer  = {
    helloWorld = fun () -> async { return "hello" }
    // primitive types
    simpleUnit = fun () -> async { return 42 }
    getLength = fun input -> async { return input.Length }
    echoString = fun str -> async { return str }
}

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

let expectoLogger = Log.create "fabulous-minutes"

module ServerParts =

    open Giraffe
    open Fable.Remoting.AspNetCore
    open Fable.Remoting.Server
    open Fable.Remoting.Giraffe
    open Microsoft.Extensions.DependencyInjection

    let webApp =
        Remoting.createApi()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.fromContext server

    let logging : Logger -> unit = 
        fun logger -> expectoLogger.info(Message.eventX "Test HERE")

    let loggingWebApp =
        Remoting.createApi()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.fromContext loggingServer
        |> Remoting.buildHttpHandler
        |> Logger().bindToHttpHandler

    let configureApp (app : IApplicationBuilder) =
        app.UseRemoting(webApp)
        app.UseGiraffe loggingWebApp

    let configureServices (services : IServiceCollection) =
        // Add Giraffe dependencies
        services.AddGiraffe() |> ignore

    let createHost() =
        WebHostBuilder()
            .UseContentRoot(Directory.GetCurrentDirectory())
            .Configure(Action<IApplicationBuilder> configureApp)
            .ConfigureServices(configureServices)

open ServerParts

let testServer = new TestServer(createHost())
let client = testServer.CreateClient()

module LogginBaseCase =

    let testServer = new TestServer(createHost())
    let client = testServer.CreateClient()

module ClientParts =
    open Fable.Remoting.DotnetClient

    // proxies to different API's
    let proxy = Proxy.custom<IServer> Route.builder client false
    let loggingProxy = Proxy.custom<ILoggingServer> Route.builder LogginBaseCase.client false

open ClientParts

[<Tests>]
let server_fable_remoting_tests = 
    testList "Server fable remoting tests " [ 

        testCaseAsync "TestServer test" <| async {
            do! expectoLogger.infoWithBP(Message.eventX "start")
            let! result = proxy.call(fun server -> server.getLength "hello")
            Expect.equal result 5 ""
        }

        testCaseAsync "Base logging test with print to console" <| async {
            let! result = loggingProxy.call(fun server -> server.getLength "hello")
            Expect.equal result 5 ""
        }

        //testCaseAsync "Fable base logging test save logg to txt" <| async {
        //    let mutable res = Logger()
        //    let logging = 
        //        fun (logger:Logger) -> 
        //            let loggedJson = logger.toJson()
        //            expectoLogger.info(eventX $"JSON: {loggedJson}")
        //            ()
        //    let proxy = createloggedTestServer(logging)
        //    let! result = proxy.call(fun server -> server.getLength "hello") 
        //    Expect.equal result 5 ""
        //}

        //testCaseAsync "Fable base logging test save path to mutable" <| async {
        //    let mutable res = ""
        //    let loggingFunc = fun (logger: Logger) -> logger.dynamicAccess("Request.Path") |> printfn "fabulous-minutes: %A"
        //    let proxy = createloggedTestServer(loggingFunc)
        //    let! result = proxy.call(fun server -> server.getLength "hello")
        //    Expect.isNotEmpty res ""
        //}

    ]
