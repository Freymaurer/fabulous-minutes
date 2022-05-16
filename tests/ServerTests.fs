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

open FabulousMinutes
open Types

module StorageUtils =

    type MyLog = {
        Path: string
        Body: string
        IsSuccess: bool
        Timestamp : string
    } with
        static member create path body isSuccess ts = {
            Path = path
            Body = body
            IsSuccess = isSuccess
            Timestamp = ts
        }

    let exmpLog = MyLog.create "\test\api\path" """[{"jsonKey": "jsonValue"}]""" true (System.DateTime.Now.ToUniversalTime().ToString())

    type LogStore() = 

        let logs = ResizeArray<(string*MyLog)>()

        member _.GetLogs() = logs |> Array.ofSeq

        member _.AddLog(log:MyLog) = logs.Add(log.Path,log)

type ILoggingServer = {
    helloWorld : unit -> Async<string>
    getLength : string -> Async<int>
    echoStringList: string list -> Async<string list>
    echoPerson:  PersonTestType -> Async<PersonTestType>
}

let loggingServer (ctx: HttpContext) : ILoggingServer  = {
    helloWorld = fun () -> async { return "hello" }
    getLength = fun input -> async { return input.Length }
    echoStringList = fun strList -> async { return strList }
    echoPerson = fun person -> async { return person }
}

module Route =
    let builder manual typeName methodName =
        sprintf "/%s/api/%s/%s" manual typeName methodName

let expectoLogger = Log.create "fabulous-minutes"

module Logger = 
    
    open Fable.Remoting.Server
    open Fable.Remoting.Giraffe

    let createWebApp loggingFunc pathIdent =
        Remoting.createApi()
        |> Remoting.withRouteBuilder (Route.builder pathIdent)
        |> Remoting.fromContext loggingServer
        |> Remoting.buildHttpHandler
        |> Logger(loggingFunc).BindToHttpHandler

    module Print =

        let logging(str:string) : Logger -> unit = 
            fun logger -> expectoLogger.info(Message.eventX str)

        let webApp =
            createWebApp (logging "print web app") "print"

    module DynamicAccessPrint =
       
        let dynAccessPrintLogging : Logger -> unit = 
            fun logger -> 
                let path = logger.DynamicAccess "Request.Path"
                let ts = logger.TryGetValue("Timestamp") |> Option.get
                let body = logger.DynamicAccess<Logger> "Request.Body" |> fun x -> x.ToJson()
                expectoLogger.infoWithBP(Message.eventX $"dynAccessPrintLogging: {path} {ts} <{body}>") |> Async.RunSynchronously

        let webApp =
            createWebApp dynAccessPrintLogging "dynAcc"

    module DynamicAccessMutable =
       
        let mutable str = ""

        let dynAccessMutableLogging : Logger -> unit = 
            fun logger -> 
                let path = logger.DynamicAccess "Request.Path" |> string
                str <- path

        let webApp =
            createWebApp dynAccessMutableLogging "dynAccMutable"

    module Storage =

        let logStorage = StorageUtils.LogStore()

        logStorage.AddLog(StorageUtils.exmpLog) |> ignore

        let logToStorage : Logger -> unit = 
            fun logger -> 
                let p = logger.GetPath()
                let body = 
                    try 
                        logger.DynamicAccess<Logger> "Request.Body"
                        |> fun x -> x.ToJson()
                    with
                        | exn -> ""
                let isSuccess = logger.DynamicAccess<int> "Response.StatusCode" |> fun x -> x = 200
                let ts = logger.DynamicAccess<string> "Timestamp"
                StorageUtils.MyLog.create p body isSuccess ts
                |> logStorage.AddLog

        let webApp =
            createWebApp logToStorage "storage"

    module FileLogger =

        let logToFile : Logger -> unit = 
            fun logger -> 
                let p0 = logger.GetPath().Replace("/","")
                let p = $"{p0}.txt"
                let body = 
                    logger.DynamicAccess<Logger> "Request.Body"
                    |> fun x -> x.ToJson()
                File.WriteAllTextAsync(p, body) |> Async.AwaitTask |> Async.RunSynchronously

        let webApp =
            createWebApp logToFile "file"


module ServerParts =

    open Giraffe
    //open Fable.Remoting.AspNetCore
    open Microsoft.Extensions.DependencyInjection

    open Logger

    let endpoints =
        choose [
            // improve perfomance with
            //routeStartsWith "/dynAcc" >=> PrintDynamicAccess.webApp
            Print.webApp
            DynamicAccessPrint.webApp
            DynamicAccessMutable.webApp
            Storage.webApp
            FileLogger.webApp
        ]

    let configureApp (app : IApplicationBuilder) =
        app.UseGiraffe(endpoints)

    let configureServices (services : IServiceCollection) =
        services.AddGiraffe() |> ignore

    let createHost() =
        WebHostBuilder()
            .UseContentRoot(Directory.GetCurrentDirectory())
            .Configure(Action<IApplicationBuilder> configureApp)
            .ConfigureServices(configureServices)

open ServerParts

let testServer = new TestServer(createHost())
let client = testServer.CreateClient()  

module ClientParts =
    open Fable.Remoting.DotnetClient

    // proxies to different API's
    let printLoggerProxy = Proxy.custom<ILoggingServer> (Route.builder "print") client false
    let dynAccPrintLoggerProxy = Proxy.custom<ILoggingServer> (Route.builder "dynAcc") client false
    let dynAccMutableLoggerProxy = Proxy.custom<ILoggingServer> (Route.builder "dynAccMutable") client false
    let storageLoggerProxy = Proxy.custom<ILoggingServer> (Route.builder "storage") client false
    let fileLoggerProxy = Proxy.custom<ILoggingServer> (Route.builder "file") client false

open ClientParts

[<Tests>]
let server_fable_remoting_tests = 
    testList "Server fable remoting tests" [ 

        test "Test storage base case" {
            let logs = Logger.Storage.logStorage.GetLogs()
            let exmpLog = logs |> Array.find (fun (p,log) -> p = "\test\api\path")
            Expect.equal (snd exmpLog) StorageUtils.exmpLog ""
        }

        testCaseAsync "Logger test with print to console" <| async {
            let! result = printLoggerProxy.call(fun server -> server.getLength "hello")
            Expect.equal result 5 ""
        }

        testCaseAsync "Logger test empty request body" <| async {
            let! result = printLoggerProxy.call(fun server -> server.helloWorld () )
            Expect.equal result "hello" "This could fail if an empty request body is not correctly parsed to the logger obj."
        }

        testCaseAsync "Logger test with print to console duplicate (test async)" <| async {
            let! result = printLoggerProxy.call(fun server -> server.getLength "hello")
            Expect.equal result 5 ""
        }

        testCaseAsync "Logger test with Dynamic Access print to console with record type" <| async {
            let person = {Name = "Sam"; Age = 28; Size = 1.84}
            let! result = dynAccPrintLoggerProxy.call(fun server -> server.echoPerson person)
            Expect.equal result person ""
        }

        testCaseAsync "Logger test with Dynamic Access to mutable variable" <| async {
            let! result = dynAccMutableLoggerProxy.call(fun server -> server.getLength "hello")
            let expectedPath = "/dynAccMutable/api/ILoggingServer/getLength"
            Expect.equal Logger.DynamicAccessMutable.str expectedPath ""
            Expect.equal result 5 ""
        }

        testCaseAsync "Logger test with Dynamic Access and log storage type" <| async {
            let expectedPath = "/storage/api/ILoggingServer/getLength"
            let! result = storageLoggerProxy.call(fun server -> server.getLength "hello")
            let logs = Logger.Storage.logStorage.GetLogs()
            let exmpLog = logs |> Array.find (fun (p,log) -> p = "\test\api\path")
            let newLog = logs |> Array.find (fun (p,log) -> p = expectedPath) |> snd
            Expect.equal (snd exmpLog) StorageUtils.exmpLog ""
            Expect.equal newLog.Path expectedPath ""
            Expect.equal newLog.IsSuccess true ""
            Expect.equal newLog.Body """["hello"]""" "If This is empty. The ctx.Request.body was read in. Needs to reset body."
            Expect.isNotEmpty newLog.Timestamp ""
            Expect.equal result 5 ""
        }

        //testCaseAsync "Logger test with File" <| async {
        //    let! result = fileLoggerProxy.call(fun server -> server.getLength "hello")
        //    let p0 = "/file/api/ILoggingServer/getLength".Replace("/","")
        //    let p = $"{p0}.txt"
        //    let! t = 
        //        task {
        //            let! body = File.ReadAllTextAsync(p)
        //            Expect.equal body """["hello"]""" "If This is empty. The ctx.Request.body was read in. Needs to reset body."
        //            let! delete = async { File.Delete p }
        //            return ()
        //        } 
        //        |> Async.AwaitTask
        //    Expect.equal result 5 ""
        //}

        testCaseAsync "Logger test with File and empty request body." <| async {
            let! result = fileLoggerProxy.call(fun server -> server.helloWorld () )
            let p0 = "/file/api/ILoggingServer/helloWorld".Replace("/","")
            let p = $"{p0}.txt"
            let! t = 
                task {
                    let! body = File.ReadAllTextAsync(p)
                    Expect.equal body """[null]""" "First time check to see how empty request bodies are handled."
                    let! delete = async { File.Delete p }
                    return ()
                } 
                |> Async.AwaitTask
            //File.Delete(p)
            Expect.equal result "hello" ""
        }

    ]
