namespace rec FabulousMinutes.Core

open System
open System.IO

open Microsoft.AspNetCore.Http
open Giraffe

open DynamicObj
open Newtonsoft.Json

type Logger(?customLogging: Logger -> unit) =
    inherit DynamicObj()

    static member Init(?props) =
        let t = Logger()
        if props.IsSome then
            props.Value |> List.iter t.SetValue
            t
        else
            t.SetValue("", None)
            t

    member this.ToJson() = DynamicObj.toJson this

    member this.GetPath() = 
        this.TryGetTypedValue<Logger> "Request"
        |> Option.bind (fun x ->
            x.TryGetValue "Path" |> Option.map string
        )
        |> fun x -> if x.IsNone then failwith "Could not find Request.Path in logger object." else x.Value

    member this.DynamicAccess(accessStr:string) = DynamicAccess.dynamicAccess accessStr this |> Option.get

    member this.DynamicAccess<'a>(accessStr:string) = DynamicAccess.dynamicAccess accessStr this |> Option.get :?> 'a

    member this.ToTemplate(templateStr:string) = DynamicAccess.readDynObjInFormatString(this,templateStr)

    member this.BindToHttpHandler (app:HttpHandler) : HttpHandler =

        fun (next:HttpFunc) (ctx:HttpContext) ->
            task {
                let appliedHandler = app next
                use reader = new StreamReader(ctx.Request.Body)

                /// This will empty 'ctx.Request.Body', which we will have to reinsert afterwards
                /// Maybe change to this: https://devblogs.microsoft.com/dotnet/re-reading-asp-net-core-request-bodies-with-enablebuffering/
                let! body = reader.ReadToEndAsync()
                let nextSTREAM =
                    let toBytes = System.Text.Encoding.UTF8.GetBytes(body)
                    new MemoryStream(toBytes)
                //printfn "Text %A" text
                /// return stream back to body so our Fable.Remoting actually has parameters to work with
                ctx.Request.Body <- nextSTREAM

                let! result = appliedHandler ctx
                match result with
                | Some resultContext ->
                    let st = System.DateTime.Now.ToUniversalTime()
                    let response =
                        [
                            "StatusCode", string resultContext.Response.StatusCode |> box
                            "Time", (System.DateTime.Now.ToUniversalTime() - st).ToString() |> box
                        ]
                        |> List.map (fun x -> fst x, snd x)
                    let request =
                        let query =
                            let queryLogger = Logger()
                            ctx.Request.Query |> Seq.iter (fun x -> queryLogger.SetValue(x.Key, x.Value) )
                            queryLogger
                        let headers =
                            let queryLogger = Logger()
                            ctx.Request.Headers |> Seq.iter (fun x -> queryLogger.SetValue(x.Key, x.Value |> String.concat ",") )
                            queryLogger 
                        let userAgent =
                            ctx.Request.Headers
                            |> Seq.tryFind (fun x -> x.Key ="User-Agent")
                            |> Option.map (fun x -> x.Value |> String.concat ",")
                            |> Option.defaultValue ""
                        let contentType =
                            ctx.Request.Headers
                            |> Seq.tryFind (fun x -> x.Key ="Content-Type")
                            |> Option.map (fun x -> x.Value |> String.concat ",")
                            |> Option.defaultValue ""
                        [
                            "Path", box ctx.Request.Path
                            "PathBase", box ctx.Request.PathBase
                            "Method", box ctx.Request.Method
                            "Host", box ctx.Request.Host.Host
                            "Port",
                                if ctx.Request.Host.Port.HasValue then string ctx.Request.Host.Port.Value else ""
                                |> box
                            "QueryString",
                                if ctx.Request.QueryString.HasValue then string ctx.Request.Host.Port.Value else ""
                                |> box
                            "Query", if ctx.Request.Query.Count > 0 then box query else null
                            "Headers", if ctx.Request.Headers.Count > 0 then box headers else null
                            "UserAgent", box userAgent
                            "ContentType", box contentType
                            "Body", box body
                        ]
                        |> List.map (fun x -> fst x, snd x)
                    let logger : Logger =
                        [
                            "Timestamp", st.ToString("yyyy.MM.dd hh:mm:ss.fffff") |> box
                            "Request", Logger.Init(request) |> box
                            "Response", Logger.Init(response) |> box
                        ]
                        |> List.map (fun x -> fst x, snd x)
                        |> List.iter (fun x -> 
                            this.SetValue x
                        )
                        |> ignore
                        this

                    match customLogging with
                    | Some logging -> logging logger
                    | None -> logger |> printfn "%A"

                    return Some resultContext

                | None ->
                    return None

            }