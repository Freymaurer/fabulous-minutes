namespace rec FabulousMinutes

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

    member this.GetPath() = 
        this.TryGetTypedValue<Logger> "Request"
        |> Option.bind (fun x ->
            x.TryGetValue "Path" |> Option.map string
        )
        |> fun x -> if x.IsNone then failwith "Could not find Request.Path in logger object." else x.Value

    member this.TryDynamicAccess(accessStr: string) = 
        // Uses base function
        DynamicAccess.dynamicAccess accessStr this

    member this.TryDynamicAccess<'a>(accessStr: string) = this.TryDynamicAccess(accessStr) |> Option.map (fun obj -> obj :?> 'a)

    member this.TryDynamicAccessAsInt(accessStr: string) : int option = DynamicAccess.dynamicAccess accessStr this |> Option.map (string >> int)

    member this.TryDynamicAccessAsFloat(accessStr: string) : float option = DynamicAccess.dynamicAccess accessStr this |> Option.map (string >> float)

    member inline this.TryDynamicAccessAsRecordType<'a>(accessStr: string) = 
        let cast (dObj0:obj) =
            let dObj = dObj0 :?> DynamicObj
            let t = typeof<'a>
            match Reflection.FSharpType.IsRecord(t) with
            | true -> 
                let fieldValues = 
                    Reflection.FSharpType.GetRecordFields(t) 
                    |> Array.map (fun x -> 
                        let vo = dObj.TryGetValue x.Name
                        if vo.IsNone then failwithf "Field %A does not exist." x.Name else vo.Value
                    )
                Reflection.FSharpValue.MakeRecord(t, fieldValues) :?> 'a
            | false ->
                failwithf "%A is not a record type." t.FullName
        this.TryDynamicAccess(accessStr) |> Option.map cast

    member inline this.TryDynamicAccessAsRecordTypeSeq<'a>(accessStr: string) = 
        let cast (dObj0:obj) =
            let dObj = dObj0 :?> seq<obj>
            let t = typeof<'a>
            match Reflection.FSharpType.IsRecord(t) with
            | true -> 
                dObj
                |> Seq.map (fun dObjSingle ->
                    let d = dObjSingle :?> DynamicObj
                    let fieldValues = 
                        Reflection.FSharpType.GetRecordFields(t) 
                        |> Array.map (fun x -> 
                            let vo = d.TryGetValue x.Name
                            if vo.IsNone then failwithf "Field %A does not exist." x.Name else vo.Value
                        )
                    Reflection.FSharpValue.MakeRecord(t, fieldValues) :?> 'a
                )
                |> List.ofSeq
            | false ->
                failwithf "%A is not a record type." t.FullName
        this.TryDynamicAccess(accessStr) |> Option.map cast

    /// <sumary> Allows nested access of Logger parameter via string. See docs for default object format.</sumary>
    /// <param name="accessStr"> String to access object parameters. 
    /// *Example*: "Request.Path" </param>
    member this.DynamicAccess(accessStr: string) = this.TryDynamicAccess(accessStr) |> Option.get

    member this.DynamicAccessAsInt(accessStr: string) = this.TryDynamicAccessAsInt(accessStr) |> Option.get

    member this.DynamicAccessAsFloat(accessStr: string) = this.TryDynamicAccessAsFloat(accessStr) |> Option.get

    /// <sumary> Allows nested access of Logger parameter via string. Returns parameter as type. See docs for default object format.</sumary>
    /// <param name="accessStr"> String to access object parameters. 
    /// *Example*: "Request.Path" </param>
    member this.DynamicAccess<'a>(accessStr: string) = this.DynamicAccess(accessStr) :?> 'a

    /// <summary> Inserts info from Logger type to prewritten 'templateStr'. </summary>
    /// <param name="templateStr"> The string to insert info into. Inside Curly braces dynamic access patterns can be used '{}'. 
    /// *Example*: "Logged {Request.Path} at {Timestamp}." </param>
    /// <returns> The string with inserted info. </returns>
    member this.ToTemplate(templateStr:string) = DynamicAccess.readDynObjInFormatString(this,templateStr)

    member this.ToTemplate() = 
        // if you change this you will have to adapt test "Test Logger default template string" in LibraryTests.fs
        let defaultTemplateString = """fabulous-minutes -- Response {Response.StatusCode}. Request at {Request.Path}, {Timestamp} with body: {Request.Body}"""
        DynamicAccess.readDynObjInFormatString(this,defaultTemplateString)

    member this.ToJson() = DynamicObj.toJson this

    static member OfJson(jsonStr:string) = 
        let d = DynamicObj.ofJson jsonStr
        let l = Logger()
        d.CopyDynamicPropertiesTo l
        l

    member this.Print() = this |> DynamicObj.format |> printfn "%s"

    member this.BindToHttpHandler (app:HttpHandler) : HttpHandler =

        fun (next:HttpFunc) (ctx:HttpContext) ->
            // task { } does not exist in .net5.0
            async {
                let appliedHandler = app next
                use reader = new StreamReader(ctx.Request.Body)

                // This will empty 'ctx.Request.Body', which we will have to reinsert afterwards
                // Maybe change to this: https://devblogs.microsoft.com/dotnet/re-reading-asp-net-core-request-bodies-with-enablebuffering/
                let! body = reader.ReadToEndAsync() |> Async.AwaitTask
                let nextSTREAM =
                    let toBytes = System.Text.Encoding.UTF8.GetBytes(body)
                    new MemoryStream(toBytes)
                // printfn "Text %A" text
                // return stream back to body so our Fable.Remoting actually has parameters to work with
                ctx.Request.Body <- nextSTREAM

                let! result = appliedHandler ctx |> Async.AwaitTask
                match result with
                | Some resultContext ->
                    let st = System.DateTime.Now.ToUniversalTime()
                    let response =
                        [
                            "StatusCode", resultContext.Response.StatusCode |> box
                            "Time", (System.DateTime.Now.ToUniversalTime() - st).ToString() |> box
                        ]
                        |> List.map (fun x -> fst x, snd x)
                    let request =
                        let query =
                            let l = Logger()
                            ctx.Request.Query |> Seq.iter (fun x -> l.SetValue(x.Key, x.Value) )
                            l
                        let headers =
                            let l = Logger()
                            ctx.Request.Headers |> Seq.iter (fun x -> l.SetValue(x.Key, x.Value |> String.concat ",") )
                            l 
                        let userAgent =
                            ctx.Request.Headers
                            |> Seq.tryFind (fun x -> x.Key = "User-Agent")
                            |> Option.map (fun x -> x.Value |> String.concat ",")
                            |> Option.defaultValue ""
                        let contentType =
                            ctx.Request.Headers
                            |> Seq.tryFind (fun x -> x.Key = "Content-Type")
                            |> Option.map (fun x -> x.Value |> String.concat ",")
                            |> Option.defaultValue ""
                        [
                            "Path", box ctx.Request.Path
                            "PathBase", box ctx.Request.PathBase
                            "Method", box ctx.Request.Method
                            "Host", box ctx.Request.Host.Host
                            "Port",
                                if ctx.Request.Host.Port.HasValue then box ctx.Request.Host.Port.Value else box null
                            "QueryString",
                                if ctx.Request.QueryString.HasValue then box ctx.Request.Host.Port.Value else box null
                            "Query", if ctx.Request.Query.Count > 0 then box query else null
                            "Headers", if ctx.Request.Headers.Count > 0 then box headers else null
                            "UserAgent", box userAgent
                            "ContentType", box contentType
                            "Body", box (Logger.OfJson body)
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
            |> Async.StartAsTask
