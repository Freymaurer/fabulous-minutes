module DynamicObjExtension

open Newtonsoft.Json
open DynamicObj

type DynamicObjConverter() =
    inherit JsonConverter<DynamicObj>()

    override this.ReadJson(reader : JsonReader, objectType : System.Type, existingValue : DynamicObj, hasExistingValue:bool, serializer : JsonSerializer) : DynamicObj = 
        // [Review]
        // Naming: sortiert die funktion ein JsonParser array? Wenn ja, sehe ich das nicht - aber ich glaube der name passt nicht. 

        // [Review answer]
        // umbenannt in 'readJsonParserFieldToDynObj'
        
        /// The isInit parameter is necessary as the reader starts with the first value.
        /// But every iteration thereafter we need to progress the reader to the next value, with reader.next().
        let rec readJsonParserFieldToDynObj (result: obj option) (isInit:bool) =
            let addValueToParentList(listObj:obj option) (value:'a) =
                /// unbox 'a does not seem to provide any benefit. When comparing output to manually created dyn object,
                /// it still needs to be boxed to be equal.
                let list = listObj.Value :?> obj seq |> Seq.map (fun x -> unbox<'a> x) |> List.ofSeq
                let res = (value::list) |> Seq.ofList
                readJsonParserFieldToDynObj (Some res) false
            // [Review]
            // Nitpicking: auch hier kannste die abfrage evtl schöner machen, soweit ich weiß evaluiert `||` die rechte seite nur wenn die linke false ist:
            // let x() = printfn "LOL!"; true
            // true || x() -> printet nix
            // false || x() -> printet "LOL!"
            //
            // kannst hier also einfach if isInit || reader.Read() machen
            //
            // solltest du aber testen

            // [Review answer]
            // unit tests sind erfolgreich, guter Punkt!
            let next = isInit || reader.Read()
            // [Review]
            // Nitpicking: next ist schon ein bool, schöner ist `if not next` oder einfach `if next` und die conditionals tauschen
            // [Review answer]
            // Würde ich normal zustimmen, hier war mir readability wichtig. 
            // Und ich persönlich fand (if reader.read() = false then result) besser verständlich.
            if next = false then 
                result
            else 
                // [Review]
                // An der benennung currentJsonObj siehste hier auch ncohmal, dass der typenname `JsonParser` nicht passt. ist es ein json objekt? auch nich so wirklich. Eigentlich speicherst du nur typ und value vom momentanen token. 
                // ich würde fast sagen du brauchst den typ garnicht und kannst einfach gegen reader.TokenType matchen und reader.Value verarbeiten.
                // [Review answer]
                // Sehr smart, nimmt complexity raus, direkt umgesetzt.
                let isList = result.IsSome && result.Value :? obj seq
                let tokenType = reader.TokenType
                let tokenValue = (if isNull reader.Value then None else string reader.Value |> Some)
                printfn "%A, %A" tokenType tokenValue
                match tokenType with
                | JsonToken.StartObject ->
                    let obj = DynamicObj()
                    if isList then
                        let v = readJsonParserFieldToDynObj (Some obj) false
                        addValueToParentList result v.Value
                    else
                        readJsonParserFieldToDynObj (Some obj) false
                | JsonToken.EndObject -> 
                    result
                | JsonToken.StartArray ->
                    /// Need to use Sequence to be able to use any casting to and from: obj seq <-> 'a seq
                    let list: obj seq = Seq.empty
                    readJsonParserFieldToDynObj (Some list) false
                | JsonToken.EndArray ->
                    let list = result.Value :?> obj seq |> List.ofSeq |> List.rev
                    Some list
                | JsonToken.PropertyName ->
                    let key = tokenValue.Value
                    if result.IsNone then failwith "Cannot apply property without parent dyn object."
                    let parent = 
                        match result.Value with
                        // [Review]
                        // Den cast verstehe ich nicht, was genau soll Logger sein, warum kommt das in nem generischen JsonConverter vor?
                        // [Review answer]
                        // Das hatte ich vergessen rauszunehmen in der fsx die du angeschaut hast. Hatte den converter nochmal extra
                        // in eine neue .fsx gemacht, dort war der Fehler schon aufgefallen.
                        | :? DynamicObj ->
                            let logger = result.Value :?> DynamicObj
                            let v = readJsonParserFieldToDynObj None false
                            logger.SetValue(key, v.Value)
                            logger |> box
                        | _ -> failwith "Cannot parse parent type to supported types." 
                    readJsonParserFieldToDynObj (Some parent) false
                | JsonToken.String -> 
                    let v = string tokenValue.Value
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                | JsonToken.Integer -> 
                    let v = int tokenValue.Value
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                | JsonToken.Float -> 
                    let v = float tokenValue.Value
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                | JsonToken.Boolean ->
                    let v = System.Boolean.Parse tokenValue.Value
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                | JsonToken.Null ->
                    // [Review]
                    // Null handling bei json ist so ne sache. Da du eh dynamic arbeitest, kannst du das auch nutzen und 
                    // null values einfach weg lassen, dann hast du auch kein Some/None gedöns
                    // [Review answer]
                    // nicht sicher was du hier genau meinst oder ich das umsetzen kann
                    let v = None
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                // TODO!
                | JsonToken.Bytes | JsonToken.Date ->
                    let v = string tokenValue.Value
                    if isList then
                        addValueToParentList result v
                    else
                        Some v
                | any -> 
                    // printfn "CAREFUL! %A" currentJsonObj
                    readJsonParserFieldToDynObj None false
        let res = readJsonParserFieldToDynObj(None) true |> Option.get
        match res with
        | :? list<obj> as list ->
            let loggerList = list
            let r = DynamicObj()
            r.SetValue("root", loggerList)
            r
        | :? DynamicObj as root ->
            root
        | _ -> failwith "Could not parse Result to any supported type."

    override this.WriteJson(writer : JsonWriter, value : DynamicObj, serializer : JsonSerializer) =
        let v =
            let settings = 
                let s = JsonSerializerSettings()
                s.ReferenceLoopHandling <- ReferenceLoopHandling.Serialize
                s
            let hasRootArr = value.TryGetValue "root"
            if hasRootArr.IsSome then
                hasRootArr.Value 
                |> fun v -> JsonConvert.SerializeObject(v, settings)
            else
                JsonConvert.SerializeObject(value, settings)
        writer.WriteRaw (v)

let toJson(dynObj:DynamicObj) = 
    JsonConvert.SerializeObject(dynObj, new DynamicObjConverter())

let ofJson(jsonSource:string) = JsonConvert.DeserializeObject<DynamicObj>(jsonSource, new DynamicObjConverter())