namespace FabulousMinutes

open System
open DynamicObj

module DynamicAccess =

    open System.Text.RegularExpressions

    module Regex =
        
        /// 1. negative lookbehind: (?<!(/|\\)) -> No / or \ before {
        /// 2. must start with: {
        /// 3. capture named group 'value' : (?<value>.+?(?!(/|\\))); careful \<value> does not show as comment, better look at code.
        /// 4. group contains any number of wildcard characters except { AND }, minimum 1 but as few as possible: [^\{}]+?
        /// 5. negative lookahead: (?!(/|\\)) -> No / or \ before }
        /// 6. must end with: }
        [<Literal>]
        let internal Pattern = @"(?<!(/|\\)){(?<value>[^\{}]+?(?!(/|\\)))}"

        let getDynamicAccessStrings (input: string) = [| for i in Regex.Matches(input, Pattern) -> i |] 

    /// Use "." syntax to access nested objects. Use "^?" to access sequence header.
    /// **Example**: "Request.Body^?"
    let dynamicAccess (accessStr:string) (dynObject:DynamicObj) =
        let toDynArr = accessStr.Split([|"."|], StringSplitOptions.RemoveEmptyEntries)
        let rec access (ind:int) (dynArr:string []) result =
            if ind >= dynArr.Length then
                result
            elif ind <> 0 && result = None then
                None
            else
                // when starting the start object is the parent
                let parentObj = if ind = 0 then dynObject else box result.Value :?> DynamicObj
                // next will be the result if it is the last step of the dynArr. 
                // If it is not, it MUST be a dynamic object (Thats why parentObj will be casted to DynamicObj in the next rec iteration).
                // If the current dynArr-step ends with "^?" the user assumes it is a sequence at this step and wants access to the seq head. In this case we cast to seq<'a>.
                let currentDynArr, isSeqHead = 
                    let cda0 = dynArr.[ind]
                    let isSeqHead = cda0.EndsWith "^?" 
                    let cda = if isSeqHead then cda0.[..cda0.Length-3] else cda0
                    cda, isSeqHead
                let next = 
                    let next0 = parentObj.TryGetValue(currentDynArr)
                    next0 |> Option.map (fun (n: obj) -> if isSeqHead then n :?> seq<obj> |> Seq.head else n) 
                access (ind+1) dynArr next
        access 0 toDynArr None

    let readDynObjInFormatString(dynObj:DynamicObj,formatString:string) =
        /// Need replacerList to store artificial guids and actual dynamic access values. 
        /// The Guids are used as temporary replacements to remove escaped curly braces, without accidentally touching any inserted dynamic values.
        let mutable replacerList: (string*string) list = []
        let evaluator = 
            MatchEvaluator (fun m -> 
                let dynAccessResult = dynamicAccess m.Groups.["value"].Value (dynObj) 
                let dynAccessResultString =
                    if dynAccessResult.IsSome then
                        match dynAccessResult.Value with
                        | :? DynamicObj as d -> DynamicObj.toJson d
                        | :? seq<obj> as dseq -> dseq |> Seq.map (fun x -> x :?> DynamicObj |> DynamicObj.toJson) |> String.concat "; " |> sprintf "[%s]"
                        | _ -> dynAccessResult.Value.ToString()
                    else
                        "None"
                let newGuid = System.Guid.NewGuid().ToString()
                // save both guid and actual value in replacerList. 
                replacerList <- (newGuid,dynAccessResultString)::replacerList
                // return guid to replace dynamic access string
                newGuid
            )
        let removeEscapedCurlyBraces(str:string) = 
            Regex.Replace(str, @"(\\{|/{)", @"{")
            |> fun x -> Regex.Replace(x, @"(\\}|/})", @"}")
        let replaceTempGuids(str:string) = 
            let mutable res = str
            replacerList |> List.iter (fun (guid,value) -> 
                res <- Regex.Replace(res, guid, value)
            )
            res
        // replace dyn access string with random guids, stored with actual values in replacerList
        Regex.Replace(formatString, Regex.Pattern, evaluator)
        // Update escaped curly braces to normal curly braces
        |> removeEscapedCurlyBraces
        // replace guids with actual dynamic access values
        |> replaceTempGuids

