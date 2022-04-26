namespace FabulousMinutes.Core

open System
open DynamicObj

module Template =

    let dynamicAccess (dynObject:DynamicObj)  (accessStr:string) =
        let toDynArr = accessStr.Split([|"."|], StringSplitOptions.RemoveEmptyEntries)
        let rec access (ind:int) (dynArr:string []) result =
            if ind >= dynArr.Length then
                result
            elif ind <> 0 && result = None then
                None
            else
                let parentObj = if ind = 0 then dynObject else box result.Value :?> DynamicObj
                let next = parentObj.TryGetValue(dynArr.[ind])
                access (ind+1) dynArr next
        access 0 toDynArr None

    /// 1. negative lookbehind: (?<!(/|\\)) -> No / or \ before {
    /// 2. must start with: {
    /// 3. capture named group 'value' : (?<value>.+?(?!(/|\\))); careful \<value> does not show as comment, better look at code.
    /// 4. group contains any number of wildcard characters except { AND }, minimum 1 but as few as possible: [^\{}]+?
    /// 5. negative lookahead: (?!(/|\\)) -> No / or \ before }
    /// 6. must end with: }
    [<Literal>]
    let private Pattern = @"(?<!(/|\\)){(?<value>[^\{}]+?(?!(/|\\)))}"

    open System.Text.RegularExpressions

    let getDynamicAccessStrings (input: string) = [| for i in Regex.Matches(input, Pattern) -> i |] 

    let readDynObjInFormatString(dynObj:DynamicObj,formatString:string) =
        /// Need replacerList to store arbitrary guids and actual dynamic access values. 
        /// The Guids are used as temporary replacements to remove escaped curly braces, without accidentally touching any inserted dynamic values.
        let mutable replacerList: (string*string) list = []
        let evaluator = 
            MatchEvaluator (fun m -> 
                let dynAccessResult = dynamicAccess (dynObj) m.Groups.["value"].Value
                let dynAccessResultString =
                    if dynAccessResult.IsSome then 
                        dynAccessResult.Value.ToString()
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
        Regex.Replace(formatString, Pattern, evaluator)
        // Update escaped curly braces to normal curly braces
        |> removeEscapedCurlyBraces
        // replace guids with actual dynamic access values
        |> replaceTempGuids

