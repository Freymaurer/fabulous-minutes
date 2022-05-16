module LibraryTests

open Expecto
open DynamicObj

open FabulousMinutes

/// This function should always ONLY BE USED FOR TESTING!
/// THIS FUNCTION KILLS ANY WHITESPACE EVEN FROM JSON VALUES!
let private minifyJson(json:string) = 
    json
        .Replace(" ","")
        // if i add this line, my tests break
        // .Replace("\n",System.Environment.NewLine)
        .Replace(System.Environment.NewLine,"") 

[<Tests>]
let dynamicObj_json_converter_tests = 
    testList "DynamicObj json converter tests" [
        test "Test json string to dyn object compared to dyn object created by hand." {
            let simpleJson = """{"firstLevel": "test"}"""
            let dynObjOfJson = DynamicObj.ofJson(simpleJson)
            let dynObj = 
                let l = DynamicObj()
                l.SetValue("firstLevel", "test")
                l
            Expect.equal dynObjOfJson dynObj "Both dyn objects are expected to be equal."
        }
        test "Test json string to dyn object and back to json" {
            let simpleJson = minifyJson """{"firstLevel": "test"}"""
            let dynObjOfJson = DynamicObj.ofJson(simpleJson)
            let revertToJson = DynamicObj.toJson(dynObjOfJson)
            Expect.equal simpleJson revertToJson "Recreated Json, after being converted from to dyn object shoudl equal json source."
        }
        test "Test nested simple json object" {
            let json = minifyJson """{"firstLevel": {"name": "firstLevelName"}}"""
            let dynObjOfJson = DynamicObj.ofJson json
            let revertToJson = DynamicObj.toJson dynObjOfJson
            Expect.equal json revertToJson "Recreated Json equals json source with nested example."
        }
        test "Test json number types" {
            let json = minifyJson """{"depth": 2, "floatingBoat": 3.51}"""
            let dynObjOfJson = DynamicObj.ofJson json 
            let revertToJson = DynamicObj.toJson dynObjOfJson
            Expect.equal json revertToJson "Recreated Json equals json source with json number types."
        }
        test "Test 3-level nested json object with string and number json types" {
            let json = minifyJson """{"firstLevel": {"name": "firstLevelName","type": "object","firstLevelProperties": {"depth": 2,"floatingBoat": 3.51}}}"""
            let dynObjOfJson = DynamicObj.ofJson(json)
            let revertToJson = DynamicObj.toJson dynObjOfJson
            Expect.equal json revertToJson "Recreated Json equals json source with 3 level nested example."
        }
        test "Test Integer, float, bool, null json types" {
            let json = minifyJson """{"depth": 2,"floatingBoat": 3.51,"isTrue?": true,"isNull?": null}"""
            let dynObjOfJson = DynamicObj.ofJson json 
            let revertToJson = DynamicObj.toJson dynObjOfJson
            Expect.equal json revertToJson "Recreated Json equals json source with different json value types."
        }
        test "Test basic json array type." {
            let json = minifyJson """{"myfirstArray": ["value1", "value2", "value3"]}"""
            let dynObjOfJson = DynamicObj.ofJson json
            let revertToJson = DynamicObj.toJson dynObjOfJson
            Expect.equal json revertToJson "Recreated Json equals json source with different json value types."
        }
        test "Compare 'ofJson' to dyn obj created by hand, for json array type." {
            let simpleJson = """{"myfirstArray": ["value1", "value2", "value3"]}"""
            let dynObjOfJson = DynamicObj.ofJson(simpleJson)
            let dynObj = 
                let l = DynamicObj()
                /// Sadly i am not able to avoid converting to 'obj list'.
                let list: obj list = ["value1"; "value2"; "value3"]
                l.SetValue("myfirstArray", list)
                l
            Expect.equal dynObjOfJson dynObj "Both dyn objects are expected to be equal."
        }
        test "Test nested json array with object elements" {
            let json = minifyJson """{"myfirstArray": [{"name": "John","age": 30},{"name": "Mary","age": 25},{"name": "Peter","age": 20}]}"""
            let dynObjOfJson = DynamicObj.ofJson json
            let revertToJson = DynamicObj.toJson dynObjOfJson
            Expect.equal json revertToJson "Recreated Json equals json source with different json value types."
        }
        test "Test root level json array with object elements" {
            let json = minifyJson """[{"name": "John","age": 30},{"name": "Mary","age": 25},{"name": "Peter","age": 20}]"""
            let dynObjOfJson = DynamicObj.ofJson json
            let revertToJson = DynamicObj.toJson dynObjOfJson
            Expect.equal json revertToJson "Recreated Json equals json source with different json value types."
        }
        test "Test empty json objects" {
            let json = minifyJson """{"name": {}}"""
            let dynObjOfJson = DynamicObj.ofJson json
            let revertToJson = DynamicObj.toJson dynObjOfJson
            Expect.equal json revertToJson "Recreated Json equals json source with empty json object."
        }
        test "Root json array with simple elements" {
            let json = minifyJson """["Ford", "BMW", "Fiat"]"""
            let dynObjOfJson = DynamicObj.ofJson json
            let revertToJson = DynamicObj.toJson dynObjOfJson
            Expect.equal json revertToJson "Recreated Json equals json source with root level json array with simple elements."
        }
        test "Root json array with null" {
            let json = minifyJson """[null]"""
            let dynObjOfJson = DynamicObj.ofJson json
            let revertToJson = DynamicObj.toJson dynObjOfJson
            Expect.equal json revertToJson "Recreated Json equals json source with root level json array with simple elements."
        }
        test "Empty logger to json" {
            let l = Logger ()
            let res = l.ToJson()
            Expect.equal res "{}" ""
        }

    ]

open FabulousMinutes.DynamicAccess
open FabulousMinutes.DynamicAccess.Regex

let testLogJson = 
    minifyJson """{"Timestamp":"2022.05.03 07:28:11.82714","Request":{"Path":"/api/IHelpdeskAPI/getCaptcha","PathBase":"","Method":"GET","Host":"localhost","Port":8085,"QueryString":"","Query":null,"Headers":{"Connection":"close","Content-Type":"application/json; charset=utf-8","Accept":"*/*","Accept-Encoding":"gzip, deflate, br","Accept-Language":"en-GB,en-US;q=0.9,en;q=0.8","Cookie":"ajs_anonymous_id=%22bf0866e7-3877-4d25-8805-c1b2a4b5bd71%22; isDarkmode=false","Host":"localhost:8085","Referer":"http://localhost:8080/","User-Agent":"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.84 Safari/537.36 OPR/85.0.4341.75","sec-fetch-dest":"empty","sec-fetch-mode":"cors","sec-fetch-site":"same-origin","sec-ch-ua-platform":"\"Windows\"","sec-ch-ua-mobile":"?0","x-remoting-proxy":"true","sec-ch-ua":"\" Not A;Brand\";v=\"99\", \"Chromium\";v=\"99\", \"Opera\";v=\"85\""},"UserAgent":"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99.0.4844.84 Safari/537.36 OPR/85.0.4341.75","ContentType":"application/json; charset=utf-8","Body":[{"Name":"Sam","Age":28,"Size":1.84}]},"Response":{"StatusCode":200,"Time":"00:00:00.0000627"}}"""

[<Tests>]
let dynamic_access_tests =
    testList "dynamic access tests" [
        test "Test simple dynamic access" {
            let simpleJson = """{"myLog": {"Timestamp": "2022.03.28 07:45:10.00949","Request": {"Path": "/api/IHelpdeskAPI/checkCaptcha","PathBase": "","Method": "POST","Host": "localhost","Port": "8085","QueryString": ""}}}"""
            let dynObjOfJson = DynamicObj.ofJson(simpleJson)
            let dynamicAccessPort = dynamicAccess "myLog.Request.Port" dynObjOfJson
            Expect.equal dynamicAccessPort (Some "8085") "Expected to get port value."
        }
        test "Test access string pattern with simple access string" {
            let formatString = """{myLog.Request.Path}"""
            let accessString = getDynamicAccessStrings(formatString) |> Array.head
            Expect.equal accessString.Groups.["value"].Value ("myLog.Request.Path") "Should match and return access string."
        }
        test "Test access string pattern in more complex formatting string" {
            let formatString = """Logging {myLog.Request.Path} @ some time point."""
            let accessString = getDynamicAccessStrings(formatString) |> Array.head
            Expect.equal accessString.Groups.["value"].Value ("myLog.Request.Path") "Should match and return access string."
        }
        test "Test access string pattern with multiple access string" {
            let formatString = """Logging {myLog.Request.Path} @ {myLog.Timestamp}."""
            let accessString = getDynamicAccessStrings(formatString)
            Expect.equal accessString.[0].Groups.["value"].Value ("myLog.Request.Path") "Should match and return first access string."
            Expect.equal accessString.[1].Groups.["value"].Value ("myLog.Timestamp") "Should match and return second access string."
        }
        test "Test access string pattern with escaped curly only." {
            let formatString = """Testing escaped /{curles/}."""
            let accessString = getDynamicAccessStrings(formatString)
            Expect.equal accessString (Array.empty) "Should match and return access string."
        }
        test "Test access string pattern with empty non-escaped curly only." {
            let formatString = """Hello i am just fooling around {}"""
            let accessString = getDynamicAccessStrings(formatString)
            Expect.equal accessString (Array.empty) "Should match and return access string."
        }
        test "Test access string pattern with complext access string." {
            let formatString = """Logging {myLog.Request.Path} @ {myLog.Timestamp}. {} Request solved for {myLog.Response.StatusCode} /{{myLog.Response.Time}/}. Testing escaped /{curles/}."""
            let accessStrings = getDynamicAccessStrings(formatString)
            printfn "%A" accessStrings
            Expect.equal accessStrings.Length 4 "Should return 4 access strings."
            Expect.equal accessStrings.[0].Groups.["value"].Value ("myLog.Request.Path") "Should match and return 'myLog.Request.Path' access string."
            Expect.equal accessStrings.[1].Groups.["value"].Value ("myLog.Timestamp") "Should match and return 'myLog.Timestamp' access string."
            Expect.equal accessStrings.[2].Groups.["value"].Value ("myLog.Response.StatusCode") "Should match and return 'myLog.Response.StatusCode' access string."
            Expect.equal accessStrings.[3].Groups.["value"].Value ("myLog.Response.Time") "Should match and return 'myLog.Response.Time' access string."
        }
        test "Test correct escape of curly braces." {
            let json = """{"Key": "Value"}"""
            let formatString = """Testing escaped \{curly\} /{boys/}. And another \\{boy\\}. Now a mixed up \{curly boy/}."""
            let dynObjOfJson = DynamicObj.ofJson json
            let result = """Testing escaped {curly} {boys}. And another \{boy\}. Now a mixed up {curly boy}."""
            let readDynObjIntoFormatString = readDynObjInFormatString(dynObjOfJson,formatString)
            Expect.equal readDynObjIntoFormatString result "readDynObjIntoFormatString should equal result."
        }
        test "Test if values with escaped curly braces are still escaped." {
            let json = """{"Key": "This is my value with /{escaped/} curly braces."}"""
            let formatString = """The following value should still contain escaped curly braces: {Key}"""
            let dynObjOfJson = DynamicObj.ofJson json
            let result = """The following value should still contain escaped curly braces: This is my value with /{escaped/} curly braces."""
            let readDynObjIntoFormatString = readDynObjInFormatString(dynObjOfJson,formatString)
            Expect.equal readDynObjIntoFormatString result "readDynObjIntoFormatString should equal result."
        }
        test "Test read DynObj into complex formatString" {
            let json = """{"myLog": {"Timestamp": "2022.03.28 07:45:10.00949","Response": {"StatusCode": "200","Time": "00:00:14.3531003"}, "Request": {"Path": "/api/IHelpdeskAPI/checkCaptcha","PathBase": "","Method": "POST","Host": "localhost","Port": "8085","QueryString": ""}}}"""
            let formatString = """Logging "{myLog.Request.Path}" @ {myLog.Timestamp}. {} Request solved for {myLog.Response.StatusCode} /{time: {myLog.Response.Time}/}. Testing escaped \{curly\} /{boys/}."""
            let dynObjOfJson = DynamicObj.ofJson json
            let result = """Logging "/api/IHelpdeskAPI/checkCaptcha" @ 2022.03.28 07:45:10.00949. {} Request solved for 200 {time: 00:00:14.3531003}. Testing escaped {curly} {boys}."""
            let readDynObjIntoFormatString = readDynObjInFormatString(dynObjOfJson,formatString)
            Expect.equal readDynObjIntoFormatString result "readDynObjIntoFormatString should equal result."
        }
        test "Test dynamicAccess with seq header logic" {
            let logger = Logger.OfJson(testLogJson)
            let bodyName = logger.DynamicAccess<string> "Request.Body^?.Name"
            let bodyAge = logger.DynamicAccess<int> "Request.Body^?.Age"
            let bodySize = logger.DynamicAccess<float> "Request.Body^?.Size"
            Expect.equal bodyName "Sam" ""
            Expect.equal bodyAge 28 ""
            Expect.equal bodySize 1.84 ""
        }
    ]


type Response = {
    StatusCode: int
    Time: string
}

open Types

[<Tests>]
let logger_tests =
    testList "logger tests" [
        test "Test simple dynamic access member" {
            let logger = Logger.OfJson testLogJson
            let dynamicAccessPath = logger.DynamicAccess "Request.Path" 
            let revertToJson = DynamicObj.toJson logger
            Expect.equal revertToJson testLogJson ""
            Expect.equal dynamicAccessPath "/api/IHelpdeskAPI/getCaptcha" ""
        }
        test "Test nested prints" {
            let outer = Logger()
            let inner = Logger()
            inner.SetValue("Level", "Information")
            inner.SetValue("MessageTemplate","{Method} Request at {Path}")
            outer.SetValue("serilog", inner)
            let print =
                try 
                    outer.Print()
                    true 
                with
                    | e -> false
            Expect.isTrue print "Expected to print nested object."
        }
        test "Test different access functions" {
            let logger = Logger.OfJson testLogJson
            let dynamicAccess = logger.DynamicAccess "Request.Port"
            let tryDynamicAccess = logger.TryDynamicAccess "Request.Port"
            let dynamicAccessTyped = logger.DynamicAccess<int> "Request.Port" 
            let tryDynamicAccessTyped = logger.TryDynamicAccess<int> "Request.Port" 
            let dynamicAccessTypedDynObj = logger.DynamicAccess<DynamicObj> "Request"
            let dynamicAccessTypedInner =
                let l = Logger()
                dynamicAccessTypedDynObj.CopyDynamicPropertiesTo(l) 
                l.DynamicAccess "Path"
            let revertToJson = DynamicObj.toJson logger
            Expect.equal revertToJson testLogJson ""
            Expect.equal dynamicAccess 8085 ""
            Expect.equal tryDynamicAccessTyped (Some 8085) ""
            Expect.equal dynamicAccessTyped 8085 ""
            Expect.equal tryDynamicAccess (Some 8085) ""
            Expect.equal dynamicAccessTypedInner "/api/IHelpdeskAPI/getCaptcha" ""
        }
        test "Test dynamic access as record type" {
            let logger = Logger.OfJson testLogJson
            let response = logger.TryDynamicAccessAsRecordType<Response> "Response"
            Expect.isTrue response.IsSome ""
            Expect.equal response.Value.StatusCode 200 ""
            Expect.equal response.Value.Time "00:00:00.0000627" ""
        }
        test "Test Logger automated obj to json logic" {
            let expectedResult = """{"Name":"Sam","Age":28,"Size":1.84}"""
            let logger = Logger.OfJson testLogJson
            let templateStr = """{Request.Body^?}"""
            let response = logger.ToTemplate(templateStr)
            Expect.equal response expectedResult ""
        }
        test "Test Logger automated obj seq to json logic" {
            let expectedResult = """[{"Name":"Sam","Age":28,"Size":1.84}]"""
            let logger = Logger.OfJson testLogJson
            let templateStr = """{Request.Body}"""
            let response = logger.ToTemplate(templateStr)
            Expect.equal response expectedResult ""
        }
        test "Test Logger default template string" {
            let logger = Logger.OfJson testLogJson
            let response = logger.ToTemplate()
            // minify json kills the space in the timestamp
            let expectedRes = """fabulous-minutes -- Response 200. Request at /api/IHelpdeskAPI/getCaptcha, 2022.05.0307:28:11.82714 with body: [{"Name":"Sam","Age":28,"Size":1.84}]"""
            Expect.equal response expectedRes ""
        }
        test "Logger dynamic access to record type" {
            let logger = Logger.OfJson testLogJson
            let response = logger.TryDynamicAccessAsRecordType<Response> "Response"
            Expect.isTrue response.IsSome ""
            Expect.equal response.Value.StatusCode 200 ""
        }

        test "Logger dynamic access to record type seq" {
            let expectedResult = [{Name = "Sam"; Age = 28; Size = 1.84}]
            let logger = Logger.OfJson testLogJson
            let response = logger.TryDynamicAccessAsRecordTypeSeq<Types.PersonTestType> "Request.Body"
            Expect.isTrue response.IsSome ""
            Expect.equal (List.head response.Value) (List.head expectedResult) ""
            Expect.equal response.Value expectedResult ""
        }

        test "Logger dynamic access to record type and seq header" {
            let expectedResult = {Name = "Sam"; Age = 28; Size = 1.84}
            let logger = Logger.OfJson testLogJson
            let response = logger.TryDynamicAccessAsRecordType<Types.PersonTestType> "Request.Body^?"
            Expect.isTrue response.IsSome ""
            Expect.equal response.Value expectedResult ""
        }
    ]
