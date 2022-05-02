module LibraryTests

open Expecto
open DynamicObj

open FabulousMinutes.Core

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
    ]

open FabulousMinutes.Core.DynamicAccess
open FabulousMinutes.Core.DynamicAccess.Regex

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
    ]

[<Tests>]
let logger_tests =
    testList "logger tests" [
        test "Test simple dynamic access member" {
            let simpleJson = minifyJson """{"myLog": {"Timestamp": "2022.03.28 07:45:10.00949","Request": {"Path": "/api/IHelpdeskAPI/checkCaptcha","PathBase": "","Method": "POST","Host": "localhost","Port": "8085","QueryString": ""}}}"""
            let dynObjOfJson = DynamicObj.ofJson (simpleJson)
            let logger = Logger()
            dynObjOfJson.CopyDynamicPropertiesTo(logger)
            let dynamicAccessPath = logger.DynamicAccess "myLog.Request.Path" 
            let revertToJson = DynamicObj.toJson dynObjOfJson
            Expect.equal revertToJson simpleJson ""
            Expect.equal dynamicAccessPath "/api/IHelpdeskAPI/checkCaptcha" ""
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
            let simpleJson = minifyJson """{"myLog": {"Timestamp": "2022.03.28 07:45:10.00949","Request": {"Path": "/api/IHelpdeskAPI/checkCaptcha","PathBase": "","Method": "POST","Host": "localhost","Port": "8085","QueryString": ""}}}"""
            let dynObjOfJson = DynamicObj.ofJson (simpleJson)
            let logger = Logger()
            dynObjOfJson.CopyDynamicPropertiesTo(logger)
            let dynamicAccess = logger.DynamicAccess "myLog.Request.Port" 
            let tryDynamicAccess = logger.TryDynamicAccess "myLog.Request.Port"
            let dynamicAccessTyped = logger.DynamicAccess<Logger> "myLog"
            let dynamicAccessTypedInner =
                let l = Logger()
                dynamicAccessTyped.CopyDynamicPropertiesTo(l) 
                l.DynamicAccess "Request.Path"
            let revertToJson = DynamicObj.toJson dynObjOfJson
            Expect.equal revertToJson simpleJson ""
            Expect.equal dynamicAccess "8085" ""
            Expect.equal tryDynamicAccess (Some "8085") ""
            Expect.equal dynamicAccessTypedInner "/api/IHelpdeskAPI/checkCaptcha" ""
        }

    ]
