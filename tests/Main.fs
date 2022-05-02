module tests
open Expecto

open Expecto.Logging

[<EntryPoint>]
let main argv =
    
    printfn "Running tests!"

    Tests.runTestsInAssembly defaultConfig argv
