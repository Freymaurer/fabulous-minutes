module tests
open Expecto

open Expecto.Logging

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
