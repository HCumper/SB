#r @"C:\Source\SB\SB\bin\Debug\net10.0\SB.dll"
#r @"C:\Source\SB\SBLib\bin\Debug\net10.0\SBLib.dll"
#r @"C:\Source\SB\SBTests\bin\Debug\net10.0\SBTests.dll"
#r @"C:\Source\SB\SBRuntimeTests\bin\Debug\net10.0\SBRuntimeTests.dll"
open SBRuntimeTests.TestSupport
open CompilerPipeline
open AstToHir
open Interpreter
let ast = parseAstFromFile "Golfer.sb"
let hir = lowerProgram ast
match interpretProgramWithOptions defaultRuntimeOptions hir with
| Result.Ok result -> printfn "OK %d" result.Output.Length
| Result.Error err -> printfn "ERR %A" err
