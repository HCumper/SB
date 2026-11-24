open System
open System.IO
open System.Reflection
open System.Runtime.Loader
let bin = @"C:\Source\SB\SB\bin\Debug\net10.0"
AppContext.SetData("APP_CONTEXT_BASE_DIRECTORY",bin)
AssemblyLoadContext.Default.add_Resolving(fun _ name ->
    let candidate=Path.Combine(bin,name.Name+".dll")
    if File.Exists candidate then AssemblyLoadContext.Default.LoadFromAssemblyPath candidate else null)
let assembly=AssemblyLoadContext.Default.LoadFromAssemblyPath @"C:\Source\codex-workspace\.scratch\turbo-local-for\baseline-SB.dll"
let args=fsi.CommandLineArgs[1..]
let result=assembly.EntryPoint.Invoke(null,[|box args|]) :?> int
exit result
