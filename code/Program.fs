open Evaluator
open System
open Parser

// Basic program: reads input file if you type it in command line (input.txt)
// Can pipe into an svg file so see the result rather than just the code which leads to the result

[<EntryPoint>]
let main args =
        let file = args[0]
        let text = IO.File.ReadAllText file
        match parse text with
        | Some ast ->
                let svg = eval ast
                printfn "%s" svg
                0
        | None ->
                printfn "Invalid program."
                1