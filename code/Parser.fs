module Parser

open AST
open Combinator
open System

let posnum = pmany1 pdigit |>> (fun ds -> stringify ds |> int) 
let num = pright (pchar('-')) posnum |>> (fun a -> -a) <|> posnum

let pad p = pbetween pws0 p pws0

let var = pletter |>> (fun v -> Var v)

let domain = 
    pseq
        (var)
        (pseq
            (pright
                (pad (pstr "from"))
                (num))
            (pright
                (pad (pstr "to"))
                (num))
            (fun(a,b) -> (a,b)))
        (fun(x,(b,c)) -> {var: x; lower: b; upper:c})



let graph = domain

let grammar = pleft graph peof

let parse (input: string) : Graph option =
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_,_) -> None