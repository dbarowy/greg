module Parser

open AST
open Combinator
open System

let posnum = pmany1 pdigit |>> (fun ds -> stringify ds |> int) 
let num = pright (pchar('-')) posnum |>> (fun a -> -a) <|> posnum

let pad p = pbetween pws0 p pws0

let var = pletter |>> (fun v ->  v)

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
            (fun(a,b: int) -> { lower= a; upper= b }))
        (fun(a,b) -> { var= a; bounds= b })


let grammar = pleft domain peof

let parse (input: string) : Domain option =
    let i = prepare input
    match grammar i with
    | Success(ast: Domain, _) -> Some ast
    | Failure(_,_) -> None