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
            (pbetween
                (pad (pstr "to"))
                (num)
                (pchar '.'))
            (fun(a,b: int) -> { lower= a; upper= b }))
        (fun(a,b) -> { var= a; bounds= b })

let p3seq(p1: Parser<'a>)(p2: Parser<'b>)(p3: Parser<'c>)(f: 'a*'b*'c -> 'd) : Parser<'d> =
    pbind p1 (fun a ->
        pbind p2 (fun b ->
            pbind p3 (fun c -> 
                presult (f (a,b,c))
            )
        )
    )

let Func, FuncImpl = recparser()

let pVal = 
    ((num |>> (fun num -> Num num))
    <|>
    (var |>> (fun var -> Var var))) |>> (fun v -> Val v)

let plus = pchar '+' <!> "plus"
let minus = pchar '-' <!> "minus"
let times = pchar '*' <!> "times"
let div = pchar '/' <!> "div"
let exp = pchar '^' <!> "exp"

let op = plus <|> minus <|> times <|> div <|> exp

let whichOp (left: Func, mid: char, right:Func) = 
    match mid with
    | '+' -> Op(Plus (left, right))
    | '-' -> Op(Minus (left, right))
    | '*' -> Op(Times (left, right))
    | '/' -> Op(Div (left, right))
    | '^' -> Op(Exp (left, right))
    | _ -> failwith"invalid expression"

let pOp = 
    pbetween
        (pchar '(')
        (p3seq
            (Func)
            (op)
            (Func)
            (whichOp))
        (pchar ')')

let pSin = 
    pright
        (pstr "Sin")
        Func
    |>> fun f -> Trig(Sin(f))
let pCos = 
    pright
        (pstr "Cos")
        Func
    |>> fun f -> Trig(Cos(f))

let pTan = 
    pright
        (pstr "Tan")
        Func
    |>> fun (f: Func) -> Trig(Tan(f))

let Trig = pSin <|> pCos <|> pTan

let parens = 
    pbetween
        (pchar '(')
        Func
        (pchar ')')

FuncImpl := Trig <|> pVal <|> pOp <|> parens

let pRed = pstr("Red") |>> (fun v -> Red v)
let pGreen = pstr("Green")  |>> (fun v -> Green v)
let pBlue = pstr("Blue")  |>> (fun v -> Blue v)
let pPurple = pstr("Purple")  |>> (fun v -> Purple v)

let pColor = pRed <|> pBlue <|> pGreen <|> pPurple

let pDashed = pstr("Dashed") |>> (fun v -> Dashed v)
let pDotted = pstr("Dotted") |>> (fun v -> Dotted v)
let pSolid = pstr("Solid") |>> (fun v -> Solid v)

let pLineType = pDashed <|> pDotted <|> pSolid


let plot = 
    p3seq
        (pbetween
            (pstr "Plot ")
            Func
            (pad (pchar ',')))
        (pleft pLineType (pad (pchar ',')))
        (pleft pColor (pad (pchar '.')))
        (fun(a,b,c) -> {f= a; line= b; color=c})

let plots = pmany1 plot

let graph = 
    pseq
        (plots)
        (domain)
        (fun (a,b) -> {plots= a; domain = b})

let grammar = pleft graph peof

let parse (input: string) : Graph option =
    let i = debug input
    match grammar i with
    | Success(ast: Graph, _) -> Some ast
    | Failure(_,_) -> None