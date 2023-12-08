module AST

type Num = int
type Var = char
type Blue = string

type Color =
| Red of string
| Green of string
| Blue of string
| Purple of string
| Pink of string
| Gray of string
| Black of string
| Yellow of string
| Orange of string
| RGB of Num*Num*Num

type LineType = 
| Dashed of string
| Dotted of string
| Solid of string

type Val = 
| Num of int
| Var of char

type Func = 
| Val of Val
| Trig of Trig
| Op of Op
and TwoOp = {first: Func; second: Func}
and Op = 
| Plus of TwoOp
| Minus of TwoOp
| Times of TwoOp
| Div of TwoOp
| Exp of TwoOp
and Trig = 
| Sin of Func
| Cos of Func
| Tan of Func


type Plot = {f: Func; line: LineType; color: Color}
type Bound = {lower: Num; upper:Num}
type Domain = {var: Var; bounds: Bound}
type Graph = {plots: Plot list; domain: Domain}