module AST


type Color =
| Red of string
| Green of string
| Blue of string
| Purple of string
type LineType = 
| Dashed of string
| Dotted of string
| Solid of string

type Num = int
type Var = char

type Val = 
| Num of int
| Var of char

type Trig = 
| Sin
| Cos
| Tan

type Func = 
| Val of Val
| Trig of Trig
| Op of Op
and Op = {first: Func; op: char; second: Func}
and Sin = Func
and Cos = Func
and Tan = Func

type Plot = {f: Func; line: LineType; color: Color}
type Bound = {lower: Num; upper:Num}
type Domain = {var: Var; bounds: Bound}
type Graph = {plots: Plot list; domain: Domain}