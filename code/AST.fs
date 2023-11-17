module AST


type Color =
| Red
| Green
| Blue
| Purple
type LineType = 
| Dashed
| Dotted
| Solid

type Num = int
type Var = char

// type Val = 
// | Num of int
// | Var of char

type Trig = 
| Sin
| Cos
| Tan
type Function = 
// | Val of Val
| Trig of Trig
| Op of Op
and  Op = {first: Function; second: Function}
and Sin = Function
and Cos = Function
and Tan = Function

type Plot = {f: Function; line: LineType; color: Color}
type Bound = {lower: Num; upper:Num}
type Domain = {var: Var; bounds: Bound}
type Graph = {plots: Plot list; domain: Domain}