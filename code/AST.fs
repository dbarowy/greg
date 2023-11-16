module AST

type Num = int
type Var = char
type Color =
| Red
| Green
| Blue
| Purple
type LineType = 
| Dashed
| Dotted
| Solid

type Val = 
| Num
| Var
type Trig = 
| Sin
| Cos
| Tan
type Function = 
| Val of Val
| Trig of Trig
| Op of Op
and  Op = {first: Function; second: Function}
and Sin = Function
and Cos = Function
and Tan = Function

type Plot = {f: Function; line: LineType; color: Color}
type Domain = {var: Var; lower: Num; upper: Num}
type Graph = {plots: Plot list; domain: Domain}