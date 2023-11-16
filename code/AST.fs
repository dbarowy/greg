module AST

type num = int
type var = char
type Color =
| Red
| Green
| Blue
| Purple
type Function = 
| Polynomial of Polynomial
| Trig of Trig
| Op of {first: Function; second: Function}
type Polynomial = 
| num
| var
type Plot = {f: Function; line: LineType; color: Color }
type Graph = {plots: Plot list; domain: Domain}