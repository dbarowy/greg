namespace GregTests

open Parser
open AST
open Evaluator
open System
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClass () = 

    [<TestMethod>]
    member this.ValidProgramParsesToAnAST() = 
        let input = "Plot (3 + Sin(x)), Solid, Blue. x from -8 to 8."
        let expected = { plots = [{ f = Op (Plus { first = Val (Num 3)
                                                           second = Trig (Sin (Val (Var 'x'))) })
                                        line = Solid "Solid"
                                        color = Blue "Blue" }]
                                 domain = { var = 'x'
                                            bounds = { lower = -8
                                                       upper = 8 } } }
        let result = parse input
        match result with 
        | Some ws -> Assert.AreEqual(expected, ws)
        | None -> Assert.IsTrue false

    [<TestMethod>]
    member this.ValidProgramWritesAnSVG() = 
        let input = "Plot 1, Dotted, Black. x from -2 to 2."
        let expected = "<svg width=\"500\" height=\"500\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">
 <style> .axisLabel {font: bold 20px sans-serif;} </style>
 <rect x=\"10\" y=\"10\" rx=\"10\" ry=\"10\" width=\"480\" height=\"480\" style=\"fill:rgb(232,232,232);stroke-width:2;stroke:rgb(0,0,0)\"/>
 <line x1=\"20\" x2=\"480\" y1=\"250\" y2=\"250\" stroke=\"black\" stroke-width=\"4\"/>
<line x1=\"480\" x2=\"480\" y1=\"240\" y2=\"260\" stroke=\"black\" stroke-width=\"2\"/>
<text x=\"475\" y=\"285\" class=\"axisLabel\">2</text>
<line x1=\"365\" x2=\"365\" y1=\"240\" y2=\"260\" stroke=\"black\" stroke-width=\"2\"/>
<text x=\"360\" y=\"285\" class=\"axisLabel\">1</text>
<line x1=\"250\" x2=\"250\" y1=\"20\" y2=\"480\" stroke=\"black\" stroke-width=\"4\"/>
<line x1=\"135\" x2=\"135\" y1=\"240\" y2=\"260\" stroke=\"black\" stroke-width=\"2\"/>
<text x=\"130\" y=\"285\" class=\"axisLabel\">-1</text>
<line x1=\"20\" x2=\"20\" y1=\"240\" y2=\"260\" stroke=\"black\" stroke-width=\"2\"/>
<text x=\"15\" y=\"285\" class=\"axisLabel\">-2</text>
<line x1=\"240\" x2=\"260\" y1=\"480\" y2=\"480\" stroke=\"black\" stroke-width=\"2\"/>
<text x=\"285\" y=\"485\" class=\"axisLabel\">-2</text>
<line x1=\"240\" x2=\"260\" y1=\"365\" y2=\"365\" stroke=\"black\" stroke-width=\"2\"/>
<text x=\"285\" y=\"370\" class=\"axisLabel\">-1</text>
<line x1=\"240\" x2=\"260\" y1=\"135\" y2=\"135\" stroke=\"black\" stroke-width=\"2\"/>
<text x=\"285\" y=\"140\" class=\"axisLabel\">1</text>
<line x1=\"240\" x2=\"260\" y1=\"20\" y2=\"20\" stroke=\"black\" stroke-width=\"2\"/>
<text x=\"285\" y=\"25\" class=\"axisLabel\">2</text>

<circle cx=\"20\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"27.999999999999943\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"35.999999999999915\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"43.99999999999986\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"51.99999999999983\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"59.99999999999977\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"67.99999999999972\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"75.99999999999969\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"83.99999999999963\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"91.99999999999957\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"99.99999999999955\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"107.99999999999949\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"115.99999999999943\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"123.9999999999994\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"131.99999999999935\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"139.99999999999932\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"147.99999999999926\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"155.9999999999992\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"163.99999999999918\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"171.99999999999912\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"179.9999999999991\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"187.99999999999903\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"195.99999999999898\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"203.99999999999892\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"211.9999999999989\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"219.99999999999883\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"227.99999999999883\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"235.99999999999886\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"243.99999999999886\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"251.99999999999886\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"259.99999999999886\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"267.99999999999886\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"275.99999999999886\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"283.99999999999886\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"291.9999999999988\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"299.99999999999875\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"307.9999999999987\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"315.99999999999864\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"323.99999999999864\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"331.9999999999986\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"339.9999999999985\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"347.99999999999847\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"355.9999999999984\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"363.9999999999984\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"371.99999999999835\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"379.9999999999983\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"387.9999999999983\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"395.9999999999982\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"403.9999999999982\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"411.9999999999981\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"419.99999999999807\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"427.99999999999807\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"435.99999999999795\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"443.99999999999795\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"451.9999999999979\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"459.99999999999784\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"467.9999999999978\" cy=\"135\" r=\"3\" fill=\"black\"/>
<circle cx=\"475.9999999999977\" cy=\"135\" r=\"3\" fill=\"black\"/>
</svg>
"

        let result = parse input
        let svg = match result with
                        | Some g -> eval g
                        | None -> ""
        Assert.AreEqual(expected, svg)
