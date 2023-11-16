module Evaluator

open AST

// start of dynamic implimentation
let rec evalCanvas (canvas: Canvas) : string =
    match canvas with
    | [] -> ""
    | l::ls -> (evalLine l) + (evalCanvas ls)

// all the nessesary static implimentation
let eval (canvas: Canvas) : string =
    let csz = CANVAS_SZ |> string
    "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
    " <style> .axisLabel {font: bold 20px sans-serif;} </style>\n" + 
    " <rect x=\"10\" y=\"10\" rx=\"10\" ry=\"10\" width=\"" + csz + " height=\"" + csz + "\" style=\"fill:rgb(232,232,232);stroke-width:2;stroke:rgb(0,0,0)\"/>\n" +
    // (evalCanvas canvas)
    + "</svg>\n"