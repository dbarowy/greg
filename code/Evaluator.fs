module Evaluator

open AST

// start of dynamic implimentation
// let rec evalGraph (domain: Domain) : string =
//     match domain with
//     | [] -> ""
//     | l::ls -> (evalLine l) + (evalCanvas ls)
let CANVAS_SZ = 500
let padding = 10

let rec yticks (domain: Domain, num_remaining: int, xpos: int) : string = 
    let line_length = CANVAS_SZ - (4*padding)
    let cur_num = (-((domain.bounds.upper - domain.bounds.lower)/2) |> int) + num_remaining
    let num = 2 * (((domain.bounds.upper - domain.bounds.lower)/2) |> int)
    let scale = line_length / num
    let tick_length = 20 
    let ypos = (num_remaining * scale + 2*padding) 
    let xpos_start = (xpos - (tick_length / 2)) |> string
    let xpos_end   = (xpos + (tick_length / 2)) 
    let num_offset = 25
    //printfn "%A" num
    match num_remaining, cur_num with
    | -1, _ -> ""
    | _, 0 -> yticks(domain, (num_remaining - 1), xpos)
    | _, _ -> "<line x1=\"" + xpos_start + "\" x2=\"" + (xpos_end |> string) + "\" y1=\"" + (ypos |> string) + "\" y2=\"" + (ypos |> string) + "\" stroke=\"black\" stroke-width=\"2\"/>\n" + 
              "<text x=\"" + ((xpos_end + num_offset) |> string) + "\" y=\"" + ((ypos + 5) |> string) + "\" class=\"axisLabel\">" + (-cur_num |> string) + "</text>\n" + 
              yticks(domain, (num_remaining - 1), xpos)

// all the nessesary static implimentation
let rec tickMarks (domain: Domain, num_remaining: int) : string = 
    let line_length = CANVAS_SZ - (4*padding)
    let num = domain.bounds.upper - domain.bounds.lower
    let cur_num = domain.bounds.lower + num_remaining
    let scale = line_length / num
    let tick_length = 20 
    let xpos = (num_remaining * scale + 2*padding) 
    let ypos_start = ((CANVAS_SZ / 2) - (tick_length / 2)) |> string
    let ypos_end   = ((CANVAS_SZ / 2) + (tick_length / 2)) 
    let num_offset = 25
    //printfn "%A" num
    match num_remaining, cur_num with
    | -1, _ -> ""
    | _, 0 -> "<line x1=\"" + (xpos |> string) + "\" x2=\"" + (xpos |> string) + "\" y1=\"" + ((2 * padding) |> string) + "\" y2=\"" + ((CANVAS_SZ - (2 * padding)) |> string) + "\" stroke=\"black\" stroke-width=\"4\"/>\n" + 
              tickMarks (domain, (num_remaining - 1)) + 
              yticks(domain, num, xpos)
    | _, _ -> "<line x1=\"" + (xpos |> string) + "\" x2=\"" + (xpos |> string) + "\" y1=\"" + ypos_start + "\" y2=\"" + (ypos_end |> string) + "\" stroke=\"black\" stroke-width=\"2\"/>\n" + 
              "<text x=\"" + ((xpos - 5) |> string) + "\" y=\"" + ((ypos_end + num_offset) |> string) + "\" class=\"axisLabel\">" + (cur_num |> string) + "</text>\n" + 
              tickMarks (domain, (num_remaining - 1))

    


let eval (domain: Domain) : string =
    let cszi = CANVAS_SZ 
    let csz = CANVAS_SZ |> string
    let lower = padding |> string
    let upper = (cszi - 2*padding) |> string
    //printfn "%A" domain.bounds.lower
    "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
    " <style> .axisLabel {font: bold 20px sans-serif;} </style>\n" + 
    " <rect x=\"" + lower + "\" y=\"" + lower + "\" rx=\"10\" ry=\"10\" width=\"" + upper + "\" height=\"" + upper + "\" style=\"fill:rgb(232,232,232);stroke-width:2;stroke:rgb(0,0,0)\"/>\n" +
    " <line x1=\"" + ((2*padding) |> string) + "\" x2=\"" + upper + "\" y1=\"" + ((cszi / 2) |> string) + "\" y2=\"" + ((cszi / 2) |> string) + "\" stroke=\"black\" stroke-width=\"4\"/>\n" +
    (tickMarks (domain, (domain.bounds.upper - domain.bounds.lower))) +
    "</svg>\n"