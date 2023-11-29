module Evaluator

open AST


// global parameters 
let CANVAS_SZ = 500.0
let PADDING = 10.0
// the lower this value is, the better the RESOLUTION gets
let RESOLUTION: float = 1.0

// start of dynamic implimentation

//some temperary function to test implimentation
let temp_function (x: float) = (sin(x))

let rec draw_function (funct: float -> float, domain: Domain, cur_value: float) : string = 
    let scaling_factor = (CANVAS_SZ - (4.0*PADDING))/float(domain.bounds.upper - domain.bounds.lower)
    let origin_cc_x    = (2.0*PADDING) - (scaling_factor*float(domain.bounds.lower))
    let origin_cc_y    = CANVAS_SZ / 2.0

    let starting_point_x_gc = cur_value
    let starting_point_y_gc = funct starting_point_x_gc
    let starting_point_x_cc = scaling_factor * starting_point_x_gc + origin_cc_x
    let starting_point_y_cc = -scaling_factor * starting_point_y_gc + origin_cc_y

    let ending_point_x_cc   = starting_point_x_cc + RESOLUTION
    let ending_point_x_gc   = (ending_point_x_cc - origin_cc_x)/ scaling_factor
    let ending_point_y_gc   = funct ending_point_x_gc
    let ending_point_y_cc   = -ending_point_y_gc * scaling_factor + origin_cc_y

    let over_max = cur_value + (RESOLUTION/scaling_factor) >= domain.bounds.upper
    match over_max with 
    | true  -> ""
    | false -> "<line x1=\"" + (starting_point_x_cc |> string) + "\" x2=\"" + (ending_point_x_cc |> string) + "\" y1=\"" + (starting_point_y_cc |> string) + "\" y2=\"" + (ending_point_y_cc |> string) + "\" stroke=\"blue\" stroke-width=\"5\"/>\n" + 
                draw_function (funct, domain, cur_value + (RESOLUTION/scaling_factor))

// dynamic positioning of ticks on y axis
// TODO: fix positioning of numbers in relation to axis when y-axis on far right side
let rec yticks (domain: Domain, num_remaining: int, xpos: float) : string = 
    // local variable calculation to use for positioning
    let line_length = CANVAS_SZ - float(4.0*PADDING)
    let cur_num = (-((domain.bounds.upper - domain.bounds.lower)/2) |> int) + num_remaining
    let num = 2 * (((domain.bounds.upper - domain.bounds.lower)/2) |> int)
    let scale = line_length / float(num)
    let tick_length = 20.0
    let ypos = (float(num_remaining) * scale + 2.0*PADDING) 
    let xpos_start = (xpos - (tick_length / 2.0)) |> string
    let xpos_end   = (xpos + (tick_length / 2.0)) 
    let num_offset = 25.0
    match num_remaining, cur_num with
    | -1, _ -> ""
    | _, 0 -> yticks(domain, (num_remaining - 1), xpos)
    | _, _ -> "<line x1=\"" + xpos_start + "\" x2=\"" + (xpos_end |> string) + "\" y1=\"" + (ypos |> string) + "\" y2=\"" + (ypos |> string) + "\" stroke=\"black\" stroke-width=\"2\"/>\n" + 
              "<text x=\"" + ((xpos_end + num_offset) |> string) + "\" y=\"" + ((ypos + 5.0) |> string) + "\" class=\"axisLabel\">" + (-cur_num |> string) + "</text>\n" + 
              yticks(domain, (num_remaining - 1), xpos)


let rec tickMarks (domain: Domain, num_remaining: int) : string = 
    let line_length = CANVAS_SZ - (4.0*PADDING)
    let num = domain.bounds.upper - domain.bounds.lower
    let cur_num = domain.bounds.lower + num_remaining
    let scale = line_length / float(num)
    let tick_length = 20.0 
    let xpos = (float(num_remaining) * scale + 2.0*PADDING) 
    let ypos_start = ((CANVAS_SZ / 2.0) - (tick_length / 2.0)) |> string
    let ypos_end   = ((CANVAS_SZ / 2.0) + (tick_length / 2.0)) 
    let num_offset = 25.0
    match num_remaining, cur_num with
    | -1, _ -> ""
    | _, 0 -> "<line x1=\"" + (xpos |> string) + "\" x2=\"" + (xpos |> string) + "\" y1=\"" + ((2.0 * PADDING) |> string) + "\" y2=\"" + ((CANVAS_SZ - (2.0 * PADDING)) |> string) + "\" stroke=\"black\" stroke-width=\"4\"/>\n" + 
              tickMarks (domain, (num_remaining - 1)) + 
              yticks(domain, num, xpos)
    | _, _ -> "<line x1=\"" + (xpos |> string) + "\" x2=\"" + (xpos |> string) + "\" y1=\"" + ypos_start + "\" y2=\"" + (ypos_end |> string) + "\" stroke=\"black\" stroke-width=\"2\"/>\n" + 
              "<text x=\"" + ((xpos - 5.0) |> string) + "\" y=\"" + ((ypos_end + num_offset) |> string) + "\" class=\"axisLabel\">" + (cur_num |> string) + "</text>\n" + 
              tickMarks (domain, (num_remaining - 1))

    
let eval (domain: Domain) : string =
    let cszi = CANVAS_SZ 
    let csz = CANVAS_SZ |> string
    let lower = PADDING |> string
    let upper = (cszi - 2.0*PADDING) |> string
    // all the nessesary static implimentation
    "<svg width=\"" + csz + "\" height=\"" + csz + "\"" +
    " xmlns=\"http://www.w3.org/2000/svg\"" +
    " xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n" +
    " <style> .axisLabel {font: bold 20px sans-serif;} </style>\n" + 
    " <rect x=\"" + lower + "\" y=\"" + lower + "\" rx=\"10\" ry=\"10\" width=\"" + upper + "\" height=\"" + upper + "\" style=\"fill:rgb(232,232,232);stroke-width:2;stroke:rgb(0,0,0)\"/>\n" +
    " <line x1=\"" + ((2.0*PADDING) |> string) + "\" x2=\"" + upper + "\" y1=\"" + ((cszi / 2.0) |> string) + "\" y2=\"" + ((cszi / 2.0) |> string) + "\" stroke=\"black\" stroke-width=\"4\"/>\n" +
    // generate tick parks, numbers and y-axis
    (tickMarks (domain, (domain.bounds.upper - domain.bounds.lower))) +
    // generate function
    (draw_function (temp_function, domain, domain.bounds.lower)) +
    "</svg>\n"