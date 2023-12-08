module Evaluator

open AST


// global parameters 
let CANVAS_SZ = 500.0
let PADDING = 10.0
// the lower this value is, the better the RESOLUTION gets
let RESOLUTION: float = 0.5

// start of dynamic implimentation

//some temperary function to test implimentation
let temp_function (x: float) = (sin(x) - 1.0)
let temp_function2 (x: float) = (cos(x) + 0.5)



let rec draw_function (funct: float -> float, domain: Domain, cur_value: float, color: string, line: string, iter: int) : string = 
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

    let is_dash = ((iter % int(16.0/(RESOLUTION*2.0))) - 8) >= 0
    let is_dot = (iter % int(9.0/(RESOLUTION*2.0))) = 0

    match line, over_max, is_dash, is_dot with 
    | _, true, _, _  -> ""
    | "Solid", false, _, _ -> "<line x1=\"" + (starting_point_x_cc |> string) + "\" x2=\"" + (ending_point_x_cc |> string) + "\" y1=\"" + (starting_point_y_cc |> string) + "\" y2=\"" + (ending_point_y_cc |> string) + "\" stroke=\"" + color + "\" stroke-width=\"5\"/>\n" + 
                                draw_function (funct, domain, cur_value + (RESOLUTION/scaling_factor), color, line, iter+1)
    | "Dashed", false, true, _ -> "<line x1=\"" + (starting_point_x_cc |> string) + "\" x2=\"" + (ending_point_x_cc |> string) + "\" y1=\"" + (starting_point_y_cc |> string) + "\" y2=\"" + (ending_point_y_cc |> string) + "\" stroke=\"" + color + "\" stroke-width=\"5\"/>\n" + 
                                    draw_function (funct, domain, cur_value + (RESOLUTION/scaling_factor), color, line, iter+1)
    | "Dashed", false, _, _ -> draw_function (funct, domain, cur_value + (RESOLUTION/scaling_factor), color, line, iter+1)
    | "Dotted", false, _, true -> "<circle cx=\"" + (starting_point_x_cc |> string) + "\" cy=\"" + (starting_point_y_cc |> string) + "\" r=\"3\" fill=\"" + color + "\"/>\n" + 
                                    draw_function (funct, domain, cur_value + (RESOLUTION/scaling_factor), color, line, iter+1)
    | "Dotted", false, _, _ -> draw_function (funct, domain, cur_value + (RESOLUTION/scaling_factor), color, line, iter+1)
    | _, _, _, _ -> "This shouldn't get here - somthing went wrong..."

//brute force
let tick_scaler (n: int): int = 
    match n with
    | x when x <= 12 -> 1
    | x when x <= 24 -> 2
    | x when x <= 60 -> 5
    | x when x <= 120 -> 10
    | x when x <= 240 -> 20
    | x when x <= 600 -> 50
    | x when x <= 1200 -> 100
    | x when x <= 2400 -> 200
    | x when x <= 6000 -> 500
    | x when x <= 12000 -> 1000
    | x when x <= 24000 -> 2000
    | x when x <= 60000 -> 5000
    | x when x <= 120000 -> 10000
    | x when x <= 240000 -> 20000
    | x when x <= 600000 -> 50000
    | x when x <= 1200000 -> 100000
    | _ -> 100000
    

// dynamic positioning of ticks on y axis
// TODO: fix positioning of numbers in relation to axis when y-axis on far right side
let rec yticks (domain: Domain, num_remaining: int, xpos: float) : string = 
    // local variable calculation to use for positioning
    let line_length = CANVAS_SZ - float(4.0*PADDING)
    let dom_length = domain.bounds.upper - domain.bounds.lower
    let tick_scale = tick_scaler dom_length
    let cur_num = (-((dom_length)/2) |> int) + num_remaining
    let num = 2 * (((dom_length)/2) |> int)
    let scale = line_length / float(num)
    let tick_length = 20.0
    let ypos = (float(num_remaining) * scale + 2.0*PADDING) 
    let xpos_start = (xpos - (tick_length / 2.0)) |> string
    let xpos_end   = (xpos + (tick_length / 2.0)) 
    let num_offset = 25.0
    match num_remaining, cur_num with
    | -1, _ -> ""
    | _, 0 -> yticks(domain, (num_remaining - 1), xpos)
    | _, x when x%tick_scale = 0 -> "<line x1=\"" + xpos_start + "\" x2=\"" + (xpos_end |> string) + "\" y1=\"" + (ypos |> string) + "\" y2=\"" + (ypos |> string) + "\" stroke=\"black\" stroke-width=\"2\"/>\n" + 
                                    "<text x=\"" + ((xpos_end + num_offset) |> string) + "\" y=\"" + ((ypos + 5.0) |> string) + "\" class=\"axisLabel\">" + (-cur_num |> string) + "</text>\n" + 
                                    yticks(domain, (num_remaining - 1), xpos)
    | _, _ -> yticks(domain, (num_remaining - 1), xpos)

let rec tickMarks (domain: Domain, num_remaining: int) : string = 
    let line_length = CANVAS_SZ - (4.0*PADDING)
    let num = domain.bounds.upper - domain.bounds.lower
    let tick_scale = tick_scaler num
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
    | _, x when x%tick_scale = 0 -> "<line x1=\"" + (xpos |> string) + "\" x2=\"" + (xpos |> string) + "\" y1=\"" + ypos_start + "\" y2=\"" + (ypos_end |> string) + "\" stroke=\"black\" stroke-width=\"2\"/>\n" + 
                                    "<text x=\"" + ((xpos - 5.0) |> string) + "\" y=\"" + ((ypos_end + num_offset) |> string) + "\" class=\"axisLabel\">" + (cur_num |> string) + "</text>\n" + 
                                    tickMarks (domain, (num_remaining - 1))
    | _, _ -> tickMarks (domain, (num_remaining - 1))

let eval_domain (domain: Domain) : string =
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
    (tickMarks (domain, (domain.bounds.upper - domain.bounds.lower)))
    // generate function
    // (draw_function (temp_function, domain, domain.bounds.lower, )) +


let eval_color (color: Color) : string = 
    match color with
    | Red "Red" -> "red"
    | Green "Green" -> "green"
    | Blue "Blue" -> "blue"
    | Purple "Purple" -> "purple"
    | Pink "Pink" -> "pink"
    | Gray "Gray" -> "gray"
    | Black "Black" -> "black"
    | Yellow "Yellow" -> "yellow"
    | Orange "Orange" -> "orange"
    | RGB (a,b,c) -> "rgb(" + (a |> string) + "," + (b |> string) + "," + (c |> string) + ")"
    | _ -> "black"

let eval_line (line: LineType) : string = 
    match line with
    | Dashed "Dashed" -> "Dashed"
    | Solid "Solid" -> "Solid"
    | Dotted "Dotted" -> "Dotted"
    | _ -> "Solid"

let eval_val (v: Val) : float -> float =
    match v with
    | Num n -> (fun x -> n |> float)
    | Var z -> (fun x ->  x)

let rec eval_func (func: Func) : float -> float = 
    match func with
    | Val v -> eval_val v
    | Trig t -> eval_trig t
    | Op o -> eval_op o

and eval_sin(f: Func, n: float): float = 
    let g = eval_func(f) n
    sin(g)

and eval_cos(f: Func, n: float): float = 
    let g = eval_func(f) n
    cos(g)

and eval_tan(f: Func, n: float): float = 
    let g = eval_func(f) n
    tan(g)

and eval_trig (t: Trig) : float -> float = 
    match t with
    | Sin f -> (fun x -> eval_sin(f, x))
    | Cos f -> (fun x -> eval_cos(f, x))
    | Tan f -> (fun x -> eval_tan(f, x))



and eval_plus(t:TwoOp, n: float) : float = 
    let h = eval_func(t.first) n
    let j = eval_func(t.second) n
    h+j
and eval_minus(t:TwoOp, n: float) : float = 
    let h = eval_func(t.first) n
    let j = eval_func(t.second) n
    h-j
and eval_times(t:TwoOp, n: float) : float = 
    let h = eval_func(t.first) n
    let j = eval_func(t.second) n
    h*j
and eval_div(t:TwoOp, n: float) : float = 
    let h = eval_func(t.first) n
    let j = eval_func(t.second) n
    h/j
and eval_exp(t:TwoOp, n: float) : float = 
    let h = eval_func(t.first) n
    let j = eval_func(t.second) n
    h**j

and eval_op (o: Op) : float -> float = 
    match o with
    | Plus t -> (fun x -> eval_plus(t,x))
    | Minus t -> (fun x -> eval_minus(t,x))
    | Times t -> (fun x -> eval_times(t,x))
    | Div t -> (fun x -> eval_div(t,x))
    | Exp t -> (fun x -> eval_exp(t,x))    

let eval_plot (plot: Plot, graph: Graph) : string = 
    draw_function (eval_func (plot.f), graph.domain, graph.domain.bounds.lower, eval_color plot.color, eval_line plot.line, 0)

let rec eval_plots (plots: Plot list, graph: Graph) : string = 
    match plots with
    | [] -> ""
    | x::xs -> eval_plot (x, graph) + eval_plots (xs, graph)

let rec eval (graph: Graph) : string = 
    eval_domain graph.domain + "\n" + eval_plots (graph.plots, graph) + "</svg>\n"
    // + eval_functions graph.plots
    

// let eval_test (graph: Graph) : string = 
//     graph.plots[0].f |> string