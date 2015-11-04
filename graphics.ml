#load "graphics.cma";;
#load "unix.cma";;
#use "main.ml";;

open Graphics;;
close_graph();;
open_graph "";;

set_line_width 1;;
set_color black;;

let rec draw_points (points:point_set) = match points with
         [] -> ();
        |h::t -> draw_circle (int_of_float  h.x) (int_of_float h.y) 1;
                 draw_points t;;
    
let rec draw_triangles (triangles:triangle_set) = match triangles with
         [] -> ()
        |h::t -> draw_poly [|(int_of_float (h.p1.x),int_of_float(h.p1.y));
                             (int_of_float (h.p2.x),int_of_float(h.p2.y));
                             (int_of_float (h.p3.x),int_of_float(h.p3.y))|];
                 draw_triangles t;;
                 
let delaunay_step_by_step points max_x max_y =
    let pop l = match !l with
	[] -> failwith "liste vide"
       |h::t -> l := t;
	        h in
    let delaunay_set = ref (start_triangles max_x max_y)
    and points_to_add = ref points in
    while !points_to_add <> [] do
        draw_triangles !delaunay_set;
        Unix.sleep 1;
	clear_graph();
        delaunay_set := add_point (!delaunay_set) (pop points_to_add);
    done;
    draw_triangles (!delaunay_set);;
