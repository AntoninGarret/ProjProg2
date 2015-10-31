#load "graphics.cma";;
open Graphics;;
close_graph();;
open_graph " 800x600_0+0";;

#use "main.ml";;

let draw_points (points:point_set) = 
    let rec aux = function
         [] -> ();
        |h::t -> plot (int_of_float  h.x) (int_of_float h.y);
                 aux t in
    set_color red;
    set_line_width 2;
    aux points;;
    
let draw_triangles (triangles:triangle_set) =
    let rec aux = function
         [] -> ()
        |h::t -> moveto (int_of_float h.p1.x) (int_of_float h.p1.y);
                 rlineto (int_of_float h.p2.x) (int_of_float h.p2.y);
                 rlineto (int_of_float h.p3.x) (int_of_float h.p3.y);
                 rlineto (int_of_float h.p1.x) (int_of_float h.p1.y);
                 aux t in
    set_color black;
    set_line_width 1;
    aux triangles;;
