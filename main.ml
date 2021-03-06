(* functions that are not detailed match the description given by the subject *)

type point = {x : float ; y : float};;
type triangle = {p1 : point ; p2 : point ; p3 : point};;
type point_set = point list;;
type triangle_set = triangle list;;

let make_point x y = {x = x; y =y};;
let make_triangle p1 p2 p3 = {p1 = p1; p2 = p2; p3 = p3};;

let rec random nb max_x max_y = match nb with
  |0->[]
  |_-> (make_point (float(Random.int(max_x))) (float(Random.int(max_y))))::(random (nb-1) max_x max_y);;

let del_first_column mat =
(* 'a array array -> 'a array array
   deletes the first column of the given matrix, used later to calcuate determinant *)
  let n = Array.length mat in
  let a = Array.make_matrix n (n-1) 0. in
  for i = 0 to n-1 do 
    a.(i)<-Array.sub mat.(i) 1 (n-1)
  done;
  a;;

let del_line i mat =
(* 'a array array -> 'a array array
deletes the first line of given matrix *)
  let n = Array.length mat in
  let a = Array.make_matrix (n-1) (n-1) 0. in
  for j=0 to i-1 do
    a.(j)<-mat.(j)
  done;
  for j=i to n-2 do
    a.(j)<-mat.(j+1)
  done;
  a;;

let rec det mat = match Array.length mat with
(* float array array -> float
  calculates the determinant of given matrix *)
  |1-> mat.(0).(0)
  |n-> let r = ref 0. in
       for i = 0 to n-1 do
	 if i mod 2 = 0 then 
	   r:= !r+.mat.(i).(0)*.det (del_line i (del_first_column mat))
	 else
	   r:= !r-.mat.(i).(0)*.det (del_line i (del_first_column mat))
       done;
       !r;;

let ccw a b c = 
  det [|[|b.x-.a.x;c.x-.a.x|];[|b.y-.a.y;c.y-.a.y|]|] >= 0. ;;

let in_circle triangle point = 
  let a=triangle.p1 and b=triangle.p2 and c=triangle.p3 in
  let mat1 = [|[|a.x;a.y;a.x**2.+.a.y**2.;1.|];
	       [|b.x;b.y;b.x**2.+.b.y**2.;1.|];
	       [|c.x;c.y;c.x**2.+.c.y**2.;1.|];
	       [|point.x;point.y;point.x**2.+.point.y**2.;1.|]|] in
  if ccw a b c then
    det mat1 > 0.
  else 
    det mat1 < 0.;;

let equal_edges (a,b) (c,d) = 
  (* point * point -> point * point -> bool
     determines if two edges are the same *)
    (a.x=c.x && a.y=c.y && b.x=d.x && b.y=d.y) 
  ||(a.x=d.x && a.y=d.y && b.x=c.x && b.y=c.y);;

let rec search_and_delete (e:point*point) l = match l with
(* point * point -> (point * point) list -> bool * ((point * point) list)
   determines if element e was in l and deletes its occurences *)
  |[]-> (false,[])
  |h::t->let(b,tl)=search_and_delete e t in
	 if equal_edges h e then (true,tl) else (b,h::tl);;

let border (triangles:triangle_set) = 
  let rec make_list_edges triangles = match triangles with
    |[]->[]
    |h::t-> (h.p1,h.p2)::(h.p2,h.p3)::(h.p1,h.p3)::(make_list_edges t) 
  in
  let rec border_edges edges = match edges with
    |[]->[]
    |h::t->let (b,tl)=search_and_delete h t in 
	   let be = border_edges tl in
	   if b then be
	   else h::be
  in
  border_edges (make_list_edges triangles);;
  
let rec delete_triangles (triangles:triangle_set) point = match triangles with
  (*triangle_set -> point -> triangles_set * triangle_set
   returns the triangles to delete when adding point to the triangulation in del_set 
   and the remaining triangles in remain_set*)
  |[] -> ([]:triangle_set), triangles
  |h::t when in_circle h point -> let del_set, remain_set = delete_triangles t point in
                                  h::del_set, remain_set
  |h::t -> let del_set, remain_set = delete_triangles t point in
           del_set, h::remain_set;;

let add_point triangles point =
    let del_set, remain_set = delete_triangles triangles point in
    let del_set_border = border del_set in
    let rec add_triangles segments t_set p = match segments with

         [] -> t_set
        |(f,s)::t -> (make_triangle f s p)::(add_triangles t t_set p) in
    add_triangles del_set_border remain_set point;;
    
let start_triangles max_x max_y =
	(*int -> int -> triangle_set
	  returns a triangulation formed by the four border points*)
    let xmax, ymax  = float(max_x), float(max_y) in
    let bl_point, tl_point, tr_point, br_point = make_point 0. 0., make_point 0. ymax, 
                                                 make_point xmax ymax, make_point xmax 0. in
    [make_triangle bl_point  tl_point  br_point; make_triangle tl_point tr_point br_point];;
    
let delaunay (points:point_set) max_x max_y =
	let delaunay_set = start_triangles max_x max_y in
    let rec aux points_to_add triangles = match points_to_add with
         [] -> triangles
        |h::t -> aux t (add_point triangles h) in
    aux points delaunay_set;;
    
let delaunay_no_border_points points max_x max_y =
	(*point_set -> int -> int -> triangle_set
	 returns the delaunay triangulation for points without adding border points*)
	let xmax, ymax  = float(max_x), float(max_y) in
    let is_border_point p =
    (*point -> bool
     returns true if p is a border point*)
		   (p.x = 0. && p.y = 0.)
		|| (p.x = 0. && p.y = ymax)
		|| (p.x = xmax && p.y = ymax)
		|| (p.x = xmax && p.y = 0.) in
	let has_border_point t = 
	(*triangle -> bool
      returns true t contains a summit that is a border point*)
		   is_border_point t.p1
		|| is_border_point t.p2
		|| is_border_point t.p3 in
	let rec delete_border_points (triangles:triangle_set) = match triangles with
	(*triangle_set -> triangle_set
	  delete all triangles in triangles that contain border points*)
		 [] -> []
		|h::t when has_border_point h -> delete_border_points t
		|h::t -> h::(delete_border_points t) in
	delete_border_points (delaunay points max_x max_y);;
