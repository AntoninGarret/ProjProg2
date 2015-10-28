type point = {x : float ; y : float};;
type triangle = {p1 : point ; p2 : point ; p3 : point};;
type point_set = point list;;
type triangle_set = triangle list;;

let rec random nb max_x max_y = match nb with
  |0->[]
  |_->{x=float(Random.int(max_x));y=float(Random.int(max_y))}::(random (nb-1) max_x max_y);;

let del_first_column mat =
  let n = Array.length mat in
  let a = Array.make_matrix n (n-1) 0. in
  for i = 0 to n-1 do 
    a.(i)<-Array.sub mat.(i) 1 (n-1)
  done;
  a;;

let del_line i mat =
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

let equal_edges (a,b) (c,d) = (a=b && c=d) || (a=d && b=c);;

let rec search_and_delete (e:point*point) l = match l with
  |[]-> (false,[])
  |h::t->let(b,tl)=search_and_delete e t in
	 if equal_edges h e then (true,tl) else (b,h::tl);;

let border triangles = 
  let rec make_list_edges triangles = match triangles with
    |[]->[]
    |h::t-> (h.p1,h.p2)::(h.p2,h.p3)::(h.p1,h.p3)::(make_list_edges t) 
  in
  let rec border_edges edges = match edges with
    |[]->[]
    |h::t->let (b,tl)=search_and_delete h t in border_edges tl
  in
  border_edges (make_list_edges triangles);;

  
let rec delete_triangles (triangles:triangle_set) point = match triangles with
     [] -> ([]:triangle_set), triangles
    |h::t when in_circle h point -> let del_set, remain_set = delete_triangles t point in
                                    h::del_set, remain_set
    |h::t -> let del_set, remain_set = delete_triangles t point in
             del_set, h::remain_set;;
             
let add_point triangles point =
    let del_set, remain_set = delete_triangles triangles point in
    let del_set_border = border del_set in
    let rec add_triangles segments t_set p = match segments with
         [] -> t_set
        |(f,s)::t -> {p1 = f; p2 = s; p3 = p}::(add_triangles t t_set p) in
    add_triangles del_set_border remain_set point;;
