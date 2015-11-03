(* file for the functions that create a delaunay triangulation in arbitrary dimenson d *)
(* the aim is similar to the triangulation in 2D, except now we use simplexes and not triangles, and a point in the given set must not be located inside the hemisphere of a simplex in the ibtained triangulation *)
(* points are represented by the array of their coordinates, simplexes are array of points of length d+1, hyperfaces of those simplexes are represented but the list of their vertices *)

let norm d p =
(* int -> float array -> float
   calculates the square of the euclidian norm of point p belonging to a d-dimensional space *)
  let r = ref 0. in
  for i = 0 to d-1 do
    r:= !r+.p.(i)**2.
  done;
  !r;;

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


let ccw d points = 
(* int -> float array array -> bool
   calculates the determinant of a matrix determined by the d+1 vertices of a simplex, used later to determine if a point is inside the hemisphere of said simplex *)
  let mat = Array.make_matrix d d 0. in
  let a=points.(0) in
  for i = 1 to d-1 do
    for j = 0 to d-1 do
      mat.(i).(j)<- points.(i).(j)-.a.(j)
    done9
  done;
  det mat > 0.;;

let in_sphere d simplex p =
(* int -> float array array -> float array -> bool
   determines whether point p is inside the hemisphere of given simplex (working in dimension d)*)
  let mat = Array.make_matrix (d+2) (d+2) 1. in
  for i = 0 to d do
    for j = 0 to d-1 do
      mat.(i).(j)<-simplex.(i).(j)
    done;
    mat.(i).(d)<- norm d simplex.(i)
  done;
  for j = 0 to d-1 do
    mat.(d+1).(j)<-p.(j)
  done;
  mat.(d+1).(d)<-norm d p;
  if ccw d simplex then 
    ((-1.)**float_of_int(d))*.(det mat) > 0.
  else 
    ((-1.)**float_of_int(d))*.(det mat) < 0.;;

let rec search e l = match l with
  (* 'a -> 'a list -> bool
     determines if e is in l *)
  |[]->false
  |t::h->if t=e then true else search e h;;

let rec equal l1 l2 = match l1 with
  (* 'a list -> 'a list -> bool
    it actually tests the inclusion of the first list into the other one, but it is going to be used on same-sized lists so it is the same as equality*)
  |[]->true
  |t::h-> if search t l2 then equal h l2 else false;;

let rec search_and_delete e l = match l with
(* 'a list -> 'a list list -> bool * ('a list list)
   determines whether element e is in list l and returns the list l without any occurence of e *)
  |[]-> (false,[])
  |h::t-> let (b,tl)=search_and_delete e t in
	  if equal e h then (true,tl)
	  else (b,h::tl);;

let border d simplexes = 
(* int -> 'a array list -> 'a list list
   finds the border of a set of simplexes that forms a convex zone of the d-dimensional space *)
  let rec make_list_hyperfaces simplexes = match simplexes with
    |[]->[]
    |simplex::t-> let l = ref [] in
	    for i=0 to d do 
	      let l'= ref [] in
	      for j=0 to d do
		if j != i then
		  l':= simplex.(j)::(!l')
	      done;
	      l:= (!l')::(!l)
	    done;
	    !l@(make_list_hyperfaces t)
  in
  let rec border_hyperfaces hyperfaces = match hyperfaces with
    |[]->[]
    |h::t-> let (b,tl)=search_and_delete h t in
	    let bh = border_hyperfaces t in
	    if b then bh
	    else h::bh
  in
  border_hyperfaces (make_list_hyperfaces simplexes);;

let rec delete_simplexes d simplexes point =
  (* int -> float array array list -> float array -> (float array array list) * (float array array list)
     given the dimension d in which we're working, a set of simplexes and a point, returns both the list of simplexes which hemisphere contains the point and the list of simplexes which hemisphere does not *)
  match simplexes with
    |[] -> ([],[])
    |h::t -> let (del_set, remain_set) = delete_simplexes d t point in
	     if in_sphere d h point then (h::del_set,remain_set)
	     else (del_set, h::remain_set);;

let make_simplex d point hyperface = 
  (* int -> float array -> float array list -> float array array
     given a hyperface and a point, returns the simplex which vertices are the d vertices of the hyperface and said point *)
  let rec add_points_to_simplex i l a = match l with
    |[]->()
    |h::t->a.(i)<-h;
      add_points_to_simplex (i+1) t a
  in
  let simplex = Array.make (d+1) (Array.make d 0.) in
  simplex.(0)<-point;
  add_points_to_simplex 1 hyperface simplex;
  simplex;;

let add_point d simplexes point = 
  (* int -> float array array list -> float array -> float array array list 
     given a triangulation represented by the set of simplexes and a new point, returns the new triangulation taking the new point into account *)
  let (del_set,remain_set) = delete_simplexes d simplexes point in
  let del_set_border = border d del_set in
  let rec add_simplexes hyperfaces simplexes point =  match hyperfaces with
    |[]->simplexes
    |h::t->(make_simplex d point h)::(add_simplexes t simplexes point)
  in
  add_simplexes del_set_border remain_set point;;

let add_coordinate position value point = 
(* int -> float -> float array
   add a coordinate of given value to a given point, in a given position, so as to transform it into a point of a space of superior dimension *)
  let d = Array.length point in
  let new_point = Array.make (d+1) 0. in
  for i = 0 to d do
    if i < position then new_point.(i)<-point.(i);
    if i = position then new_point.(i)<-value;
    if i > position then new_point.(i)<-point.(i-1)
  done;
  new_point;;

let rec starting_simplexes d size = 
  (* int -> float -> float array array list
     creates the simplexes the algorithm will start with, considering that all given points are in a d-hypercube which edges are of size 2*(given size)*)
  if d = 2 then [[|[|size;size|];[|-.size;size|];[|size;-.size|]|];[|[|-.size;-.size|];[|-.size;size|];[|size;-.size|]|]]
  else
    let rec create_hyperfaces position l = match l with
      |[]->[]
      |h::t-> let h1 = ref [] in
	      let h2 = ref [] in
	      for i = 0 to d-1 do 
		h1:= (add_coordinate position size h.(i))::(!h1);
		h2:= (add_coordinate position (-.size) h.(i))::(!h2)
	      done;
	      (!h1)::(!h2)::create_hyperfaces position t
    in
    let rec make_simplexes hyperfaces = match hyperfaces with
      |[]->[]
      |h::t->(make_simplex d (Array.make d 0.) h)::(make_simplexes t)
    in
    let previous_simplexes = starting_simplexes (d-1) size in
    let hyperfaces = ref [] in
    for i = 0 to d-1 do
      hyperfaces:= (create_hyperfaces i previous_simplexes)@(!hyperfaces)
    done;
    make_simplexes (!hyperfaces);;

let delaunay d size points =
  (* int -> float -> float array list -> float array array list
     the main function, creates a delaunay triangulation from a set of d-dimensional points and half the size of the edges of the d-hypercube centered on zero and containing all given points *)
  let delaunay_set = starting_simplexes d size in
  let rec aux points_to_add simplexes = match points_to_add with
    |[]-> simplexes
    |h::t-> aux t (add_point d simplexes h) in
  aux points delaunay_set;;
