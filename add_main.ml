let rec delete_triangles (triangles:triangle_set) point = match triangles with
     [] -> ([]:triangle_set) triangles
    |h::t when in_circle h point -> let del_set, remain_set = delete_triangles t point in
                                    h::del_set, remain_set
    |h::t -> let del_set, remain_set = delete_triangles t point in
             del_set, h::remain_set;;
             
let add_triangles triangles point =
    let del_set, remain_set = delete_triangles triangles point in
    let del_set_border = border del_set in
    let rec add_triangles segments t_set p = match segments with
         [] -> t_set
        |(f,s)::t -> {p1 = f; p2 = s; p3 = p}::(add_triangles t t_set p) in
    add_triangles del_set_border remain_set p;;
