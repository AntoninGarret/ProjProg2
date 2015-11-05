# ProjProg2
Repository for Programmation Project about Delaunay Triangulation
Rights to Antonin Garret and Nathan Thomasset

main.ml contient les instructions de base pour la triangulation de delaunay ainsi que la triangulation sans points extremaux
graphics.ml contient les instructions relatives à l'affichage de la triangulation et l'affichage étape par étape de la triangulation

Dans l'interpréteur OCaml, une fois graphics.ml chargé avec #use "graphics.ml", utiliser :
draw_points (random n 600 450);; pour afficher un nuage de n points
draw_triangles (delaunay (random n 600 450) 600 450);; pour afficher la triangulation d'un nuage de n points
draw_triangles (delaunay_no_border_points (random n 600 450) 600 450);; pour afficher la triangulation d'un nuage de n points sans points extremaux
delaunay_step_by_step (random n 600 450) 600 450;; pour afficher la triangulation étape par étape d'un nuage de n points


Delaunay_Multidimension.ml contient les instructions relatives à la triangulation de delaunay pour toute dimension supérieure à 2.
Il n'y a pas d'affichage disponible pour le triangulations dans des dimensions strictement supérieures

Dans l'interpréteur OCaml, une fois Delaunay_Multidimension.ml chargé avec #use "Delaunay_Multidimension.ml", utiliser :
delaunay d 500. (random d n 500.);; pour créer une triangulation d'un nuage de n points en dimension d
