open Quadtree;;
open Quadutils;;

let toString x = if x then "#" else "-";;

let tree1 = Quadtree.fillRectangle (Quadtree.empty 8 0 false) true 6 4 7 7;;

let tree2 = Quadtree.fillRectangle (Quadtree.empty 8 0 false) true 2 2 6 6;;

(*let tree = Quadtree.fillRectangle tree2 "0" 8 12 22 28;;*)

let string_of_quadtree = Quadutils.string_of_quadtree toString;;


print_string "Tree1 (# - true, - - false):\n";;
print_string (string_of_quadtree tree1);;
print_endline "Press Enter";;
let _ = input_char stdin;; (* more *)

print_string "Tree2 (# - true, - - false):\n";;
print_string (string_of_quadtree tree2);;
print_endline "Press Enter";;
let _ = input_char stdin;; (* more *)

print_string "Intersection:\n";;
print_string (string_of_quadtree (Quadtree.intersect tree1 tree2 0));;
print_endline "Press Enter";;
let _ = input_char stdin;; (* more *)

print_string "Sum:\n";;
print_string (string_of_quadtree (Quadtree.sum tree1 tree2 0));;
print_endline "Press Enter";;
let _ = input_char stdin;; (* more *)


