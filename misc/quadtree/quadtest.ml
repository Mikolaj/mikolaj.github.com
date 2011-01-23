open Quadtree;;
open Quadutils;;

let toString x = x ^ " ";;

let tree1 = Quadtree.fillRectangle (Quadtree.empty 32 0 "-") "8" 6 4 28 18;;

let tree2 = Quadtree.fillRectangle tree1 " " 7 11 23 29;;

let tree = Quadtree.fillRectangle tree2 "0" 8 12 22 28;; 

let smallTree = Quadtree.zoomOut (fun x _ _ _ -> x) tree;;

let string_of_quadtree = Quadutils.string_of_quadtree toString;;

print_string "Drzewo T:\n";;
print_string (string_of_quadtree tree);;
print_endline "Press Enter";;
let _ = input_char stdin;; (* more *)

print_string "T pomniejszone:\n";;
print_string (string_of_quadtree smallTree);;
print_endline "Press Enter";;
let _ = input_char stdin;; (* more *)

print_string "T pomniejszone i zwiekszone:\n";;
print_string (string_of_quadtree (Quadtree.zoomIn smallTree));;
print_endline "Press Enter";;
let _ = input_char stdin;; (* more *)

print_string "T odbite pionowo:\n";;
print_string (string_of_quadtree (Quadtree.flipVertical tree));;
print_endline "Press Enter";;
let _ = input_char stdin;; (* more *)

print_string "T odbite poziomo:\n";;
print_string (string_of_quadtree (Quadtree.flipHorizontal tree));;
print_endline "Press Enter";;
let _ = input_char stdin;; (* more *)

print_string "T odbite wzdloz back slash'a:\n";;
print_string (string_of_quadtree (Quadtree.flipBackSlash tree));;
print_endline "Press Enter";;
let _ = input_char stdin;; (* more *)

print_string "T odbite wzdloz slasha:\n";;
print_string (string_of_quadtree (Quadtree.flipSlash tree));;
print_endline "Press Enter";;
let _ = input_char stdin;; (* more *)

print_string "Rotacje T, po kolei 90, 180 i 270:\n";;
print_string (string_of_quadtree (Quadtree.rotate90 tree));;
print_endline "Press Enter";;
let _ = input_char stdin;; (* more *)

print_string (string_of_quadtree (Quadtree.rotate180 tree));;
print_endline "Press Enter";;
let _ = input_char stdin;; (* more *)

print_string (string_of_quadtree (Quadtree.rotate270 tree));;
print_endline "Press Enter";;
let _ = input_char stdin;; (* more *)

(*print_string (Quadutils.string_of_quadtree (fun x -> ) (Quadtree.add 2 1 true (Quadtree.add 2 2 true (Quadtree.empty 6 0 false))));;
*)
