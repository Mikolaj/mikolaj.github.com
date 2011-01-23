open Quadtree;;

Quadtree.find 2 2 (Quadtree.add 2 1 88 (Quadtree.add 2 2 17 (Quadtree.empty 6 0 3)));;


let bb n= 
  print_int 4; print_string "wwwwwwww\n";;


bb 4;;

let print_quadtree_element tree x y =
	print_string "[";
	print_int (Quadtree.find x y tree);
	print_string "]";
;;

let rec print_quadtree_row tree y xEnd actX  =
	if actX > xEnd then ()
	else 
		(print_quadtree_element tree actX y;
		print_quadtree_row tree y xEnd (actX + 1))	
;;



let print_quadtree_rectangle tree xStart yStart xEnd yEnd =
	let rec pqr actY =
		if actY > yEnd then ()
		else 
			(print_quadtree_row tree actY xEnd xStart;
			print_string "\n";
			pqr (actY + 1))
	in
	pqr yStart;
print_endline "Press Enter";
let _ = input_char stdin in () (* more *)
;;

let print_quad n =
	
	print_int n;
	print_string " - ";
	print_int ( Quadtree.find 2 2 (Quadtree.add 2 1 88 (Quadtree.add 2 2 17 (Quadtree.empty n (0) 3))) );
	print_string "\n";;

let rec blabla n =
	if n < 1 then ()
	else 
		( blabla (n - 1); print_quad n );;

print_string "start counting\n";;


print_quadtree_rectangle ((Quadtree.add 2 2 7 (Quadtree.empty 6 (1) 3))) 0 0 3 3;;
                                                                                     
print_string "\n\n";;


print_quadtree_rectangle ((Quadtree.add 2 2 7 (Quadtree.empty 5 (0) 3))) 0 0 3 3;;

print_string "\n\n";;

print_quadtree_rectangle (Quadtree.add 2 1 8 (Quadtree.add 2 2 7 (Quadtree.empty 6 (0) 3))) 0 0 3 3;;

print_string "\n\n";;

print_quadtree_rectangle (Quadtree.add 2 1 8 (Quadtree.add 2 2 7 (Quadtree.empty 6 (-1) 3))) 0 0 3 3;;

print_string "\n\n";;
print_quadtree_rectangle (Quadtree.add 2 1 8 (Quadtree.add 2 2 7 (Quadtree.empty 8 (0) 3))) 0 0 3 3;;

print_string "\n\n";;


let rec create_example_row tree y actX last =
	if last < actX then tree
	else create_example_row (Quadtree.add actX y actX tree) y (actX + 1) last;;

let exmplTree = create_example_row (Quadtree.empty 10 (0) 0) 2 0 4;;

print_quadtree_rectangle exmplTree (-1) (-1) 12 12;;

let tmpTree2 = Quadtree.fillRectangle exmplTree 8 0 2 6 2;; 

print_string "\n\n";;

print_quadtree_rectangle tmpTree2 (-1) (-1) 12 12;;


let echo_is_in_tree t n =
  (print_int n;
  print_string " in tree: ";
  print_string (string_of_bool(Quadtree.contain t n)) );;

echo_is_in_tree exmplTree 3;;

echo_is_in_tree exmplTree 30;;

print_string ("\n\nleavs number " ^ (string_of_int(Quadtree.countLeavs exmplTree)) ^ "   " ^ (string_of_int(Quadtree.countLeavs (Quadtree.empty 8 (0) 3))) ^ " \n\n");;
print_string ("\n\nnodes number " ^ (string_of_int(Quadtree.countNodes exmplTree)) ^ "   " ^ (string_of_int(Quadtree.countNodes (Quadtree.empty 8 (0) 3))) ^ " \n\n");;

print_string ("wszystko jest 8 " ^ (string_of_bool(Quadtree.forAllInRectangle tmpTree2 (fun a -> a = 8) (-100) (-100) 200 200)));;

print_newline ();;

print_string ("jest jakies 8 " ^ (string_of_bool(Quadtree.existInRectangle tmpTree2 (fun a -> a = 8) (-100) (-100) 200 200)));;

print_newline ();;

print_string ("8 wystepuje : " ^ (string_of_int (Quadtree.countColor tmpTree2 8 (-100) (-100) 200 200 )));;

print_newline ();;

print_string "Zmiana zer na 1 i niezer na 0\n\n";;
print_quadtree_rectangle (Quadtree.map (fun a -> if a = 0 then 1 else 0) exmplTree ) (-1) (-1) 12 12;;

print_newline ();;
print_newline ();;

(*print_quadtree_rectangle (Quadtree.add 2 2 1 (Quadtree.empty 12 (0) 0)) (-1) (-1) 12 12;;*)
(*
print_string "\n\n";;

print_quadtree_rectangle (Quadtree.add 1 1 2(Quadtree.add 0 0 1 (Quadtree.empty 4 (0) 0))) (-1) (-1) 2 2;;
*)





(*blabla 257;;*)
(*
print_string "\n";;
print_int ( Quadtree.find 2 2 (Quadtree.add 2 1 88 (Quadtree.add 2 2 17 (Quadtree.empty 110 0 3))) );;
print_string "\n";;
print_int ( Quadtree.find 2 2 (Quadtree.add 2 1 88 (Quadtree.add 2 2 17 (Quadtree.empty 8 0 3))) );;
print_string "\n";;
*)
