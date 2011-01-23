(* Copyright (C) 2005 Pawel Findeisen, Mikolaj Konarski
 *
 * This file is part of the Dule compiler.
 * The Dule compiler is released under the GNU General Public License (GPL).
 * Please see the file Dule-LICENSE for license information.
 *
 * $Id: dags3.ml,v 1.1 2005/05/22 21:36:22 mikon Exp $
 *)

let rec foreach n f acc = 
    if n=0 then
	acc
    else
	foreach (n-1) f ((f n)@acc);;

let numbered n pref suf acc =
    foreach
	n
	(fun n -> pref::(string_of_int n)::suf::[])
	acc;;

let doubleNumbered n pref mid suf acc =
    foreach
	n
	(fun n -> pref::(string_of_int n)::mid::(string_of_int n)::suf::[])
	acc;;

let repeated n str acc = 
    foreach n (fun _ -> str::[]) acc;;


let header n acc = 
    numbered n " ~a" "" (*numbered n " ~b" ""*) acc;;

let decla n acc =
    foreach
	n
	(fun n ->
	    ("      v"
	    ::(string_of_int n)
	    ::" = a"
	    ::(string_of_int n)
	    ::(doubleNumbered (n-1) " ~x" ":a" ""[])@("\n"::[])))
	acc;;


let nn = int_of_string Sys.argv.(1);;
	
let decla = decla nn ("  in {}\nin f\n"::[]);;
let header = header nn (" ->\n  let\n"::decla);;

List.iter print_string ("let f = fun"::header);;
