(* Copyright (C) 2003--2006 Mikolaj Konarski
 *
 * This file is part of the Dule compiler.
 * The Dule compiler is released under the GNU General Public License (GPL).
 * Please see the file Dule-LICENSE for license information.
 *
 * $Id: main_common.ml,v 1.22 2007-08-25 13:21:30 mikon Exp $
 *) 

(* All this is totally hackish and unnecessarily complicated. :< *)

let prin s = if !Tools.debugging then (prerr_endline s; flush stderr) else ()

let verify = ref true
let results = ref []
let load_prelude = ref true
let prelude_ref = ref `Void
let prelude_path = "./prelude.dul"

let pres m = 
if !Core_back.SemFTrans.no_execution then 
  "\nnone, invoked with \"--no-execution\"" 
else if !Core_back.SemFTrans.no_fixpoints then
  "\nnone, invoked with \"--no-fixpoints\"" 
else
  let t = Mod_back.Dule.value_part m in
  let ress = 
    (match Core_back.DeFTrans.t2de t with
    |`T_record (f, lt) ->
	if Tools.IList.is_nil lt then "\nnone, no modules"
	else Tools.IList.bfold ""
	    (fun (i, t) s ->  	    
	      (match Core_back.DeFTrans.t2de t with
	      |`T_record (f, lt) -> 
		  (match Tools.IList.find_ok Tools.AtIndex.result lt with
		  |`OK t -> 
		      (match Core_back.DeFTrans.t2de t with
		      |`T_record (f, lt) -> 
			  "\n" ^ (Tools.IdIndex.t2s i) ^ " ---" ^
			  if Tools.IList.is_nil lt 
			  then " none, \"Result\" has no values"
			  else "\n" ^ Core_back.PpFTrans.pp_t t
		      | _ -> "\nnone, \"Result\" not evaluated enough")
		  |`Error er -> "")
	      | _ -> failwith "main.pres: module not record") 
	      ^ s) lt
    | _ -> failwith "main.pres: outcome not record")
  in
  if ress = "" then "\nnone, no \"Result\" modules"
  else ress
   
let prcore f t = 
  "\ncore value of type ---\n" ^ Core_back.PpFFunct.pp_f f
  ^ "\nand content ---\n" ^ Core_back.PpFTrans.pp_t t
			      
exception VerificationError of string
let el_trans t =
  (match Core_back.ElabFCore.el_trans t with
  |`OK (_, _, _, _, t) -> t
  |`Error er -> raise (VerificationError er))

exception SemanticError of Error_rep.ErrorRepLib.error
let el_dule3 m =
  (match Mod_back.ElabIDule.el_dule m with
  |`OK m -> m
  |`Error er -> raise (SemanticError er))

let el_prel3 m =
  (match Mod_back.ElabIDule.el_prelude m with
  |`OK m -> m
  |`Error er -> raise (SemanticError er))

let el_dule2 m pe' me' =
  (match Mod_front.ElabBDule.elab_dule m pe' me' with
  |`OK m -> m
  |`Error er -> raise (SemanticError er))

let el_pre2 m =
  (match Mod_front.ElabBDule.elab_prelude m with
  |`OK mpeme -> mpeme
  |`Error er -> raise (SemanticError er))

let el_dule1 li =
  (match Mod_front.ElabEDule.elab_start li with
  |`OK m -> m
  |`Error er -> raise (SemanticError er))

let el_core prelude_m t =
  let prelude_v = Mod_back.Dule.value_part prelude_m in
  (match Core_front.ElabBCore.elab_core prelude_v t with
  |`OK t -> t
  |`Error er -> raise (SemanticError er))

let el_prelude process_start =
  if !load_prelude then
    let semantics m =
      prin "\nprelude.dul parsed";
      let re1 = el_dule1 m in
      let (re2, pe', me') = el_pre2 re1 in
      let re3 = el_dule3 re2 in
      (re3, pe', me')
    in
    let prelude_m = process_start semantics prelude_path in 
    prin "\nprelude.dul successfully compiled";
    let _ = prelude_ref := `Full prelude_m in
    let (re3, _, _) = prelude_m in
    re3
  else
    let _ = prin "\nempty prelude inserted" in
    let re3 = el_dule3 (Error_rep.Location.none,
			Mod_back.IDule.M_Link (Tools.IList.nil, 
					       Tools.IList.nil)) 
    in
    let _ = prin "." in
    let prelude_m = (re3, Tools.IList.nil, Tools.IList.nil) in
    let _ = prelude_ref := `Fake prelude_m in
    re3

let update_prelude process_start =
  (match !prelude_ref with
  |`Void -> el_prelude process_start
  |`Fake (m, 
	  (pe' : (Mod_back.IDule.sign Tools.IList.t 
		    * Mod_back.IDule.sign) Tools.IList.t), 
	  (me' : (Mod_back.IDule.sign Tools.IList.t 
		    * Mod_back.IDule.sign 
		    * Mod_back.IDule.dule) Tools.IList.t)) -> 
      if !load_prelude then el_prelude process_start else m
  |`Full (m, 
	  (pe' : (Mod_back.IDule.sign Tools.IList.t 
		    * Mod_back.IDule.sign) Tools.IList.t), 
	  (me' : (Mod_back.IDule.sign Tools.IList.t 
		    * Mod_back.IDule.sign 
		    * Mod_back.IDule.dule) Tools.IList.t)) -> 
      if not !load_prelude then el_prelude process_start else m)

let elm m =
  flush stderr; print_string "."; flush stdout;
  let re1 = el_dule1 m in
  flush stderr; print_string "."; flush stdout;
  let re2 = 
    if !load_prelude then
      match !prelude_ref with
      |`Full (_, pe', me') ->
	  el_dule2 re1 pe' me'
      | _ -> failwith "corrupted compiled prelude"
    else
      el_dule2 re1 Tools.IList.nil Tools.IList.nil
  in
  flush stderr; print_string "."; flush stdout;
  let re3 = el_dule3 re2 in
  let _ = 
    if !verify then 
      (flush stderr; print_string "\n"; flush stdout;
       let re4 = Mod_back.Dule.value_part re3 in
       ignore (el_trans re4))
    else (flush stderr; print_string "\n"; flush stdout)
  in
  let _ = results := pres re3 :: !results in
  ()

let elc prelude_m t =
  flush stderr; print_string "."; flush stdout;
  let (f, t) = el_core prelude_m t in
  flush stderr; print_string "."; flush stdout;
  let _ = 
    if !verify then 
      (flush stderr; print_string "\n"; flush stdout;
       ignore (el_trans t))
    else (flush stderr; print_string "\n"; flush stdout)
  in
  let _ = results := prcore f t :: !results in
  ()

let report_error ppf exn =
  let report ppf = function
  | Error_rep.TextualError.TextualError er -> er#report ppf
  | Tools.IList.EconsDuplication s -> prerr_string ("Error: " ^ s); 
      prerr_endline " duplicated during parsing"
  | SemanticError er -> er#report ppf
  | VerificationError er -> 
      prerr_endline "\nO dear! Verification failed with"; prerr_endline er
  | Failure er -> 
      prerr_endline "\nO dear! Internal Error!"; prerr_endline er
  | Core_back.SemFTrans.AssertionError er -> er#report ppf
  | Core_back.SemFTrans.FailError er -> er#report ppf
  | x -> Format.fprintf ppf "@]"; raise x 
  in
  Format.fprintf ppf "@[%a@]@." report exn

let usage = "Usage: dule <options> <files>\nOptions are:"

let set_int n = 
  let n = if n < 2 then 2 else n in
  Tools.PpTools.depth := n

let speclist anonymous corymous =
  ["-m", Arg.String anonymous,
   "<filename>  compile module-level entities, this is the default";
   "-c", Arg.String corymous,
   "<filename>  compile core-level entities, but I would rather use modules";
   "--prelude", Arg.Set load_prelude,
   "   load the prelude, this is the default";
   "--no-prelude", Arg.Clear load_prelude,
   "do not load the prelude code";
   "--assert", Arg.Set Core_front.ElabBCore.check_assert,
   "   check Dule assertions, this is the default";
   "--no-assert", Arg.Clear Core_front.ElabBCore.check_assert,
   "do not check Dule assertions";
   "--verification", Arg.Set verify,
   "   verify the result code, this is the default";
   "--no-verification", Arg.Clear verify,
   "do not verify that the result code is type correct";
   "--composition", Arg.Set Mod_front.ElabEDule.allow_composition,
   "   allow module identity and composition, for testing only";
   "--no-composition", Arg.Clear Mod_front.ElabEDule.allow_composition,
   "reject module identity and composition, this is the default";
   "--overwriting", Arg.Set Mod_front.ElabEDule.allow_overwriting,
   "   allow overwriting of specifications and libraries, for testing only";
   "--no-overwriting", Arg.Clear Mod_front.ElabEDule.allow_overwriting,
   "ban overwriting of specifications and libraries, this is the default";
   "--complex-structures", Arg.Set Mod_back.ElabIDule.complex_structures,
   "   use the complex encoding of base modules, for testing only";
   "--no-complex-structures", Arg.Clear Mod_back.ElabIDule.complex_structures,
   "do not use the complex encoding of base modules, this is the default";
   "--debugging", Arg.Set Tools.debugging,
   "   print a lot of debugging info";
   "--no-debugging", Arg.Clear Tools.debugging,
   "do not print debugging info, this is the default";
   "--execution", Arg.Clear Core_back.SemFTrans.no_execution,
   "compile and execute code, all at once, this is the default";
   "--no-execution", Arg.Set Core_back.SemFTrans.no_execution,
   "do not execute code, only parse, type-check and compile";
   "--fixpoints", Arg.Clear Core_back.SemFTrans.no_fixpoints,
   "execute fixpoints (general recursion), this is the default";
   "--no-fixpoints", Arg.Set Core_back.SemFTrans.no_fixpoints,
   "do not execute fixpoints (general recursion)";
   "--colors", Arg.Set Error_rep.ErrorRep.colors,
   "   highlight error messages, this is the default";
   "--no-colors", Arg.Clear Error_rep.ErrorRep.colors,
   "suppress colorized error printing";
   "--pp", Arg.Clear Tools.PpTools.mute,
   "print out the result, this is the default";
   "--no-pp", Arg.Set Tools.PpTools.mute,
   "do not print out the result";
   "--pp-depth", Arg.Int set_int,
   "<n>  set maximum pretty-printing nesting to <n>, default 5"]
(* removed, to work in 3.06
   "--pp-depth", Arg.Set_int Tools.PpTools.depth,
   "<n>  set maximum pretty-printing nesting to <n>, default 5"]
*)

let main speclist anonymous = 
  try
    let _ = Sys.catch_break true in
    let _ = Arg.parse speclist anonymous usage in
    (flush stderr; print_string "Success! The results are:"; 
     if !results = [] then 
       print_string "\nnone, probably no arguments given\n"
     else (ignore (List.map (fun re -> print_string re) (List.rev !results)); 
		  print_string "\n"))
  with
  | Sys.Break -> 
      print_string "\n"; flush stdout; prerr_endline "Interrupted!";
      exit 0
  | x ->
      print_string "\n"; flush stdout; report_error Format.err_formatter x;
      exit 2
