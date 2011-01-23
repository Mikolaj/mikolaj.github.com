(* Copyright (C) 2003--2006 Mikolaj Konarski, Patryk Czarnik
 *
 * This file is part of the Dule compiler.
 * The Dule compiler is released under the GNU General Public License (GPL).
 * Please see the file Dule-LICENSE for license information.
 *
 * $Id: core_middle.ml,v 1.86 2007-08-25 13:21:30 mikon Exp $
 *) 

open Core_back open Tools open Error_rep

module type OccursFCore =
  sig
    module IdIndex : IdIndex
    module IListBasic : IListBasic with type Index.t = IdIndex.t
    module IList : IList with type Index.t = IdIndex.t
    module VarStamp : Stamp
    module Cat : T
    module Funct : DeFFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with module VarStamp = VarStamp
    val occurs_in_f : VarStamp.t -> Funct.t -> bool
    val loccurs_in_lf : 
	IListBasic.IListStamp.t -> Funct.t -> Funct.t IList.t -> 
	  [`Yes|`With_other_elem|`No]
  end

module OccursFCore'
    (IdIndex : IdIndex)
    (IListBasic : IListBasic with type Index.t = IdIndex.t)
    (IList : IList 
    with type Index.t = IdIndex.t
    with type 'a IListBasic.t = 'a IListBasic.t)
    (VarStamp : Stamp)
    (Cat : T)
    (Funct : DeFFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with module VarStamp = VarStamp)
    (EqFFunct : EqFFunct with type Funct.t = Funct.t)
    : (OccursFCore
    with module IdIndex = IdIndex
    with module IListBasic = IListBasic
    with module IList = IList
    with module VarStamp = VarStamp
    with module Cat = Cat
    with module Funct = Funct) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module IListBasic = IListBasic
    module VarStamp = VarStamp
    module Cat = Cat
    module Funct = Funct

    let occurs_in_f occurs_in_f (* for recursive memoization *) (var_num, f) =
      let occurs_in_f var_num f = occurs_in_f (var_num, f) in
      let occurs_in_lf var_num lf =
	IList.vexists (occurs_in_f var_num) lf ||
	(IListBasic.is_flexible lf 
	   && (if occurs_in_f var_num (IListBasic.get_elem lf) then
               failwith "surprise: occurs_in_f in get_elem!" else false))
      in
      match Funct.t2de f with
      |`F_ID c -> false
      |`F_COMP (f1, f2) -> occurs_in_f var_num f1 || occurs_in_f var_num f2
      |`F_PR (lc, i) -> false
      |`F_RECORD (c, lf) -> occurs_in_lf var_num lf
      |`F_pp (c, lf) -> occurs_in_lf var_num lf
      |`F_ss (c, lf) -> occurs_in_lf var_num lf
      |`F_ii f -> occurs_in_f var_num f
      |`F_tt f -> occurs_in_f var_num f
      |`F_ee (lf, f) -> occurs_in_lf var_num lf || occurs_in_f var_num f
      |`F_finish f -> false
      |`F_var (var_num', c) -> VarStamp.eq var_num var_num'

    module HC = 
      struct
	type t = VarStamp.t * Funct.t
	type result = bool
	let equal (var_num, f) (var_num', g) = 
	  VarStamp.eq var_num var_num' && EqFFunct.eq f g
	let hash (var_num, f) = 
	  abs (combine (VarStamp.t2int var_num) (Funct.t2stamp f))
      end
    module MemoOccurs = Memoize' (HC)

    let tbl = MemoOccurs.create good_hash_size

    let occurs_in_f var_num f = MemoOccurs.memorec tbl occurs_in_f (var_num, f)

    let try_again b p f2 =
      (match b with
      |`No -> p f2
      |`With_other_elem -> 
	  (match p f2 with
	  |`No -> `With_other_elem
	  | b -> b)
      |`Yes -> `Yes)

    let loccurs_in_lf loccurs_in_f l_stamp gs lf =
      let p = loccurs_in_f l_stamp gs in
      let exists_again p =
	IList.vfold `No
	  (fun f b -> 
	    try_again b p f)
      in
      try_again 
	(exists_again p lf)
	(fun () ->
	  if IListBasic.is_flexible lf then
	    let fs = IListBasic.get_elem lf in
	    try_again
	      (let lf_stamp = IListBasic.get_stamp lf in 
	      if IListBasic.IListStamp.eq lf_stamp l_stamp then
		if EqFFunct.eq fs gs then `Yes else `With_other_elem
	      else 
		`No)
	      p fs 
	  else `No) ()

    let loccurs_in_f loccurs_in_f (l_stamp, gs, f) =
      let loccurs_in_f l_stamp gs f = loccurs_in_f (l_stamp, gs, f) in
      let loccurs_in_lf = loccurs_in_lf loccurs_in_f in
      match Funct.t2de f with
      |`F_ID c -> `No
      |`F_COMP (f1, f2) -> 
	  try_again (loccurs_in_f l_stamp gs f1) (loccurs_in_f l_stamp gs) f2
      |`F_PR (lc, i) -> `No
      |`F_RECORD (c, lf) -> loccurs_in_lf l_stamp gs lf
      |`F_pp (c, lf) -> loccurs_in_lf l_stamp gs lf
      |`F_ss (c, lf) -> loccurs_in_lf l_stamp gs lf
      |`F_ii f -> loccurs_in_f l_stamp gs f
      |`F_tt f -> loccurs_in_f l_stamp gs f
      |`F_ee (lf, f) ->
	  try_again (loccurs_in_lf l_stamp gs lf) (loccurs_in_f l_stamp gs) f
      |`F_finish f -> `No
      |`F_var (var_num, c) -> `No

    module HL = 
      struct
	type t = IListBasic.IListStamp.t * Funct.t * Funct.t
	type result = [`Yes|`With_other_elem|`No]
	let equal (l_stamp, fs, f) (l_stamp', gs, g) = 
	  IListBasic.IListStamp.eq l_stamp l_stamp' && EqFFunct.eq fs gs
	    && EqFFunct.eq f g
	let hash (l_stamp, fs, f) = 
	  abs (combine (Funct.t2stamp f) (IListBasic.IListStamp.t2int l_stamp) 
		 + Funct.t2stamp fs)
      end
    module MemoLOccurs = Memoize' (HL)

    let tbl = MemoLOccurs.create good_hash_size

    let loccurs_in_f l_stamp gs f =  
      MemoLOccurs.memorec tbl loccurs_in_f (l_stamp, gs, f)

    let loccurs_in_lf = loccurs_in_lf loccurs_in_f
  end

module OccursFCore = OccursFCore' (IdIndex) (IListBasic) (IList)
    (Stamp) (ACat) (DeFFunct) (EqFFunct)


module type TypeFFunct =
  sig
    module IdIndex : IdIndex
    module IList : IList 
    with type Index.t = IdIndex.t
    module VarStamp : Stamp
    module Funct : DeFFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with module VarStamp = VarStamp

    type t = Funct.t
    val type_sub_app_f : (* memoized, so with side-effects! *)
	bool ->
	  (t -> t) lazy_t -> 
	    (t IList.t -> t IList.t) ->
	      (VarStamp.t -> t -> t) -> 
		t -> t 
    val f_COMP : t -> t -> t
    val f_pp_nil : t -> t
  end

module TypeFFunct'
    (IdIndex : IdIndex)
    (IList : IList 
    with type Index.t = IdIndex.t)
    (VarStamp : Stamp)
    (Cat : FCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : FFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with module VarStamp = VarStamp)
    (SrcFCore : SrcFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with type Funct.t = Funct.t)
    (EqFCat : EqFCat with type Cat.t = Cat.t)
    (EqFFunct : EqFFunct with type Funct.t = Funct.t)
    : (TypeFFunct
    with module IdIndex = IdIndex
    with module IList = IList
    with module VarStamp = VarStamp
    with module Funct = Funct) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module VarStamp = VarStamp
    module Funct = Funct
    open Cat
    open Funct

    type t = Funct.t

   (* memoization to prevent exponential time substitution into DAGs *)
    module HC = 
      struct
	type t = Funct.t
	type result = Funct.t
	let equal f g = EqFFunct.eq f g
	let hash f = Funct.t2stamp f
      end
    module MemoEF = Memoize' (HC)

    (* only local memoization, so small hashtable *)
    let tbl = MemoEF.create 31

    (* hack for [sub_translate]: *)
    let ct = c_PP (IList.cons (IdIndex.s2type "@translate", c_BB) IList.nil)

    (* [old_f] and result in normal form: *)
    let type_sub_app_f finishp sub_app_f sub_app_lf sub_app_var =
      let sub_into old_f =
	let sub_app_f = Lazy.force_val sub_app_f in
	match Funct.t2de old_f with
	|`F_ID c -> old_f
	|`F_COMP (f1, f2) ->
            f_COMP 
	      (sub_app_f f1)
	      (sub_app_f f2)
	|`F_PR (lc, i) -> old_f
	|`F_RECORD (c, lf) ->
            f_RECORD c (sub_app_lf lf)
	|`F_pp (c, lf) ->
            f_pp c (sub_app_lf lf)
	|`F_ss (c, lf) ->
            f_ss c (sub_app_lf lf)
	|`F_ii f -> f_ii (sub_app_f f)
	|`F_tt f -> f_tt (sub_app_f f)
	|`F_ee (lf, f) ->
            f_ee (sub_app_lf lf) 
	      (sub_app_f f)
	|`F_finish f -> if finishp then f else old_f
	|`F_var (var_num, c) ->
	    let f = sub_app_var var_num old_f in
	    let c' = SrcFCore.src f in
            (* hack for [sub_translate]: *)
            if EqFCat.eq c' ct then
              (match Funct.t2de f with
	      |`F_var (var_num, _) -> f_var var_num c
	      | _ -> failwith "TypeFFunct.hack: f not var")
            else
              (assert (EqFCat.eq c' c);
	       f)
      in
      let _ = MemoEF.clear tbl in
      MemoEF.memoize tbl sub_into

    (* hack for [sub_translate]: *)
    let f_COMP f g = 
      let c' = SrcFCore.src g in
      if EqFCat.eq c' ct then f
      else f_COMP f g

    let f_pp_nil old_f = f_pp (SrcFCore.src old_f) IList.nil
  end

module TypeFFunct = TypeFFunct' (IdIndex) (IList) (Stamp)
    (FCat) (FFunct) (SrcFCore) (EqFCat) (EqFFunct)

module SubFFunct = Substitution' (TypeFFunct)


module type UnifyFCore =
  sig
    module IdIndex : IdIndex
    module IListBasic : IListBasic with type Index.t = IdIndex.t
    module IList : IList 
    with type Index.t = IdIndex.t
    module Location : Location
    module ErrorRepLib : ErrorRepLib
    with module Location = Location
    module VarStamp : Stamp
    module Cat : T
    module Funct : FFunct
    with module IdIndex = IdIndex
    with module IList = IList
    with module Cat = Cat
    with module VarStamp = VarStamp
    module SubFFunct : sig type subst end

    val unify : VarStamp.t -> IListBasic.IListStamp.t -> 
      (Funct.t * Funct.t * Location.t) list ->
      [`OK of (Funct.t * Funct.t * Location.t) list 
	  * SubFFunct.subst * VarStamp.t * IListBasic.IListStamp.t
      |`Error of ErrorRepLib.error]
  end

module UnifyFCore' 
    (IdIndex : IdIndex)
    (IListBasic : IListBasic with type Index.t = IdIndex.t)
    (IList : IList 
    with type Index.t = IdIndex.t
    with type 'a IListBasic.t = 'a IListBasic.t)
    (Location : Location)
    (ErrorRepLib : ErrorRepLib
    with module Location = Location)
    (VarStamp : Stamp)
    (Cat : SemFCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : SemFFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with module VarStamp = VarStamp)
    (SrcFCore : SrcFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (PpFCat : PpFCat with type Cat.t = Cat.t)
    (PpFFunct : PpFFunct
    with type Funct.t = Funct.t
    with type 'a Funct.IList.IListBasic.t = 'a IList.t)
    (EqFCat : EqFCat with type Cat.t = Cat.t)
    (EqFFunct : EqFFunct with type Funct.t = Funct.t)
    (OccursFCore : OccursFCore
    with module IdIndex = IdIndex 
    with module IListBasic = IListBasic
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with module VarStamp = Funct.VarStamp)
    (SubFFunct : Substitution
    with module IdIndex = IdIndex
    with module IListStamp = IListBasic.IListStamp
    with module IListBasic = IListBasic
    with module IList = IList
    with module VarStamp = VarStamp
    with type Type.t = Funct.t)
    : (UnifyFCore
    with module IdIndex = IdIndex
    with module IListBasic = IListBasic
    with module IList = IList
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with module VarStamp = VarStamp
    with module Cat = Cat
    with module Funct = Funct
    with module SubFFunct = SubFFunct) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module IListBasic = IListBasic
    module Location = Location
    module ErrorRepLib = ErrorRepLib
    module VarStamp = VarStamp
    module Cat' = Cat
    module Funct' = Funct
    module SubFFunct = SubFFunct
    open IList
    open ErrorRepLib
    open Cat
    open Funct
    open SrcFCore
    open OccursFCore
    open SubFFunct
    open EqFFunct
    module Cat = Cat'
    module Funct = Funct'

    let rec foldl init f =
      function
	| [] -> init
	| v :: r -> foldl (f v init) f r

    let rec mapl f = 
      function
	| [] -> []
	| v :: r -> f v :: mapl f r

    let fresh_ty_var var_num c = (* where to put this? *)
      (f_var var_num c, VarStamp.inc var_num)
	    
    let many_vars var_num c lf =
      let starting = (nil, var_num) in
      let process = fun i (lvar, var_num) ->
        let (fv, var_num) = fresh_ty_var var_num c in
	(cons (i, fv) lvar, var_num)
      in 
      ifold starting process lf

    let freshen_ilist l_stamp c l =
      (IListBasic.rigid2flexible l_stamp (f_ID c) l, 
       IListStamp.inc l_stamp)

    let fresh_ilist l_stamp c = (* where to put this? *)
      freshen_ilist l_stamp c nil

    let is_var f = 
      match Funct.t2de f with
      |`F_var _ -> true
      | _ -> false

    (* result equivalent to [`F_finish (f)], but with other root constructor *)
    let bury_finish f =
      match Funct.t2de f with
      |`F_ID c -> f
      |`F_COMP (f1, f2) -> f (* loops: f_COMP (f_finish f1) (f_finish f2) *)
      |`F_PR (lc, i) -> f
      |`F_RECORD (c, lf) -> f_RECORD c (vmap f_finish lf)
      |`F_pp (c, lf) -> f_pp c (vmap f_finish lf)
      |`F_ss (c, lf) -> f_ss c (vmap f_finish lf)
      |`F_ii g -> f_ii (f_finish g)
      |`F_tt g -> f_tt (f_finish g)
      |`F_ee (lg, h) -> f_ee (vmap f_finish lg) (f_finish h)
      |`F_finish _ -> assert false
      |`F_var _ -> assert false

    (* test if the pair is hard to unify *)
    let is_bad f g =
      match Funct.t2de f, Funct.t2de g with
      |`F_COMP (f1, f2), `F_var (var_num, _) -> 
	  is_var f2 && occurs_in_f var_num f
      |`F_COMP (f1, f2), _ when is_var f2 -> true
      |`F_var (var_num, _), `F_COMP (g1, g2) -> 
	  is_var g2 && occurs_in_f var_num g
      | _, `F_COMP (g1, g2) when is_var g2 -> true
      |`F_pp (c, lf), `F_pp (d, lg) ->
	  let lf_flexible = IListBasic.is_flexible lf in
	  let lg_flexible = IListBasic.is_flexible lg in
	  let lf_small = is_nil lf in
	  let lg_small = is_nil lg in
	  lf_flexible && lg_flexible && lf_small && lg_small
	    && 
	  (let fs = IListBasic.get_elem lf in
	  let gs = IListBasic.get_elem lg in
	  not (EqFFunct.eq gs (f_ID c) && EqFFunct.eq fs (f_ID c)))
      | _, _ -> false (* if `F_finish then 2nd or 4th case, or false *)

    (* test if the pair contributes nothing unless it fails *)
    let is_very_bad f g = (* we know that [is_bad f g] *)
      match Funct.t2de f, Funct.t2de g with
      |`F_COMP (f1, f2),`F_COMP (g1, g2) -> is_var f2 && is_var g2
      |`F_COMP (f1, f2), `F_var _ -> true
      |`F_var _, `F_COMP (g1, g2) -> true
      |`F_pp (_, lf), `F_pp (_, lg) -> true
      | _, _ -> false (* if `F_finish then false, too *)
    (* when unifying [f1 . f2] with [g], [f2] being a variable, 
       guesses by the structure of [g] what form the [f2] will have *)
    let guess_g var_num l_stamp f1 f2 g l =
      let _ =
	if !Tools.debugging then
	  let pr2 (f1, f2, g) = 
	    "\n(f1 = " ^ PpFFunct.pp_f f1
	    ^"\n f2 = " ^ PpFFunct.pp_f f2
	    ^ ",\n g = " ^ PpFFunct.pp_f g ^ ")"
	  in
	  let sl = pr2 (f1, f2, g) in
	  (prerr_endline ("\n guess_g:" ^ sl); flush stderr)
	else ()
      in
      match Funct.t2de f2 with
      |`F_var (var_num_f2, c) ->
	  (match Funct.t2de (unfinish g) with
	  |`F_pp (a, lg) ->
	      let c = src f2 in 
	      let (lf2, var_num) = many_vars var_num c lg in
	      let (lf2, l_stamp) = freshen_ilist l_stamp c lf2 in 
	      let f2my = f_pp c lf2 in
	      `OK ([(f2my, f2, l)], var_num, l_stamp)
	  |`F_ss (a, lg) ->
	      let c = src f2 in 
	      let (lf2, var_num) = many_vars var_num c lg in
	      let (lf2, l_stamp) = freshen_ilist l_stamp c lf2 in 
	      let f2my = f_ss c lf2 in
	      `OK ([(f2my, f2, l)], var_num, l_stamp)
	  |`F_ii g1 ->
	      if occurs_in_f var_num_f2 g then 
		((*pri "guess_g: occurs_in_f var_num_f2 g1 --- is this enough/too much?";*)
		 `Error "guess_g: occurs_in_f var_num_f2 g1 --- is this enough/too much?")
	      else
		let c = src f2 in 
		let b = c_PP (Cat.coi c_BB c) in
		let (fv, var_num) = fresh_ty_var var_num  b in
		let f2my = f_ii fv in
		`OK ([(f2my, f2, l)], var_num, l_stamp)
	  |`F_tt g1 ->
	      if occurs_in_f var_num_f2 g then 
		((*pri "guess_g: occurs_in_f var_num_f2 g1 --- is this enough/too much?";*)
		 `Error "guess_g: occurs_in_f var_num_f2 g1 --- is this enough/too much?")
	      else
		let c = src f2 in 
		let b = c_PP (Cat.coi c_BB c) in
		let (fv, var_num) = fresh_ty_var var_num  b in
		let f2my = f_tt fv in
		`OK ([(f2my, f2, l)], var_num, l_stamp)
	  |`F_ee (lg, h) ->
	      let c = src f2 in 
	      let (lf2, var_num) = many_vars var_num c lg in
	      let (lf2, l_stamp) = freshen_ilist l_stamp c lf2 in 
	      let (fv, var_num) = fresh_ty_var var_num c in
	      let f2my = f_ee lf2 fv in
	      `OK ([(f2my, f2, l)], var_num, l_stamp)
       (* |`F_COMP (g1, g2) when EqFFunct.eq f1 g1 ->
	      `OK ([(g2, f2, l)], var_num, l_stamp)
          --- if g1 was <...> then wrong, but a useful heuristics, yet slow *)
	  | _ -> `Error "g is i or i . ... . k --- only prs are good for f2")
(* cases g = i.var (3) or i . j . var (3) and f = f1 . var (5) are difficult*)
(* remaining question if g is COMP has f2 to be i . j . k? *)
(* --- no, can be e.g. {}, but if g2 is not var then yes *)
(* if g2 is var and g1 and f1 are not <lf> or lf=nil then f1 = f2. Messy. *)
      | _ -> assert false

    (* derives pairs and substitution from the unifiablity of [lf] and [lg] *)
    let match_lists var_num l_stamp c lf lg l = 
      let lh = 
        bfold []
          (fun (i, f) lh ->
	    (match find_ok i lg with
	    |`OK g -> (f, g, l)::lh
	    |`Error er -> lh)) lf
      in
      match lh with
      | (f, g, l)::lfg ->
	  let lf_lg = subtract lf lg in
	  let lg_lf = subtract lg lf in
	  let lf_lg = 
	    if IListBasic.is_flexible lf then
	      IListBasic.move_flexible lf lf_lg
	    else lf_lg
	  in
	  let lg_lf =
	    if IListBasic.is_flexible lg then
	      IListBasic.move_flexible lg lg_lf
	    else lg_lf
	  in
	  `OK ((f_pp c lf_lg, f_pp c lg_lf, l)::lh, sub_id, var_num, l_stamp)
      | [] ->
	  let lf_rigid = IListBasic.is_rigid lf in
	  let lg_rigid = IListBasic.is_rigid lg in
	  let lf_small = is_nil lf in
	  let lg_small = is_nil lg in
	  if lf_rigid && lg_rigid then 
	    if lf_small && lg_small
	    then `OK ([], sub_id, var_num, l_stamp)
	    else `Error
		(coreMiddleError#instance 
		   [Loc l; 
		    Msg ("the ilists are both rigid, but not both empty:" 
			 ^ "\nlf = (" 
			 ^ PpFFunct.pp_lf lf ^ ")\nlg = ("
			 ^ PpFFunct.pp_lf lg ^ ")")])
	  else if lf_rigid then
	    if lg_small then
	      let lg_stamp = IListBasic.get_stamp lg in
	      let gs = IListBasic.get_elem lg in
	      let g_in_lf = loccurs_in_lf lg_stamp gs lf in
	      if g_in_lf = `Yes
	      then `Error
		  (coreMiddleError#instance 
		     [Loc l; 
		      Msg ("occur check (1) of ilist stamp "
			   ^ IListBasic.pp_stamp lg
			   ^ " failed in ilist\n"
			   ^ PpFFunct.pp_lf lf)])
	      else
		if EqFFunct.eq gs (f_ID c) && g_in_lf = `No then
		  `OK ([], sub_ilist lg_stamp lf, 
		       var_num, l_stamp)
		else
		  let d = trg gs in
		  let (lv, var_num) = many_vars var_num d lf in
		  let sub_lg = sub_ilist lg_stamp lv in
		  let sub_app_lf_sub_lg = sub_app_lf sub_lg in (*memoization *)
		  let lgv = sub_app_lf_sub_lg lg in
		  let lf = 
		    if g_in_lf = `No then lf
		    else sub_app_lf_sub_lg lf
		  in
		  `OK ([(f_pp c lf, f_pp c lgv, l)], sub_lg, var_num, l_stamp)
	    else `Error 
		(coreMiddleError#instance 
		   [Loc l; 
		    Msg ("the lf ilist is rigid, but has to be expanded"
			 ^ "\nlf = (" 
			 ^ PpFFunct.pp_lf lf ^ ")\nlg = ("
			 ^ PpFFunct.pp_lf lg ^ ")")])
	  else if lg_rigid then
	    if lf_small then
	      let lf_stamp = IListBasic.get_stamp lf in
	      let fs = IListBasic.get_elem lf in
	      let f_in_lg = loccurs_in_lf lf_stamp fs lg in
	      if f_in_lg = `Yes
	      then `Error 
		  (coreMiddleError#instance 
		     [Loc l; 
		      Msg ("occur check (2) of ilist stamp "
			   ^ IListBasic.pp_stamp lf
			   ^ " failed in ilist\n"
			   ^ PpFFunct.pp_lf lg)])
	      else	
		if EqFFunct.eq fs (f_ID c) && f_in_lg = `No then
		  `OK ([], sub_ilist lf_stamp lg, 
		       var_num, l_stamp)
		else
		  let d = trg fs in
		  let (lv, var_num) = many_vars var_num d lg in
		  let sub_lf = sub_ilist lf_stamp lv in
		  let sub_app_lf_sub_lf = sub_app_lf sub_lf in (*memoization *)
		  let lfv = sub_app_lf_sub_lf lf in
		  let lg = 
		    if f_in_lg = `No then lg
		    else sub_app_lf_sub_lf lg
		  in
		  `OK ([(f_pp c lg, f_pp c lfv, l)], sub_lf, var_num, l_stamp)
	    else `Error 
		(coreMiddleError#instance 
		   [Loc l; 
		    Msg ("the lg ilist is rigid, but has to be expanded"
			 ^ "\nlg = (" 
			 ^ PpFFunct.pp_lf lg ^ ")\nlf = ("
			 ^ PpFFunct.pp_lf lf ^ ")")])
	  else (* both flexible *)
	    let lf_stamp = IListBasic.get_stamp lf in
	    let lg_stamp = IListBasic.get_stamp lg in
	    let fs = IListBasic.get_elem lf in
	    let gs = IListBasic.get_elem lg in
	    if IListStamp.eq lf_stamp lg_stamp then 
	      if lf_small && lg_small (* needen't be small! *)
	      then
		if EqFFunct.eq fs gs then 
		  (failwith "surprise: lf = lg in both flexible";
		   `OK ([], sub_id, var_num, l_stamp))
		else
		  (failwith "surprise: they are bad!";
		  `OK ([(f_pp c lf, f_pp c lg, l)], sub_id, var_num, l_stamp))
  	      else `Error
	          (coreMiddleError#instance 
		     [Loc l; 
		      Msg ("the ilists' stamps are equal, "
			   ^ "but the ilists are not both empty: \nlf = (" 
			   ^ PpFFunct.pp_lf lf ^ ")\nlg = ("
			   ^ PpFFunct.pp_lf lg ^ ")")])
	    else
	      if lf_small && lg_small then
		if EqFFunct.eq gs (f_ID c) && EqFFunct.eq fs (f_ID c) then
		  let sub_lf = sub_ilist lf_stamp lg in
		  `OK ([], sub_lf, var_num, l_stamp)
		else
		  `OK ([(f_pp c lf, f_pp c lg, l)], sub_id, var_num, l_stamp)
	      else
		let g_in_lf = loccurs_in_lf lg_stamp gs lf in
		let f_in_lg = loccurs_in_lf lf_stamp fs lg in
		if g_in_lf = `Yes
		then `Error 
		    (coreMiddleError#instance 
		       [Loc l; 
			Msg ("occur check (3) of ilist stamp "
			     ^ IListBasic.pp_stamp lg
			     ^ " failed in ilist\n"
			     ^ PpFFunct.pp_lf lf)])
		else if f_in_lg = `Yes
		then `Error 
		    (coreMiddleError#instance 
		       [Loc l; 
			Msg ("occur check (4) of ilist stamp "
			     ^ IListBasic.pp_stamp lf
			     ^ " failed in ilist\n"
			     ^ PpFFunct.pp_lf lg)])
		else
		  if EqFFunct.eq gs (f_ID c) && EqFFunct.eq fs (f_ID c)
		      && g_in_lf = `No && f_in_lg = `No then
		    let (l_future, l_stamp) = fresh_ilist l_stamp c in
		    let lf = IListBasic.move_flexible l_future 
			(IListBasic.set_rigid lf) in
		    let sub_lg = sub_ilist lg_stamp lf in
		    let lg = IListBasic.move_flexible l_future 
			(IListBasic.set_rigid lg) in
		    let lg = sub_app_lf sub_lg lg in
		    let sub_lf = sub_ilist lf_stamp lg in
		    let res_sub = sub_comp sub_lg sub_lf in
		    `OK ([], res_sub, var_num, l_stamp)
		  else
	            let d = trg gs in
	            let (l_future, l_stamp) = fresh_ilist l_stamp d in
	            let (lv, var_num) = many_vars var_num d lf in
                    let lv = IListBasic.move_flexible l_future lv in
		    let sub_lg = sub_ilist lg_stamp lv in

	            let d = trg fs in
	            let (l_future, l_stamp) = fresh_ilist l_stamp d in
	            let (lv, var_num) = many_vars var_num d lg in
                    let lv = IListBasic.move_flexible l_future lv in
		    let sub_lf = sub_ilist lf_stamp lv in

		    let res_sub = sub_comp sub_lg sub_lf in
		    let sub_app_lf_res_sub = sub_app_lf res_sub in
		    let lgv = sub_app_lf_res_sub lg in
		    let lfv = sub_app_lf_res_sub lf in
		    `OK ([(f_pp c lfv, f_pp c lgv, l)], res_sub, 
			 var_num, l_stamp)

    (* performs only one step of unification of a single pair [f] and [g] *)
    let single_step var_num l_stamp f g l =
      let _ =
	if !Tools.debugging then
	  let pr2 (f, g) = 
	    "\n(f = " ^ PpFFunct.pp_f f
	    ^ ",\n g = " ^ PpFFunct.pp_f g ^ ")"
	  in
	  let sl = pr2 (f, g) in
	  (prerr_endline ("\nunify stepping on:" ^ sl); flush stderr)
	else ()
      in
      let c = src f in 
      assert (EqFCat.eq c (src g));
      assert 
	(if EqFCat.eq (trg f) c_BB && EqFCat.eq (trg g) c_BB
	then true
	else failwith ("UnifyFCore.single_step: \nf(" 
		       ^ PpFFunct.pp_f f ^ ")\ng("
		       ^ PpFFunct.pp_f g ^ ")\ntrg f("
		       ^ PpFCat.pp_c (trg f) 
		       ^ ")\ntrg g("
		       ^ PpFCat.pp_c (trg g) ^ ")\n" 
		       ^ Location.t2string l));
      match Funct.t2de f, Funct.t2de g with
(* f = g: |`F_ID _,`F_ID _ -> `OK ([], sub_id, var_num, l_stamp)
          |`F_PR (_, i),`F_PR (_, j) when IdIndex.eq i j -> 
             `OK ([], sub_id, var_num, l_stamp) *)
      |`F_pp (_, lf),`F_pp (_, lg) -> match_lists var_num l_stamp c lf lg l
      |`F_ss (_, lf),`F_ss (_, lg) -> match_lists var_num l_stamp c lf lg l
      |`F_ii f,`F_ii g ->
	  `OK ([(f, g, l)], sub_id, var_num, l_stamp)
      |`F_tt f,`F_tt g ->
	  `OK ([(f, g, l)], sub_id, var_num, l_stamp)
      |`F_ee (lf, f),`F_ee (lg, g) ->
	  `OK ([(f, g, l); (f_pp c lf, f_pp c lg, l)], 
	       sub_id, var_num, l_stamp)
(* f = g: |`F_var (var_num_f, _),`F_var (var_num_g, _) 
          when var_num_f = var_num_g ->
            `OK ([], sub_id, l_stamp) *)
      |`F_var (var_num_f, c), _ ->
          if occurs_in_f var_num_f g
          then `Error
	      (coreMiddleError#instance 
		 [Loc l; 
		  Msg ("occur check (1) of variable "
		       ^ VarStamp.t2string var_num_f
		       ^ " failed in functor\n"
		       ^ PpFFunct.pp_f g)])
          else `OK ([], sub_var var_num_f g, var_num, l_stamp)
      | _,`F_var (var_num_g, d) ->
          if occurs_in_f var_num_g f
          then `Error
	      (coreMiddleError#instance 
		 [Loc l; 
		  Msg ("occur check (2) of variable "
		       ^ VarStamp.t2string var_num_g
		       ^ " failed in functor\n"
		       ^ PpFFunct.pp_f f)])
          else `OK ([], sub_var var_num_g f, var_num, l_stamp)
(* f = g: |`F_COMP (f1, f2),`F_COMP (g1, g2) when ... (i . j) . k ... -> *)
      |`F_finish _, `F_finish _ -> (* f = g !!! *) 
	  `Error
	    (coreMiddleError#instance 
	       [Loc l; 
		Msg ("incompatible finished functors:\nf = (" 
		     ^ PpFFunct.pp_f f ^ ")\ng = ("
		     ^ PpFFunct.pp_f g ^ ")")])
      |`F_finish f, _ ->
	  `OK ([(bury_finish f, g, l)], sub_id, var_num, l_stamp)
      | _,`F_finish g ->
	  `OK ([(f, bury_finish g, l)], sub_id, var_num, l_stamp)
      | _ -> 
	  `Error
	    (coreMiddleError#instance 
	       [Loc l; 
		Msg ("incompatible functors:\nf = (" 
		     ^ PpFFunct.pp_f f ^ ")\ng = ("
		     ^ PpFFunct.pp_f g ^ ")")])

    (* memoization of unification is tricky, because of fresh variables
       therefore we are memoizing single_loop but with a quotient trick *)
    module HC = 
      struct
	type t = 
	 (* VarStamp.t * IListStamp.t *  (* trick *) *)
	      Funct.t * Funct.t * Location.t
	type result = 
	    [`OK of (Funct.t * Funct.t * Location.t) list 
		* SubFFunct.subst * VarStamp.t * IListStamp.t
	        * VarStamp.t * IListStamp.t (* trick *)
  	    |`Error of ErrorRepLib.error]
	let equal (f, g, l) (f', g', l') =
	  Location.eq l l' &&
	  (EqFFunct.eq f f' && EqFFunct.eq g g' ||
	  EqFFunct.eq f g' && EqFFunct.eq g f')
	let hash (f, g, l) = 
	  abs (combine (Location.t2int l) (Funct.t2stamp f + Funct.t2stamp g))
      end
    module MemoStep = Memoize' (HC)

(*     let tbl = MemoStep.create good_hash_size *)

    (* hack for [sub_translate]: *)
    let ct = c_PP (cons (IdIndex.s2type "@translate", c_BB) nil)
    let f_var_ff var_num = f_var var_num ct
    let f_ilist_ff l_stamp = 
      IListBasic.rigid2flexible l_stamp (f_ID ct) nil

    (* this function has small arguments and thus is good for memoization *)

(* not debugged, yet:

  let rec single_loop var_num l_stamp f g l =
      (* rename each variable between start and end to current+offset *)
      let translate 
	  (bad, s, var_num_end, l_stamp_end, var_num_start, l_stamp_start)
	  var_num_current l_stamp_current =
	if VarStamp.eq var_num_start var_num_end &&
	  IListStamp.eq l_stamp_start l_stamp_end then
	  (bad, s, var_num_current, l_stamp_current)
	else
	  let (s_translate, var_num_new, l_stamp_new) = 
	    sub_translate 
	      f_var_ff f_ilist_ff
	      var_num_start l_stamp_start
	      var_num_end l_stamp_end
	      var_num_current l_stamp_current
	  in
	  let sub_app_f_s = sub_app_f s_translate in (* for memoization *)
	  let bad = mapl (fun (f, g, l) -> 
	    (sub_app_f_s f, sub_app_f_s g, l)) bad in
	  let s = sub_app_sub s_translate s in
	  (bad, s, var_num_new, l_stamp_new)
      in
      try
	(match HashStep.find tbl (f, g, l) with
	|`OK pattern -> (* (prerr_string "O"; flush stderr); *)
	    `OK (translate pattern var_num l_stamp)
	|`Error er -> `Error er)
      with Not_found -> (* (prerr_string "."; flush stderr); *)
	let result = single_loop' var_num l_stamp f g l in
	let pattern = 
	  (match result with
	  |`OK (bad, s, var_num', l_stamp') -> 
	      `OK (bad, s, var_num', l_stamp', var_num, l_stamp)
	  |`Error er -> `Error er)
	in
        let _ = MemoStep.add tbl (f, g, l) pattern in
        result
    (* main function: performs unification of the pair [f] and [g] 
       and all its consequences until only bad pairs remain *)
    and*) let rec single_loop(*'*) var_num l_stamp f g l =
      if EqFFunct.eq f g then 
	`OK ([], sub_id, var_num, l_stamp)
      else if is_bad f g then
	`OK ([(f, g, l)], sub_id, var_num, l_stamp)
      else 
      (match single_step var_num l_stamp f g l with
      |`OK (lfg, s, var_num, l_stamp) ->
	  (match multi_loop var_num l_stamp lfg [] with
	  |`OK (bad, s', var_num, l_stamp) -> 
	      `OK (bad, sub_comp s s', var_num, l_stamp)
	  |`Error er -> `Error er)
      |`Error er -> `Error er)
    (* performs unification of lfg and bad and all their consequences
       until only bad pairs remain *)
    and multi_loop var_num l_stamp lfg bad =
      match lfg with
      | [] ->
	  `OK (bad, sub_id, var_num, l_stamp)
      | (f, g, l)::r ->
	  (match single_loop var_num l_stamp f g l with
	  |`OK (sbad, s, var_num, l_stamp) ->
	      if s = sub_id then
		multi_loop var_num l_stamp r (sbad @ bad)
	      else
		let lrb = r @ bad in (* bad may become good *)
		let sub_app_f_s = sub_app_f s in (* for memoization *)
		let lrb = mapl (fun (f, g, l) -> 
		  (sub_app_f_s f, sub_app_f_s g, l)) lrb in
		(match multi_loop var_num l_stamp lrb sbad with
		|`OK (bad, s', var_num, l_stamp) -> 
		    `OK (bad, sub_comp s s', var_num, l_stamp)
		|`Error er -> `Error er)
	  |`Error er -> `Error er)

    (* to check the result of very_bad_loop below *)
    let ass (very_bad, s, var_num, l_stamp) lfg =
      let sub_finish_f_s = sub_finish_f s in (* for memoization *)
      foldl true (fun (f', g', l) r ->
	let f = finish_f (sub_finish_f_s f') in
	let g = finish_f (sub_finish_f_s g') in
	if EqFFunct.eq f g
	    (* && very_bad accounts for the incremented stamps and vars *)
	then 
	  r
	else
	  failwith ("UnifyFCore.unify: \nf' = (" 
		    ^ PpFFunct.pp_f f' 
		    ^ ")\ng' = ("
		    ^ PpFFunct.pp_f g' 
		    ^ ")\nf  = ("
		    ^ PpFFunct.pp_f f 
		    ^ ")\ng  = ("
		    ^ PpFFunct.pp_f g ^ ")\nverybad:"
		    ^ let pr2 (f, g) = 
		      "\n(f  = " ^ PpFFunct.pp_f f
		      ^ ",\n g  = " ^ PpFFunct.pp_f g ^ ")"
		    in
		    foldl "" (fun (f, g, l) r -> 
		      pr2 (f, g) ^ r) very_bad
                    ^ "\nlfg:"
		    ^(foldl "" (fun (f, g, l) r -> 
		      pr2 (f, g) ^ r) lfg))) lfg

    (* performs unification of lfg and bad and all their consequences
       until only very_bad pairs remain *)
    let rec very_bad_loop var_num l_stamp lfg bad =
      (match multi_loop var_num l_stamp lfg bad with
      |`OK (mbad, s, var_num, l_stamp) -> 
	  (match trot var_num l_stamp mbad [] with
	  |`OK (very_bad, s', var_num, l_stamp) -> 
	      let result = (very_bad, sub_comp s s', var_num, l_stamp) in
	      assert (ass result (lfg @ bad));
	      `OK result
	  |`Error er -> `Error er)
      |`Error er -> `Error er)
    (* backtracking of matching [f2] with a composition of projections *)
    and try_pr var_num l_stamp f2 bad l =
      let c = src f2 in 
 	    let _ =
	      if !Tools.debugging then
		(prerr_endline ("\nbacktracking started("); flush stderr)
	      else ()
	    in
      let rec try_p f c =
	(match unPPok c with
	|`OK lc ->
	    bfold (`Error
		     (coreMiddleError#instance 
			[Loc l; 
			 Msg ("void backtracking")]))
	      (fun (i, d) r ->
 	    let _ =
	      if !Tools.debugging then
		prerr_endline (IdIndex.t2s i)
	      else ()
	    in
		(match r with
		|`OK result -> `OK result
		|`Error er ->
		    let fpr = f_PR lc i in
		    let f2my = f_COMP f fpr in
		    try_p f2my d)) lc
	|`Error er ->
	    very_bad_loop var_num l_stamp [(f, f2, l)] bad)
      in 
      match try_p (f_ID c) c with
      |`OK result -> 
 	    let _ =
	      if !Tools.debugging then
		(prerr_endline ("\nbacktracking finished)"); flush stderr)
	      else ()
	    in
	  `OK result
      |`Error er ->
 	    let _ =
	      if !Tools.debugging then
		(prerr_endline ("\nbacktracking aborted)"); flush stderr)
	      else ()
	    in
	  `Error er
    (* try using [try_pr] and fallback to [guess_g] *)
    and guess_comp var_num l_stamp f1 f2 g bad l =
      match
	try_pr var_num l_stamp f2 bad l
      with
      |`OK result -> `OK result
      |`Error er ->
	  (match guess_g var_num l_stamp f1 f2 g l with (* var_num recycled *)
	  |`OK (lfg, var_num, l_stamp) -> very_bad_loop var_num l_stamp lfg bad
	  |`Error er ->
	      `Error
		(coreMiddleError#instance 
		   [Loc l; 
		    Msg ("backtracking failed: " ^ er)]))
    (* find not very_bad pair and guess its form *)
    and trot var_num l_stamp bad very_bad =
      match bad with
      | [] -> 
	  `OK (very_bad, sub_id, var_num, l_stamp)
      | (f, g, l)::r ->
	  if is_very_bad f g then 
	    trot var_num l_stamp r ((f, g, l)::very_bad)
	  else 
 	    let _ =
	      if !Tools.debugging then
		let pr2 (f, g) = 
		  "\n(f = " ^ PpFFunct.pp_f f
		  ^ ",\n g = " ^ PpFFunct.pp_f g ^ ")"
		in
		let sl = foldl "" (fun (f, g, l) r -> pr2 (f, g) ^ r) bad in
		(prerr_endline ("\nunify guessing at:" ^ sl); flush stderr)
	      else ()
	    in
	    (match Funct.t2de f, Funct.t2de g with
	    |`F_COMP (f1, f2), _ when is_var f2 && not (is_var g) ->
		guess_comp var_num l_stamp f1 f2 g (bad @ very_bad) l
	    | _,`F_COMP (g1, g2) when is_var g2 && not (is_var f) ->
		guess_comp var_num l_stamp g1 g2 f (bad @ very_bad) l
	    | _, _ -> failwith "UnifyFCore.trot: f and g not bad")

    let unify var_num l_stamp lfg = very_bad_loop var_num l_stamp lfg []
  end

module UnifyFCore = UnifyFCore' (IdIndex) (IListBasic) (IList)
    (Location) (ErrorRepLib) (Stamp) (SemFCat) (SemFFunct) 
    (SrcFCore) (PpFCat) (PpFFunct) (EqFCat) (EqFFunct) (OccursFCore) 
    (SubFFunct)
