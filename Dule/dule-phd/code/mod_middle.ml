(* Copyright (C) 2003--2006 Mikolaj Konarski
 *
 * This file is part of the Dule compiler.
 * The Dule compiler is released under the GNU General Public License (GPL).
 * Please see the file Dule-LICENSE for license information.
 *
 * $Id: mod_middle.ml,v 1.63 2007-08-25 13:21:30 mikon Exp $
 *) 

open Error_rep open Mod_back
open Middle_middle open Tools open Core_back

module type EqIDule =
  sig
    module Location : Location
    module ErrorRepLib : ErrorRepLib
    module IDule : IDule
    with module Location = Location
    val eq_sign : 
	IDule.sign -> IDule.sign ->
	  [`OK of bool
          |`Error of ErrorRepLib.error]
  end

module EqIDule' 
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Location : Location)
    (ErrorRepLib : ErrorRepLib
    with module Location = Location)
    (Cat : T)
    (Funct : T)
    (Trans : T)
    (EqFFunct : EqFFunct with type Funct.t = Funct.t)
    (Sign : Sign
    with type Funct.t = Funct.t)
    (Dule : Dule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with module Sign = Sign)
    (IDule : IDule 
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with module Sign = Sign
    with module Dule = Dule)
    (ElabIDule : ElabIDule
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with module Sign = Sign
    with module Dule = Dule
    with module IDule = IDule)
    : (EqIDule
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with module IDule = IDule) =
  struct
    module Location = Location
    module ErrorRepLib = ErrorRepLib
    module IDule = IDule

    let eq_sign r s = pri "(fo ";
      (match ElabIDule.el_sign r with
      |`OK f ->
	  (match ElabIDule.el_sign s with
	  |`OK g -> pri " rth)";
	      `OK (EqFFunct.eq (Sign.s2f f) (Sign.s2f g))
	  |`Error er -> `Error er) (* in error say that we are inside forth *)
      |`Error er -> `Error er)
   end

module EqIDule = EqIDule' (IdIndex) (IList) (Location) (ErrorRepLib)
    (ACat) (AFunct) (ATrans) (EqFFunct) (Sign) (Dule) (IDule) (ElabIDule)


module type OccursIDule =
  sig
    module IdIndex : IdIndex
    module IListBasic : IListBasic with type Index.t = IdIndex.t
    module IList : IList with type Index.t = IdIndex.t
    module IDule : IDule
    with module IdIndex = IdIndex 
    with module IList = IList

    val occurs_in_s : IDule.VarStamp.t -> IDule.sign -> bool
    val loccurs_in_ls : IListBasic.IListStamp.t -> IDule.sign IList.t -> bool
  end

module OccursIDule'
    (IdIndex : IdIndex)
    (IListBasic : IListBasic with type Index.t = IdIndex.t)
    (IList : IList 
    with type Index.t = IdIndex.t
    with type 'a IListBasic.t = 'a IListBasic.t)
    (IDule : IDule
    with module IdIndex = IdIndex 
    with module IList = IList)
    : (OccursIDule
    with module IdIndex = IdIndex 
    with module IListBasic = IListBasic
    with module IList = IList
    with module IDule = IDule) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module IListBasic = IListBasic
    module IDule = IDule
    open IDule

    let rec occurs_in_s var_num s =
      match term_sign s with
      | S_Pp ls -> occurs_in_ls var_num ls
      | S_Bb (r, lc, lf) -> occurs_in_s var_num r
      | SC_Bb (r, lc, lf) -> occurs_in_s var_num r
      | S_Ww (m1, s2) -> occurs_in_m var_num m1 || occurs_in_s var_num s2
      | S_Mm (n, s) -> false
      | S_Var var_num' -> VarStamp.eq var_num var_num'

    and occurs_in_ls var_num ls =
      IList.vexists (occurs_in_s var_num) ls ||
      (IListBasic.is_flexible ls 
	 && (if occurs_in_s var_num (IListBasic.get_elem ls) then
              failwith "surpise: occurs_in_s in get_elem!" else false))

    and occurs_in_m var_num m =
      match term_dule m with
      | IDule.M_Id s -> 
	  occurs_in_s var_num s
      | IDule.M_Comp (m1, m2) -> 
 	  (occurs_in_m var_num m1 || occurs_in_m var_num m2)
      | IDule.M_Pr (lr, i) -> 
	  occurs_in_ls var_num lr
      | IDule.M_Record (r, lm) -> 
 	  (occurs_in_s var_num r || occurs_in_lm var_num lm)
      | IDule.M_Base (r, s, lg, lt) -> 
	  occurs_in_s var_num s || occurs_in_s var_num r
      | IDule.MC_Base (r, s, lg, lt) -> 
	  occurs_in_s var_num s || occurs_in_s var_num r
      | IDule.M_Inst (m1, m2) ->
 	  (occurs_in_m var_num m1 || occurs_in_m var_num m2)
      | IDule.M_Trim (m1, r2) ->
 	  (occurs_in_m var_num m1 || occurs_in_s var_num r2)
      | IDule.M_Slash (m1, r2, s2) ->
 	  (occurs_in_m var_num m1 || occurs_in_s var_num r2 || 
	  occurs_in_s var_num s2)
      | IDule.M_Accord (lr, lm) ->
 	  (occurs_in_ls var_num lr || occurs_in_lm var_num lm)
      | IDule.M_Concord (lr, lm) ->
 	  (occurs_in_ls var_num lr || occurs_in_lm var_num lm)
      | IDule.M_Link (lr, lm) ->
 	  (occurs_in_ls var_num lr || occurs_in_lm var_num lm)
      | IDule.M_Ind (lr, lm) ->
 	  (occurs_in_ls var_num lr || occurs_in_lm var_num lm)
      | IDule.M_CoInd (lr, lm) ->
 	  (occurs_in_ls var_num lr || occurs_in_lm var_num lm)
      | IDule.M_Memo (n, m) -> 
	  occurs_in_m var_num m
      | IDule.M_Finish t -> 
	  false
    and occurs_in_lm var_num lm =
      IList.vexists (occurs_in_m var_num) lm

    let rec loccurs_in_s l_stamp s =
      match term_sign s with
      | S_Pp ls -> loccurs_in_ls l_stamp ls
      | S_Bb (r, lc, lf) -> loccurs_in_s l_stamp r
      | SC_Bb (r, lc, lf) -> loccurs_in_s l_stamp r
      | S_Ww (m1, s2) -> loccurs_in_m l_stamp m1 || loccurs_in_s l_stamp s2
      | S_Mm (n, s) -> false
      | S_Var _ -> false
    and loccurs_in_ls l_stamp ls =
      IList.vexists (loccurs_in_s l_stamp) ls ||
      (IListBasic.is_flexible ls 
	 && (IListBasic.IListStamp.eq (IListBasic.get_stamp ls) l_stamp || 
	 if loccurs_in_s l_stamp (IListBasic.get_elem ls) then
           failwith "surpise: loccurs_in_ls in get_elem!" else false))

    and loccurs_in_m l_stamp m =
      match term_dule m with
      | IDule.M_Id s -> 
	  loccurs_in_s l_stamp s
      | IDule.M_Comp (m1, m2) -> 
 	  (loccurs_in_m l_stamp m1 || loccurs_in_m l_stamp m2)
      | IDule.M_Pr (lr, i) -> 
	  loccurs_in_ls l_stamp lr
      | IDule.M_Record (r, lm) -> 
 	  (loccurs_in_s l_stamp r || loccurs_in_lm l_stamp lm)
      | IDule.M_Base (r, s, lg, lt) -> 
	  loccurs_in_s l_stamp s || loccurs_in_s l_stamp r
      | IDule.MC_Base (r, s, lg, lt) ->
	  loccurs_in_s l_stamp s || loccurs_in_s l_stamp r
      | IDule.M_Inst (m1, m2) ->
 	  (loccurs_in_m l_stamp m1 || loccurs_in_m l_stamp m2)
      | IDule.M_Trim (m1, r2) ->
 	  (loccurs_in_m l_stamp m1 || loccurs_in_s l_stamp r2)
      | IDule.M_Slash (m1, r2, s2) ->
 	  (loccurs_in_m l_stamp m1 || loccurs_in_s l_stamp r2 || 
	  loccurs_in_s l_stamp s2)
      | IDule.M_Accord (lr, lm) ->
 	  (loccurs_in_ls l_stamp lr || loccurs_in_lm l_stamp lm)
      | IDule.M_Concord (lr, lm) ->
 	  (loccurs_in_ls l_stamp lr || loccurs_in_lm l_stamp lm)
      | IDule.M_Link (lr, lm) ->
 	  (loccurs_in_ls l_stamp lr || loccurs_in_lm l_stamp lm)
      | IDule.M_Ind (lr, lm) ->
 	  (loccurs_in_ls l_stamp lr || loccurs_in_lm l_stamp lm)
      | IDule.M_CoInd (lr, lm) ->
 	  (loccurs_in_ls l_stamp lr || loccurs_in_lm l_stamp lm)
      | IDule.M_Memo (n, m) -> 
	  loccurs_in_m l_stamp m
      | IDule.M_Finish t -> 
	  false
    and loccurs_in_lm l_stamp lm =
      IList.vexists (loccurs_in_m l_stamp) lm
  end

module OccursIDule = OccursIDule' (IdIndex) (IListBasic) (IList) (IDule)


module type TypeISign =
  sig
    module IdIndex : IdIndex
    module IList : IList 
    with type Index.t = IdIndex.t
    module VarStamp : Stamp
    module IDule : IDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with module VarStamp = VarStamp

    type t = IDule.sign
    val type_sub_app_f : (* memoized, so with side-effects! *)
	bool ->
	  (t -> t) lazy_t -> 
	    (t IList.t -> t IList.t) ->
	      (VarStamp.t -> t -> t) -> 
		t -> t 
    val f_COMP : t -> t -> t
    val f_pp_nil : t -> t

    val sub_app_m :
	(t -> t) -> 
	  (t IList.t -> t IList.t) ->
	    IDule.dule -> IDule.dule 
  end

module TypeISign'
    (IdIndex : IdIndex)
    (IList : IList 
    with type Index.t = IdIndex.t)
    (VarStamp : Stamp)
    (IDule : IDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with module VarStamp = VarStamp)
    : (TypeISign
    with module IdIndex = IdIndex
    with module IList = IList
    with module VarStamp = VarStamp
    with module IDule = IDule) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module VarStamp = VarStamp
    module IDule = IDule
    open IList
    open IDule

    type t = IDule.sign

    let sub_app_m sub_app_s sub_app_ls old_m =
     let rec sub_app_m old_m =
      let l = loc_dule old_m in
      match term_dule old_m with
      | IDule.M_Id r -> 
	  (l, M_Id (sub_app_s r))
      | IDule.M_Comp (m1, m2) -> 
	  (l, M_Comp (sub_app_m m1, sub_app_m m2))
      | IDule.M_Pr (lr, i) -> 
	  (l, M_Pr (sub_app_ls lr, i))
      | IDule.M_Record (r, lm) -> 
	  (l, M_Record (sub_app_s r, sub_app_lm lm))
      | IDule.M_Base (r, s1, lg, lt) -> 
	  (l, M_Base (sub_app_s r, sub_app_s s1, lg, lt))
      | IDule.MC_Base (r, s1, lg, lt) -> 
	  (l, MC_Base (sub_app_s r, sub_app_s s1, lg, lt))
      | IDule.M_Inst (m1, m2) -> 
	  (l, M_Inst (sub_app_m m1, sub_app_m m2))
      | IDule.M_Trim (m1, r2) -> 
	  (l, M_Trim (sub_app_m m1, sub_app_s r2))
      | IDule.M_Slash (m1, r2, s2) -> 
	  (l, M_Slash (sub_app_m m1, sub_app_s r2, sub_app_s s2))
      | IDule.M_Accord (lr, lm) ->
 	  (l, M_Accord (sub_app_ls lr, sub_app_lm lm))
      | IDule.M_Concord (lr, lm) ->
 	  (l, M_Concord (sub_app_ls lr, sub_app_lm lm))
      | IDule.M_Link (lr, lm) -> 
 	  (l, M_Link (sub_app_ls lr, sub_app_lm lm))
      | IDule.M_Ind (lr, lm) ->
 	  (l, M_Ind (sub_app_ls lr, sub_app_lm lm))
      | IDule.M_CoInd (lr, lm) ->
 	  (l, M_CoInd (sub_app_ls lr, sub_app_lm lm))
      | IDule.M_Memo (n, m) -> 
	  let m' = sub_app_m m in 
	  (* if m = m' then old_m *) (* for sharing in memory *)
	  (* else (l, M_Memo (n, m')) --- this causes a huge slowdown! *)
	  (l, M_Memo (n, m'))
      | IDule.M_Finish t -> old_m
     and sub_app_lm old_lm =
       vmap sub_app_m old_lm
     in
     sub_app_m old_m
	 
    let type_sub_app_f finishp sub_app_f sub_app_lf sub_app_var old_f =
      let sub_app_f = Lazy.force_val sub_app_f in
      let l = loc_sign old_f in
      match term_sign old_f with
      | S_Pp ls -> (l, S_Pp (sub_app_lf ls))
      | S_Bb (r, lc, lf) -> (l, S_Bb (sub_app_f r, lc, lf))
      | SC_Bb (r, lc, lf) -> (l, SC_Bb (sub_app_f r, lc, lf))
      | S_Ww (m1, s2) ->
          let sub_app_m = sub_app_m sub_app_f sub_app_lf in
          (l, S_Ww (sub_app_m m1, sub_app_f s2))
      | S_Mm (n, r) -> old_f
      | S_Var var_num -> 
	  sub_app_var var_num old_f

    let f_COMP gs fs = fs (* for now --- weak *)

    let f_pp_nil old_f = (loc_sign old_f, S_Pp nil)
  end

module TypeISign = TypeISign' (IdIndex) (IList) (Stamp) (IDule)

module SubISign = Substitution' (TypeISign)


module type UnifyIDule =
  sig
    module IListBasic : IListBasic
    module IList : IList 
    with type 'a IListBasic.t = 'a IListBasic.t
    module IDule : IDule
    module Location : Location
    module ErrorRepLib : ErrorRepLib
    with module Location = Location
    module SubISign : sig type subst end

    val freshen_ilist : IListBasic.IListStamp.t -> IDule.sign IList.t -> 
      Location.t -> IDule.sign IList.t * IListBasic.IListStamp.t

    val unify : IDule.VarStamp.t -> IListBasic.IListStamp.t -> 
      (IDule.sign * IDule.sign * Location.t) list -> 
      (IDule.sign * IDule.sign * Location.t) list -> 
      [`OK of SubISign.subst * IDule.VarStamp.t * IListBasic.IListStamp.t
      |`Error of ErrorRepLib.error]
  end

module UnifyIDule'
    (IdIndex : IdIndex)
    (IListBasic : IListBasic with type Index.t = IdIndex.t)
    (IList : IList 
    with type Index.t = IdIndex.t
    with type 'a IListBasic.t = 'a IListBasic.t)
    (Location : Location)
    (ErrorRepLib : ErrorRepLib
    with module Location = Location)
    (IDule : IDule 
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location)
    (EqIDule : EqIDule
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with module IDule = IDule) 
    (OccursIDule : OccursIDule
    with module IdIndex = IdIndex 
    with module IListBasic = IListBasic
    with module IList = IList
    with module IDule = IDule)
    (SubISign : Substitution
    with module IdIndex = IdIndex 
    with module IListStamp = IListBasic.IListStamp
    with module IListBasic = IListBasic
    with module IList = IList
    with module VarStamp = IDule.VarStamp
    with type Type.t = IDule.sign)
    (PpIDule : PpIDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with module IDule = IDule)
    : (UnifyIDule
    with module IListBasic = IListBasic
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with module IDule = IDule
    with type SubISign.subst = SubISign.subst) =
  struct
    module IList = IList
    module IListBasic' = IListBasic
    module IDule = IDule
    module Location = Location
    module ErrorRepLib = ErrorRepLib
    module SubISign = SubISign
    open IList
    open ErrorRepLib
    open IDule
    open OccursIDule
    open SubISign
    module IListBasic = IListBasic'

    let rec foldl init f l =
      (match l with
      | [] -> init
      | v :: r -> foldl (f v init) f r)

    let rec mapl f l = 
      (match l with
      | [] -> []
      | v :: r -> f v :: mapl f r)

    let mm_nil = S_Mm ("nil", (Location.none, S_Pp IList.nil))

    let freshen_ilist l_stamp ls l =
      (IListBasic.rigid2flexible l_stamp (l, mm_nil) ls, 
       IListBasic.IListStamp.inc l_stamp)

    let equal_lfg ls lr l = [((l, S_Pp ls), (l, S_Pp lr), l)]

    let greater_lgt ls lr l =
      if IList.is_nil lr && IListBasic.is_rigid lr then [] (* for speed *)
      else [((l, S_Pp ls), (l, S_Pp lr), l)]

    let luni var_num l_stamp lf lg l =
      let lh = 
        bfold []
          (fun (i, f) lh ->
	    (match find_ok i lg with
	    |`OK g -> (f, g, l)::lh
	    |`Error er -> lh)) lf
      in
      match lh with
      | (f, g, l)::lfg ->
	  if !Tools.debugging then 
	    (prerr_endline "\nsignature luni on lh..."; 
	     flush stderr) else ();
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
	  `OK (equal_lfg lf_lg lg_lf l @ lh, [],
	       sub_id, var_num, l_stamp)
      | [] ->
	  if !Tools.debugging then 
	    (prerr_endline "\nsignature luni on []..."; 
	     flush stderr) else ();
	  let lf_rigid = IListBasic.is_rigid lf in
	  let lg_rigid = IListBasic.is_rigid lg in
	  let lf_small = is_nil lf in
	  let lg_small = is_nil lg in
	  if lf_rigid && lg_rigid then 
	    if lf_small && lg_small
	    then `OK ([], [], sub_id, var_num, l_stamp)
	    else `Error
		(modMiddleError#instance 
		   [Loc l;  (* loc of an element of the nonempty? *)
		    Msg ("the ilists are both rigid, but not both empty: \nlf = (" 
			 ^ PpIDule.pp_ls lf ^ ")\nlg = ("
			 ^ PpIDule.pp_ls lg ^ ")")])
	  else if lf_rigid then
	    if lg_small then
	      let lg_stamp = IListBasic.get_stamp lg in
	      if loccurs_in_ls lg_stamp lf
	      then `Error
		  (modMiddleError#instance 
		     [Loc l; (* let loccurs_in_ls return location? *)
		      Msg ("occur check (1) of ilist stamp "
			   ^ IListBasic.pp_stamp lg
			   ^ " failed in ilist\n"
			   ^ PpIDule.pp_ls lf)])
	      else
		  `OK ([], [], sub_ilist lg_stamp lf, 
		       var_num, l_stamp)
	    else `Error 
		(modMiddleError#instance 
		   [Loc l; 
		    Msg ("the lf ilist is rigid, but has to be expanded\nlf = (" 
			 ^ PpIDule.pp_ls lf ^ ")\nlg = ("
			 ^ PpIDule.pp_ls lg ^ ")")])
	  else if lg_rigid then
	    if lf_small then
	      let lf_stamp = IListBasic.get_stamp lf in
	      if loccurs_in_ls lf_stamp lg
	      then `Error 
		  (modMiddleError#instance 
		     [Loc l; 
		      Msg ("occur check (2) of ilist stamp "
			   ^ IListBasic.pp_stamp lf
			   ^ " failed in ilist\n"
			   ^ PpIDule.pp_ls lg)])
	      else	
		  `OK ([], [], sub_ilist lf_stamp lg, 
		       var_num, l_stamp)
	    else `Error 
		(modMiddleError#instance 
		   [Loc l; 
		    Msg ("the lg ilist is rigid, but has to be expanded\nlg = (" 
			 ^ PpIDule.pp_ls lg ^ ")\nlf = ("
			 ^ PpIDule.pp_ls lf ^ ")")])
	  else (* both flexible *)
	    let lf_stamp = IListBasic.get_stamp lf in
	    let lg_stamp = IListBasic.get_stamp lg in
	  if IListStamp.eq lf_stamp lg_stamp then 
	    if lf_small && lg_small (* needen't be small! *)
	    then
	      `OK ([], [], sub_id, var_num, l_stamp)
  	    else `Error
	        (modMiddleError#instance 
		   [Loc l; 
		    Msg ("the ilists' stamps are equal, but the ilists are not both empty: \nlf = (" 
			 ^ PpIDule.pp_ls lf ^ ")\nlg = ("
			 ^ PpIDule.pp_ls lg ^ ")")])
	  else
	    if lf_small && lg_small then
		let sub_lf = sub_ilist lf_stamp lg in
		`OK ([], [], sub_lf, var_num, l_stamp)
	    else
	      if loccurs_in_ls lg_stamp lf
	      then `Error 
		  (modMiddleError#instance 
		     [Loc l; 
		      Msg ("occur check (3) of ilist stamp "
			   ^ IListBasic.pp_stamp lg
			   ^ " failed in ilist\n"
			   ^ PpIDule.pp_ls lf)])
	      else if loccurs_in_ls lf_stamp lg
	      then `Error 
		  (modMiddleError#instance 
		     [Loc l; 
		      Msg ("occur check (4) of ilist stamp "
			   ^ IListBasic.pp_stamp lf
			   ^ " failed in ilist\n"
			   ^ PpIDule.pp_ls lg)])
	      else
		  let (l_future, l_stamp) = freshen_ilist l_stamp nil l in
		  let lf = IListBasic.move_flexible l_future 
		      (IListBasic.set_rigid lf) in
		  let sub_lg = sub_ilist lg_stamp lf in
		  let lg = IListBasic.move_flexible l_future 
		      (IListBasic.set_rigid lg) in
		  let lg = sub_app_lf sub_lg lg in
		  let sub_lf = sub_ilist lf_stamp lg in
		  let res_sub = sub_comp sub_lg sub_lf in
		  `OK ([], [], res_sub, var_num, l_stamp)

    let gtuni var_num l_stamp lf lg l = (* lf >= lg *)
      let lh = 
        bfold []
          (fun (i, f) lh ->
	    (match find_ok i lf with
	    |`OK g -> (f, g, l)::lh
	    |`Error er -> lh)) lg
      in
      match lh with
      | (f, g, l)::lfg ->
	  if !Tools.debugging then 
	    (prerr_endline "\nsignature gtuni on lh..."; 
	     flush stderr) else ();
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
	  `OK (lh, greater_lgt lf_lg lg_lf l, 
	       sub_id, var_num, l_stamp)
      | [] ->
	  let lf_rigid = IListBasic.is_rigid lf in
	  let lg_rigid = IListBasic.is_rigid lg in
	  (* let lf_small = is_nil lf in --- unused! good! *)
	  let lg_small = is_nil lg in
	  if lf_rigid && lg_rigid then 
	    ((if !Tools.debugging then 
	      (prerr_endline "\nsignature gtuni on [] both rigid..."; 
	       flush stderr) else ());
	    if lg_small
	    then `OK ([], [], sub_id, var_num, l_stamp)
	    else `Error
		(modMiddleError#instance 
		   [Loc l; 
		    Msg ("gt:the ilists are both rigid, but lg is not empty: \nlf = (" 
			 ^ PpIDule.pp_ls lf ^ ")\nlg = ("
			 ^ PpIDule.pp_ls lg ^ ")")]))
	  else if lg_rigid then
	    ((if !Tools.debugging then 
	      (prerr_endline "\nsignature gtuni on [] lg_rigid..."; 
	       flush stderr) else ());
	    if lg_small
	    then `OK ([], [], sub_id, var_num, l_stamp)
	    else
	      let lf_stamp = IListBasic.get_stamp lf in
	      if loccurs_in_ls lf_stamp lg
	      then `Error 
		  (modMiddleError#instance 
		     [Loc l; 
		      Msg ("gt:occur check (2) of ilist stamp "
			   ^ IListBasic.pp_stamp lf
			   ^ " failed in ilist\n"
			   ^ PpIDule.pp_ls lg)])
	      else	
		  let (l_future, l_stamp) = freshen_ilist l_stamp lg l in
		  let sub_lf = sub_ilist lf_stamp l_future in
		  `OK ([], [], sub_lf, var_num, l_stamp))
	  else if lf_rigid then (* beware of the loop! *)
	    ((if !Tools.debugging then 
	      (prerr_endline "\nsignature gtuni on [] lf_rigid..."; 
	       flush stderr) else ());
	    if lg_small then
	      `OK ([], greater_lgt lf lg l, sub_id, 
		   var_num, l_stamp)
	    else `Error 
		(modMiddleError#instance 
		   [Loc l; 
		    Msg ("gt:the lf ilist is rigid, but has to be expanded\nlf = (" 
			 ^ PpIDule.pp_ls lf ^ ")\nlg = ("
			 ^ PpIDule.pp_ls lg ^ ")")]))
	  else (* both flexible *)
	    ((if !Tools.debugging then 
	      (prerr_endline "\nsignature gtuni on [] both flexible..."; 
	       flush stderr) else ());
	    let lf_stamp = IListBasic.get_stamp lf in
	    let lg_stamp = IListBasic.get_stamp lg in
	  if IListStamp.eq lf_stamp lg_stamp then 
	    if lg_small (* needen't be small! *)
	    then
	      `OK ([], [], sub_id, var_num, l_stamp)
  	    else `Error
	        (modMiddleError#instance 
		   [Loc l; 
		    Msg ("gt:the ilists' stamps are equal, but the ilists are not both empty: \nlf = (" 
			 ^ PpIDule.pp_ls lf ^ ")\nlg = ("
			 ^ PpIDule.pp_ls lg ^ ")")])
	  else
	    if lg_small then (* beware of the loop! *)
	      `OK ([], greater_lgt lf lg l, 
		   sub_id, var_num, l_stamp)
	    else
	      if loccurs_in_ls lf_stamp lg
	      then `Error 
		  (modMiddleError#instance 
		     [Loc l; 
		      Msg ("gt:occur check (3) of ilist stamp "
			   ^ IListBasic.pp_stamp lf
			   ^ " failed in ilist\n"
			   ^ PpIDule.pp_ls lg)])
	      else
		  let (l_future, l_stamp) = 
		    freshen_ilist l_stamp (IListBasic.set_rigid lg) l in
		  let sub_lf = sub_ilist lf_stamp l_future in
		  let lf = sub_app_lf sub_lf lf in
		  let lg = IListBasic.move_flexible lg nil in
		  `OK ([],  greater_lgt lf lg l, 
		       sub_lf, var_num, l_stamp))

    let rec no_var s = 
      match term_sign s with
      | S_Pp ls -> IListBasic.is_rigid ls && vforall no_var ls
      | S_Bb (r, lc, lf) -> true (* ! *)
      | SC_Bb (r, lc, lf) -> true (* ! *)
      | S_Ww (m1, s2) -> true (* ! *)
      | S_Mm (n, s) -> true
      | S_Var _ -> false

    let rec is_bad_lfg r s =
      match term_sign r, term_sign s with (* bad_lfg always stay bad *)
      | S_Var var_num_r, _ -> false
      | _, S_Var var_num_s -> false
      | S_Pp _, S_Pp _ -> false
      | S_Pp _, _ -> no_var r
      | _, S_Pp _ -> no_var s
      | S_Mm (n1, r), S_Mm (n2, s) -> n1 = "" || n1 <> n2
	  (* a hack --- I know r and s are var-free *)
      | _, _ -> true

    let rec is_bad_lgt r s = (* r >= s *)
      match term_sign r, term_sign s with (* bad_lgt often turn good *)
      | S_Pp lr, S_Pp ls -> is_nil ls && IListBasic.is_flexible ls
      | _ -> false

    let rec unPp_ok l_stamp s =
      match term_sign s with
      | S_Pp ls -> `OK (ls, sub_id, l_stamp)
      | S_Bb _ | SC_Bb _ ->
	  `Error
	    (modMiddleError#instance 
	       [Loc (loc_sign s); 
		Msg ("this signature should be a product:\n" 
		     ^ PpIDule.pp_s s)])
      | S_Ww (m1, s2) -> (* this should be banned if there is any sharing *)
	  (match unPp_ok l_stamp s2 with
	  |`OK (ls2, sub, l_stamp) -> 
	      `OK (vmap (fun s -> (* weak, make this ilist flexbile *)
		(loc_sign s, 
		 S_Ww ((loc_dule m1, M_Slash (m1, s2, s)), s))) ls2, 
		   sub, l_stamp)
	  |`Error er -> `Error er)
      | S_Mm (n, s) ->
	  (match unPp_ok l_stamp s with
	  |`OK (ls, sub, l_stamp) -> 
	      `OK (vmap (fun s -> (loc_sign s, S_Mm ("", s))) ls, sub, l_stamp)
	  |`Error er -> `Error er)
      | S_Var var_num ->
	  let l = loc_sign s in
	  let (lr, l_stamp) = freshen_ilist l_stamp IList.nil l in
	  let sub = sub_var var_num (l, S_Pp lr) in
	  `OK (lr, sub, l_stamp)

    let unistep_lfg var_num l_stamp r s l =
      match term_sign r, term_sign s with
      | S_Var var_num_r, S_Var var_num_s
	when var_num_r = var_num_s ->
	  if !Tools.debugging then 
	    (prerr_endline "\nsignature unistep on var var..."; 
	     flush stderr) else ();
          `OK ([], [], sub_id, var_num, l_stamp)
      | S_Var var_num_r, _ ->
	  if !Tools.debugging then 
	    (prerr_endline "\nsignature unistep on var _..."; 
	     flush stderr) else ();
          if occurs_in_s var_num_r s
          then `Error
	      (modMiddleError#instance 
		 [Loc l; 
		  Msg ("occur check (1) of variable "
		       ^ VarStamp.t2string var_num_r
		       ^ " failed in signature\n"
		       ^ PpIDule.pp_s s)])
          else `OK ([], [], sub_var var_num_r s, var_num, l_stamp)
      | _, S_Var var_num_s ->
	  if !Tools.debugging then 
	    (prerr_endline "\nsignature unistep on _ var..."; 
	     flush stderr) else ();
          if occurs_in_s var_num_s r
          then `Error
	      (modMiddleError#instance 
		 [Loc l; 
		  Msg ("occur check (2) of variable "
		       ^ VarStamp.t2string var_num_s
		       ^ " failed in signature\n"
		       ^ PpIDule.pp_s r)])
          else `OK ([], [], sub_var var_num_s r, var_num, l_stamp)
      | S_Pp lr, S_Pp ls ->
	  if !Tools.debugging then 
	    (prerr_endline "\nsignature unistep on pp pp..."; 
	     flush stderr) else ();
	  luni var_num l_stamp lr ls l
      | S_Pp lr, _ ->
	  if !Tools.debugging then 
	    (prerr_endline "\nsignature unistep on pp _..."; 
	     flush stderr) else ();
	  (match unPp_ok l_stamp s with
	  |`OK (ls, sub, l_stamp) ->
	      `OK (equal_lfg lr ls l, [], 
		   sub, var_num, l_stamp)
	  |`Error er -> `Error er)
      | _, S_Pp ls ->
	  if !Tools.debugging then 
	    (prerr_endline "\nsignature unistep on _ pp..."; 
	     flush stderr) else ();
	  (match unPp_ok l_stamp r with
	  |`OK (lr, sub, l_stamp) ->
	      `OK (equal_lfg lr ls l, [], 
		   sub, var_num, l_stamp)
	  |`Error er -> `Error er)
      | S_Mm (n1, r), S_Mm (n2, s) when n1 <> "" && n1 = n2 ->
	  if !Tools.debugging then 
	    (prerr_endline "\nsignature unistep on mm mm..."; 
	     flush stderr) else ();
	  `OK ([], [], sub_id, var_num, l_stamp)
      | _ -> assert false (* is_bad_lfg didn't work *)

    let unistep_lgt var_num l_stamp r s l = (* r >= s *)
      match term_sign r, term_sign s with
      | S_Var var_num_r, S_Var var_num_s
	when var_num_r = var_num_s ->
	  if !Tools.debugging then 
	    (prerr_endline "\nsignature lgtstep on var var..."; 
	     flush stderr) else ();
          `OK ([], [], sub_id, var_num, l_stamp)
      | S_Mm (n1, r), S_Mm (n2, s) when n1 <> "" && n1 = n2 -> 
	  if !Tools.debugging then 
	    (prerr_endline "\nsignature lgtstep on mm mm..."; 
	     flush stderr) else ();
	    `OK ([], [], sub_id, var_num, l_stamp)
      | S_Pp lr, S_Pp ls -> 
	  if !Tools.debugging then 
	    (prerr_endline "\nsignature lgtstep on pp pp..."; 
	     flush stderr) else ();
	    gtuni var_num l_stamp lr ls l
      | _ ->
	  if !Tools.debugging then 
	    (prerr_endline "\nsignature lgtstep on _..."; 
	     flush stderr) else ();
	  (match unPp_ok l_stamp r with
	  |`OK (lr, subr, l_stamp) ->
	      (match unPp_ok l_stamp s with
	      |`OK (ls, subs, l_stamp) ->
		  `OK ([], greater_lgt lr ls l, 
		       sub_comp subr subs, var_num, l_stamp)
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)

    let rec trot bad_lfg =
      if !Tools.debugging then 
	(prerr_endline "\nsignature trotting..."; 
	 flush stderr) else ();
      match bad_lfg with
      | [] -> 
	  `OK ()
      | (f, g, l)::r ->
 	  let _ =
            if !Tools.debugging then
	      let pr2 (f, g) = 
		"\n(f = " ^ PpIDule.pp_s f
		^ ",\n g = " ^ PpIDule.pp_s g ^ ")"
	      in
	      let sl = pr2 (f, g) in
	      (prerr_endline ("\nsign unify guessing at:" ^ sl); 
	       flush stderr)
	    else ()
	  in
	  (match EqIDule.eq_sign f g with
	  |`OK equal -> 
	      if equal then
		trot r
	      else
		`Error
		  (modMiddleError#instance 
		     [Loc l; Loc (loc_sign f); Loc (loc_sign g); 
		      Msg ("forth incompatible signatures:\nr = (" 
			   ^ PpIDule.pp_s f ^ ")\ns = ("
			   ^ PpIDule.pp_s g ^ ")")])
	  |`Error er -> `Error er)

    let rec unify var_num l_stamp lfg lgt =
      if !Tools.debugging then 
	(prerr_endline "\nsignature unifying..."; 
	 flush stderr) else ();
      let r = step var_num l_stamp lfg lgt [] [] in
      if !Tools.debugging then 
	(prerr_endline "\nsignature unification complete..."; 
	 flush stderr) else ();
      r
    and step var_num l_stamp lfg lgt bad_lfg bad_lgt =
      if !Tools.debugging then 
	(prerr_endline "\nsignature stepping..."; 
	 flush stderr) else ();
      (match lfg, lgt with
      | [], [] ->
	  (match trot bad_lfg with
	  |`OK _ ->
	      `OK (sub_id, var_num, l_stamp)
	  |`Error er -> `Error er)
      | (f, g, l)::r, _ ->
	  if snd f == snd g then 
	    step var_num l_stamp r lgt bad_lfg bad_lgt
	  else if is_bad_lfg f g then
	    step var_num l_stamp r lgt ((f, g, l)::bad_lfg) bad_lgt
	  else 
	    let unires = unistep_lfg var_num l_stamp f g l in
	    if !Tools.debugging then 
	      (prerr_endline "\nsignature jump lfg..."; 
	       flush stderr) else ();
	    mangle unires r lgt bad_lfg bad_lgt
      | _, (f, g, l)::r ->
	  if snd f == snd g then 
	    step var_num l_stamp lfg r bad_lfg bad_lgt
	  else if is_bad_lgt f g then
	    step var_num l_stamp lfg r bad_lfg ((f, g, l)::bad_lgt)
	  else 
	    let unires = unistep_lgt var_num l_stamp f g l in
	    if !Tools.debugging then 
	      (prerr_endline "\nsignature jump lgt..."; 
	       flush stderr) else ();
	    mangle unires lfg r bad_lfg bad_lgt)
    and mangle unires lfg lgt bad_lfg bad_lgt =
      if !Tools.debugging then 
	(prerr_endline "\nsignature mangling..."; 
	 flush stderr) else ();
      (match unires with
      |`OK (lfg', lgt', s, var_num, l_stamp) ->
	  if s = sub_id then
	    (if !Tools.debugging then 
	      (prerr_endline "\nsignature shortcut..."; 
	       flush stderr) else ();
	    step var_num l_stamp (lfg' @ lfg) (lgt' @ lgt) bad_lfg bad_lgt)
	  else
	    let slfg = mapl (fun (f, g, l) -> 
	      (sub_app_f s f, sub_app_f s g, l)) lfg in
(*	    let slgt = if lgt = [] then bad_lgt else bad_lgt @ lgt in *)
	    let slgt = if bad_lgt = [] then lgt else lgt @ bad_lgt in
	    let slgt = mapl (fun (f, g, l) -> 
	      (sub_app_f s f, sub_app_f s g, l)) slgt in
	    let bad_lfg = mapl (fun (f, g, l) -> 
	      (sub_app_f s f, sub_app_f s g, l)) bad_lfg in
	    let all_lfg = lfg' @ slfg in
	    let all_lgt = lgt' @ slgt in
	    (match step var_num l_stamp all_lfg all_lgt bad_lfg [] with
	    |`OK (s', var_num, l_stamp) ->
		`OK (sub_comp s s', var_num, l_stamp)
	    |`Error er -> `Error er)
      |`Error er -> `Error er)
  end

module UnifyIDule = UnifyIDule' (IdIndex) (IListBasic) (IList) (Location)
    (ErrorRepLib) (IDule) (EqIDule) (OccursIDule) (SubISign) (PpIDule)

