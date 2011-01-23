(* Copyright (C) 2003--2006 Mikolaj Konarski
 *
 * This file is part of the Dule compiler.
 * The Dule compiler is released under the GNU General Public License (GPL).
 * Please see the file Dule-LICENSE for license information.
 *
 * $Id: mod_front.ml,v 1.77 2007-08-25 13:21:30 mikon Exp $
 *) 

open Core_front open Mod_middle open Error_rep
open Mod_back open Middle_middle open Tools

module type SubIDule =
  sig
    module IdIndex : IdIndex
    module IList : IList 
    with type Index.t = IdIndex.t
    module IDule : IDule
    with module IdIndex = IdIndex 
    with module IList = IList
    module SubISign : sig type subst end

    val sub_finish_m : SubISign.subst -> IDule.dule -> IDule.dule
  end

module SubIDule'
    (IdIndex : IdIndex)
    (IList : IList 
    with type Index.t = IdIndex.t)
    (IDule : IDule
    with module IdIndex = IdIndex 
    with module IList = IList)
    (TypeISign : TypeISign
    with module IdIndex = IdIndex 
    with module IList = IList
    with module VarStamp = IDule.VarStamp
    with module IDule = IDule) 
    (SubISign : Substitution
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Type.t = IDule.sign)
    : (SubIDule
    with module IdIndex = IdIndex
    with module IList = IList
    with module IDule = IDule 
    with module SubISign = SubISign) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module IDule = IDule
    module SubISign = SubISign

    let sub_finish_m sub m = 
      TypeISign.sub_app_m
	(SubISign.sub_app_f sub) (SubISign.sub_app_lf sub) m
  end

module SubIDule = SubIDule' (IdIndex) (IList) (IDule) (TypeISign) (SubISign)


module type BDule = (* bare module language *)
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module BCore : BCore with module IdIndex = IdIndex with module IList = IList
    module Location : Location
    type sp = Location.t * sp'
    and sp' =
      | S_Aa of sp IList.t
      | S_Cc of sp IList.t
      | S_Ee of sp IList.t * sp
      |	S_Bb of bb_item IList.t
      | S_Ii of string * dule * sp
      | S_Ww of string * dule * sp
      | S_Rr of string * sp IList.t * IdIndex.t
      | S_Mm of string * sp
      | S_Ll of IdIndex.t
    and bb_item =
      |	Bb_type
      | Bb_value of BCore.typ
    and dule = Location.t * dule'	  
    and dule' =
      | M_Id of sp IList.t
      | M_Comp of dule * dule
      | M_Pr of IdIndex.t
      | M_Accord of dule IList.t
      | M_Concord of dule IList.t
      | M_Spec of dule * sp
      |	M_Base of base_item IList.t
      | M_Inst of string * dule * dule
      | M_With of string * dule * dule
      | M_Trim of dule * sp
      | M_Link of sp IList.t * dule IList.t * dule IList.t
      | M_Ind of dule IList.t
      | M_CoInd of dule IList.t
      | M_Memo of string * dule
      | M_Memo_Lib of string * dule
      | M_Finish of dule
      | M_Lookup of IdIndex.t
    and base_item = 
      |	Base_type of BCore.typ
      | Base_value of BCore.valu

    val term_sp : sp -> sp'
    val loc_sp : sp -> Location.t
    val term_dule : dule -> dule'
    val loc_dule : dule -> Location.t
  end

module BDule' 
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (BCore : BCore with module IdIndex = IdIndex with module IList = IList)
    (Location : Location)
    : (BDule 
    with module IdIndex = IdIndex
    with module IList = IList
    with module BCore = BCore
    with module Location = Location) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module BCore = BCore
    module Location = Location
    type sp = Location.t * sp'
    and sp' =
      | S_Aa of sp IList.t
      | S_Cc of sp IList.t
      | S_Ee of sp IList.t * sp
      |	S_Bb of bb_item IList.t
      | S_Ii of string * dule * sp
      | S_Ww of string * dule * sp
      | S_Rr of string * sp IList.t * IdIndex.t
      | S_Mm of string * sp
      | S_Ll of IdIndex.t
    and bb_item =
      |	Bb_type
      | Bb_value of BCore.typ
    and dule = Location.t * dule'	  
    and dule' =
      | M_Id of sp IList.t
      | M_Comp of dule * dule
      | M_Pr of IdIndex.t
      | M_Accord of dule IList.t
      | M_Concord of dule IList.t
      | M_Spec of dule * sp
      |	M_Base of base_item IList.t
      | M_Inst of string * dule * dule
      | M_With of string * dule * dule
      | M_Trim of dule * sp
      | M_Link of sp IList.t * dule IList.t * dule IList.t
      | M_Ind of dule IList.t
      | M_CoInd of dule IList.t
      | M_Memo of string * dule
      | M_Memo_Lib of string * dule
      | M_Finish of dule
      | M_Lookup of IdIndex.t
    and base_item = 
      |	Base_type of BCore.typ
      | Base_value of BCore.valu

    let term_sp (_, p) = p
    let loc_sp (l, _) = l
    let term_dule (_, m) = m
    let loc_dule (l, _) = l
  end

module BDule = BDule' (IdIndex) (IList) (BCore) (Location)


module type ElabBDule = 
  sig
    module IdIndex : IdIndex
    module IListBasic : IListBasic with type Index.t = IdIndex.t
    module IList : IList with type Index.t = IdIndex.t
    module Location : Location
    module ErrorRepLib : ErrorRepLib
    with module Location = Location
    module BCore : BCore
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location
    module IDule : IDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Location = Location
    with module BCore = BCore
    module BDule : BDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Location = Location
    with module BCore = BCore
    val elab_dule : BDule.dule -> 
      (IDule.sign IList.t * IDule.sign) IList.t -> 
	(IDule.sign IList.t * IDule.sign * IDule.dule) IList.t ->
	[`OK of IDule.dule
        |`Error of ErrorRepLib.error]
    val elab_prelude : BDule.dule -> 
      [`OK of IDule.dule * (IDule.sign IList.t * IDule.sign) IList.t 
	  * (IDule.sign IList.t * IDule.sign * IDule.dule) IList.t
      |`Error of ErrorRepLib.error]
(*
    val el_dule : IDule.VarStamp.t -> IListBasic.IListStamp.t -> BDule.dule ->
      (IDule.sign IList.t * IDule.sign) IList.t ->
	(IDule.sign IList.t * IDule.sign * IDule.dule) IList.t ->
        [`OK of IDule.sign IList.t * IDule.sign * IDule.dule
	    * (IDule.sign * IDule.sign * Location.t) list 
	    * (IDule.sign * IDule.sign * Location.t) list 
	    * IDule.VarStamp.t * IListBasic.IListStamp.t 
        |`Error of ErrorRepLib.error]
    val el_sp :IDule.VarStamp.t -> IListBasic.IListStamp.t ->  BDule.sp ->
      (IDule.sign IList.t * IDule.sign) IList.t ->
	(IDule.sign IList.t * IDule.sign * IDule.dule) IList.t ->
        [`OK of IDule.sign IList.t * IDule.sign
	    * (IDule.sign * IDule.sign * Location.t) list
	    * (IDule.sign * IDule.sign * Location.t) list
  	    * IDule.VarStamp.t * IListBasic.IListStamp.t 
        |`Error of ErrorRepLib.error]
*)
  end

module ElabBDule' 
    (IdIndex : IdIndex)
    (IListBasic : IListBasic with type Index.t = IdIndex.t)
    (IList : IList 
    with type Index.t = IdIndex.t
    with type 'a IListBasic.t = 'a IListBasic.t)
    (Location : Location)
    (ErrorRepLib : ErrorRepLib
    with module Location = Location)
    (BCore : BCore 
    with module IdIndex = IdIndex
    with module IList = IList 
    with module Location = Location)
    (IDule : IDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Location = Location
    with module BCore = BCore)
    (SubISign : Substitution
    with module IdIndex = IdIndex 
    with module IListStamp = IListBasic.IListStamp
    with module IListBasic = IListBasic
    with module IList = IList
    with module VarStamp = IDule.VarStamp
    with type Type.t = IDule.sign)
    (UnifyIDule : UnifyIDule
    with module IListBasic = IListBasic
    with module Location = Location
    with module IDule = IDule
    with module ErrorRepLib = ErrorRepLib
    with type SubISign.subst = SubISign.subst)
    (BDule : BDule 
    with module IdIndex = IdIndex
    with module IList = IList 
    with module Location = Location
    with module BCore = BCore)
    (SubIDule : SubIDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with module IDule = IDule
    with module SubISign = SubISign)
    (CacheISign : Cache
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with type Value.t = IDule.sign IList.t * IDule.sign)
    (CacheIDule : Cache
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with type Value.t = IDule.sign IList.t * IDule.sign * IDule.dule)
    (ElabIDule : ElabIDule
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with type Cat.t = IDule.Cat.t
    with type Funct.t = IDule.Funct.t
    with type Trans.t = IDule.Trans.t 
    with module Sign = IDule.Sign
    with module Dule = IDule.Dule
    with module IDule = IDule)
    : (ElabBDule
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with module BCore = BCore
    with module IDule = IDule
    with module BDule = BDule) =
  struct
    module IListBasic = IListBasic
    module IList = IList
    module Location = Location
    module ErrorRepLib = ErrorRepLib
    module BCore = BCore
    module IDule = IDule
    module BDule = BDule
    module IdIndex' = IdIndex
    open IDule
    open ErrorRepLib
    module IdIndex = IdIndex'

    let mem_p = CacheISign.create () (* FIXME: memory leak! *)
    let mem_m = CacheIDule.create () (* cache of not fully typed modules *)
    let mem_l = CacheIDule.create ()

    let fresh_sig_var var_num l = (* where --- *)
      ((l, S_Var var_num), IDule.VarStamp.inc var_num)

    let freshen_ilist = UnifyIDule.freshen_ilist (* --- to put them? *)

    let equal_lfg ls lr l = [((l, S_Pp ls), (l, S_Pp lr), l)]

    let greater_lgt ls lr l =
      if IList.is_nil lr && IListBasic.is_rigid lr then [] (* for speed *)
      else [((l, S_Pp ls), (l, S_Pp lr), l)]

    let rec strip s = (loc_sign s, strip' s)
    and strip' s =
      match term_sign s with
      | S_Pp ls -> S_Pp (IList.vmap strip ls)
      | S_Bb (r, lc, lf) -> 
	  S_Bb ((loc_sign s, S_Pp IList.nil), lc, IList.nil)
      | SC_Bb (r, lc, lf) -> 
	  SC_Bb ((loc_sign s, S_Pp IList.nil), lc, IList.nil)
      | S_Ww (m1, s2) -> strip' s2
      | S_Mm (n, s) -> S_Mm (n ^ "@Stripped", strip s)
      | S_Var _ -> assert false

    let stripMm n (i, s) = 
      let nis = n ^ "@" ^ IdIndex.t2s i ^ "@Stripped" in
      (loc_sign s, S_Mm (nis, strip s))

    let rec el_sp var_num l_stamp p pe' me' = 
      let l = BDule.loc_sp p in
      match BDule.term_sp p with
      | BDule.S_Aa lp ->
	  (match el_sp_ilist var_num l_stamp lp pe' me' with
	  |`OK result -> 
	      `OK (s_AA result l)
	  |`Error er -> `Error er)
      | BDule.S_Cc lp ->
	  (match el_sp_ilist var_num l_stamp lp pe' me' with
	  |`OK result -> 
	      `OK (s_CC result l)
	  |`Error er -> `Error er)
      | BDule.S_Ee (lp, p) ->
	  (match el_sp_ilist var_num l_stamp lp pe' me' with
	  |`OK (llr1, ls1, lfg1, lgt1, var_num, l_stamp) ->
	      let (lrs, l_stamp) = freshen_ilist l_stamp ls1 l in
	      let lgt2 = fold_arg lrs llr1 l in
	      (match el_sp var_num l_stamp p pe' me' with
	      |`OK (lr3, s3, lfg3, lgt3, var_num, l_stamp) ->
		  let lfg4 = equal_lfg lr3 lrs l in
		  `OK (lr3, s3, 
		       lfg1 @ lfg3 @ lfg4, lgt1 @ lgt2 @ lgt3,
		       var_num, l_stamp)
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      |	BDule.S_Bb li ->
	  let (lr, l_stamp) = freshen_ilist l_stamp IList.nil l in
          let (lc, lf) = el_bb_ilist li in 
	  `OK (lr, (l, SC_Bb ((l, S_Pp lr), lc, lf)),
	       [], [], var_num, l_stamp)
      | BDule.S_Ii (nw, m1, p2) ->
	  (match el_dule var_num l_stamp m1 pe' me' with
	  |`OK (lr1, s1, m1, lfg1, lgt1, var_num, l_stamp) ->
	      (match el_sp var_num l_stamp p2 pe' me' with
	      |`OK (lr2, s2, lfg2, lgt2, var_num, l_stamp) ->
		  let r2 = (loc_sign s2, S_Pp lr2) in
		  let lfg3 = [(s1, r2, l)] in
		  let m1 = (loc_dule m1, M_Memo (nw, m1)) in
		  `OK (lr1, (l, S_Ww (m1, s2)), 
		       lfg1 @ lfg2 @ lfg3, lgt1 @ lgt2, 
		       var_num, l_stamp)
 	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | BDule.S_Ww (nw, m1, p2) ->
	  (match el_dule var_num l_stamp m1 pe' me' with
	  |`OK (lr1, s1, m1, lfg1, lgt1, var_num, l_stamp) ->
	      (match el_sp var_num l_stamp p2 pe' me' with
	      |`OK (lr2, s2, lfg2, lgt2, var_num, l_stamp) ->
		  let r2 = (loc_sign s2, S_Pp lr2) in
		  let m1 = (l, M_Trim (m1, r2)) in
		  let m1 = (l, M_Memo (nw, m1)) in
		  `OK (lr1, (l, S_Ww (m1, s2)), 
		       lfg1 @ lfg2, lgt1 @ lgt2, 
		       var_num, l_stamp)
 	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | BDule.S_Rr (n, lp, i) -> 
(* this is similar to S_Ee (include "optimized away" below to run this):
	  (match el_sp_ilist var_num l_stamp lp pe' me' with
	  |`OK (llr1, ls1, lfg1, lgt1, var_num, l_stamp) ->
	      let ls_stripped = IList.bmap (stripMm n) ls1 in
	      let (lr2, l_stamp) = freshen_ilist l_stamp ls_stripped l in
	      let lfg3 =
		IList.bfold []
		  (fun (i, s3) lfg1 -> 
		    let lr3 = IList.find i llr1 in
		    let l = loc_sign s3 in
		    let lfg2 = equal_lfg lr3 lr2 l in
		    lfg1 @ lfg2) ls1
	      in
	      let lr = IListBasic.move_flexible lr2 ls1 in
	      let s = IList.find i ls1 in
	      `OK (lr, s, 
		   lfg1 @ lfg3, lgt1, var_num, l_stamp)
	  |`Error er -> `Error er) *)
(* optimized version, we assume result should be Mm: *)
	  let ni = n ^ "@" ^ IdIndex.t2s i in
	  (match CacheISign.find_mem_ok mem_p ni with
	  |`OK (lr, s) -> 
	      let (lr, l_stamp) = freshen_ilist l_stamp lr l in
	      `OK (lr, s, 
		   [], [], var_num, l_stamp)
	  |`Error er ->
	  let orig_var_num = var_num in
	  let orig_l_stamp = l_stamp in
	  (match 
	    el_sp_ilist 
	      IDule.VarStamp.first IListBasic.IListStamp.first 
	      lp  pe' me'
	  with
	  |`OK (llr1, ls1, lfg1, lgt1, var_num, l_stamp) ->
	      let ls_stripped = IList.bmap (stripMm n) ls1 in
	      let (lr2, l_stamp) = freshen_ilist l_stamp ls_stripped l in
	      let lfg3 =
		IList.bfold []
		  (fun (i, s3) lfg1 -> 
		    let lr3 = IList.find i llr1 in
		    let l = loc_sign s3 in
		    let lfg2 = equal_lfg lr3 lr2 l in
		    lfg1 @ lfg2) ls1
	      in
	      let lfg = lfg1 @ lfg3 in
	      let lgt = lgt1 in
	      (match UnifyIDule.unify var_num l_stamp lfg lgt with
	      |`OK (sub, _, _) ->
		  let sub = SubISign.finish_sub var_num l_stamp sub in
		  let lr2 = SubISign.sub_finish_lf sub lr2 in
		  let ls1 = SubISign.sub_finish_lf sub ls1 in
		  let ls1 = IList.bmap (fun (i, s) ->
		    let ni = n ^ "@" ^ IdIndex.t2s i in
		    (loc_sign s, S_Mm (ni, s))) ls1
		  in
		  let lr = IList.(@@) ls1 (IList.subtract lr2 ls1) in
		  let _ = IList.bfold () (fun (i, s) _ -> 
		    let ni = n ^ "@" ^ IdIndex.t2s i in
		    CacheISign.add_mem mem_p ni (lr, s)) ls1
		  in
		  let (lr, l_stamp) = freshen_ilist orig_l_stamp lr l in
		  let s = IList.find i ls1 in
		  `OK (lr, s, 
		       [], [], orig_var_num, l_stamp)
	      |`Error er -> `Error er)
	  |`Error er -> `Error er))
      | BDule.S_Mm (n, p) ->
	  (match CacheISign.find_mem_ok mem_p n with
	  |`OK (lr, s) -> 
	      let (lr, l_stamp) = freshen_ilist l_stamp lr l in
	      `OK (lr, s, 
		   [], [], var_num, l_stamp)
	  |`Error er ->
	      let orig_var_num = var_num in
	      let orig_l_stamp = l_stamp in
	      (match 
		el_sp 
		  IDule.VarStamp.first IListBasic.IListStamp.first 
		  p pe' me'
	      with
	      |`OK (lr, s, lfg, lgt, var_num, l_stamp) ->
		  (match UnifyIDule.unify var_num l_stamp lfg lgt with
		  |`OK (sub, _, _) ->
		      let sub = SubISign.finish_sub var_num l_stamp sub in
		      let lr = SubISign.sub_finish_lf sub lr in
		      let s = SubISign.sub_finish_f sub s in
		      let s = (l, S_Mm (n, s)) in
		      let _ = CacheISign.add_mem mem_p n (lr, s) in
		      let (lr, l_stamp) = freshen_ilist orig_l_stamp lr l in
		      `OK (lr, s, 
			   [], [], orig_var_num, l_stamp)
		  |`Error er -> `Error er)
	      |`Error er -> `Error er))
      | BDule.S_Ll i ->
	  (match IList.find_ok i pe' with
	  |`OK (lr, s) ->
 	      let (lr, l_stamp) = freshen_ilist l_stamp lr l in
	      `OK (lr, s, 
		   [], [], var_num, l_stamp)
	  |`Error er -> 
	      `Error
		(modFrontError#instance 
		   [Loc l; 
		    Msg (IdIndex.t2string i 
			 ^ " not found among the prelude specifications")]))

    and el_sp_ilist var_num l_stamp lp pe' me' =
      IList.bfold1ok (IList.nil, IList.nil, [], [], var_num, l_stamp)
	(fun (i, p) (llr1, ls1, lfg1, lgt1, var_num, l_stamp) -> 
	  (match el_sp var_num l_stamp p pe' me' with
	  |`OK (lr2, s2, lfg2, lgt2, var_num, l_stamp) -> 
	      let llr = IList.cons (i, lr2) llr1 in
	      let ls = IList.cons (i, s2) ls1 in
	      `OK (llr, ls,
		   lfg1 @ lfg2, lgt1 @ lgt2, var_num, l_stamp)
	  |`Error er -> `Error er)) lp

    and fold_arg lrs llr l =
      let lgt = 
	IList.bfold []
	  (fun (i, lr) lgt1 -> 
(*	    let l = loc_sign (IList.find i ls) in --- wrong: result location *)
	    let lgt2 = greater_lgt lrs lr l in
	    lgt1 @ lgt2) llr
      in lgt

    and fold_aa l_stamp llr l =
      let (lr2, l_stamp) = freshen_ilist l_stamp IList.nil l in
      let lgt = fold_arg lr2 llr l in
      (lr2, lgt, l_stamp)

    and s_AA (llr1, ls1, lfg1, lgt1, var_num, l_stamp) l =
      let (lr2, lgt2, l_stamp) = fold_aa l_stamp llr1 l in
      (lr2, (l, S_Pp ls1), lfg1, lgt1 @ lgt2, var_num, l_stamp)

    and s_CC (llr1, ls1, lfg1, lgt1, var_num, l_stamp) l =
      let (lr2, lgt2, l_stamp) = fold_aa l_stamp llr1 l in
      let (lr3, l_stamp) = freshen_ilist l_stamp ls1 l in
      let lgt3 = greater_lgt lr3 lr2 l in (* to unify ls1 and llr1 *)
      let lrs = IListBasic.move_flexible lr2 ls1 in (* lr2 + ls1 *)
      (lr2, (l, S_Pp lrs), lfg1, lgt1 @ lgt2 @ lgt3, var_num, l_stamp)

    and el_bb_ilist li =
      IList.bfold (IList.nil, IList.nil)
	(fun (i, item) (lc, lf) ->
	  match item with 
	  | BDule.Bb_type -> 
              (IList.cons (i, ()) lc, lf)
	  | BDule.Bb_value f -> 
	      (lc, IList.cons (i, f) lf)) li

    and el_dule var_num l_stamp m pe' me' = 
      let l = BDule.loc_dule m in
      match BDule.term_dule m  with 
      | BDule.M_Id lp -> (* similar to S_Ee *)
	  (match el_sp_ilist var_num l_stamp lp pe' me' with
	  |`OK (llr1, ls1, lfg1, lgt1, var_num, l_stamp) ->
	      let (lrs, l_stamp) = freshen_ilist l_stamp ls1 l in
	      let lgt2 = fold_arg lrs llr1 l in
	      let r = (l, S_Pp lrs) in
	      `OK (lrs, r, (l, M_Id r), 
		   lfg1, lgt1 @ lgt2, var_num, l_stamp)
	  |`Error er -> `Error er)
      | BDule.M_Comp (m1, m2) ->
	  (match el_dule var_num l_stamp m1 pe' me' with
	  |`OK (lr1, s1, m1, lfg1, lgt1, var_num, l_stamp) ->
	      (match el_dule var_num l_stamp m2 pe' me' with
	      |	`OK (lr2, s2, m2, lfg2, lgt2, var_num, l_stamp) ->
		  let r2 = (loc_dule m2, S_Pp lr2) in
		  let lfg3 = [(s1, r2, l)] in
		  `OK (lr1, s2, (l, M_Comp (m1, m2)), 
                       lfg1 @ lfg2 @ lfg3, lgt1 @ lgt2, var_num, l_stamp)
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | BDule.M_Pr i ->
	  let (s, var_num) = fresh_sig_var var_num l in
	  let (lr, l_stamp) = freshen_ilist l_stamp
	      (IList.cons (i, s) IList.nil) l in
	  `OK (lr, s, (l, M_Pr (lr, i)), 
	       [], [], var_num, l_stamp)
      | BDule.M_Accord lm ->
	  (match el_dule_ilist var_num l_stamp lm pe' me' with
	  |`OK (llr, ls, lm, lfg, lgt, var_num, l_stamp) -> 
	      let (lr1, s1, lfg1, lgt1, var_num, l_stamp) =
		s_AA (llr, ls, lfg, lgt, var_num, l_stamp) l
	      in
	      `OK (lr1, s1, (l, M_Accord (lr1, lm)), 
		   lfg1, lgt1, var_num, l_stamp)
	  |`Error er -> `Error er)
      | BDule.M_Concord lm ->
	  (match el_dule_ilist var_num l_stamp lm pe' me' with
	  |`OK (llr, ls, lm, lfg, lgt, var_num, l_stamp) -> 
	      let (lr1, s1, lfg1, lgt1, var_num, l_stamp) =
		s_CC (llr, ls, lfg, lgt, var_num, l_stamp) l
	      in
	      `OK (lr1, s1, (l, M_Concord (lr1, lm)), 
		   lfg1, lgt1, var_num, l_stamp)
	  |`Error er -> `Error er)
      |	BDule.M_Spec (m1, p2) ->
	  (match el_dule var_num l_stamp m1 pe' me' with
	  |`OK (lr1, s1, m1, lfg1, lgt1, var_num, l_stamp) ->
	      (match el_sp var_num l_stamp p2 pe' me' with
	      |`OK (lr2, s2, lfg2, lgt2, var_num, l_stamp) ->
		  let lfg3 = [(s1, s2, l)] in
		  let lgt3 = greater_lgt lr1 lr2 l in
		  `OK (lr1, s2, m1, (* s2 is most probably cached *)
		       lfg1 @ lfg2 @ lfg3, lgt1 @ lgt2 @ lgt3,
		       var_num, l_stamp) 
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      |	BDule.M_Base li ->
	  let (lr, l_stamp) = freshen_ilist l_stamp IList.nil l in
	  let (s, var_num) = fresh_sig_var var_num l in
	  let (lg, lt) = el_base_ilist li in 
	  let r = (l, S_Pp lr) in
	  `OK (lr, s, (l, MC_Base (r, s, lg, lt)), [], [], var_num, l_stamp)
      | BDule.M_Inst (nw, m1, m2) ->
	  (match el_dule var_num l_stamp m1 pe' me' with
	  |`OK (lr1, s1, m1, lfg1, lgt1, var_num, l_stamp) ->
	      (match el_dule var_num l_stamp m2 pe' me' with
	      |	`OK (lr2, s2, m2, lfg2, lgt2, var_num, l_stamp) ->
		  let r2 = (loc_dule m2, S_Pp lr2) in
		  let lfg3 = [(s1, r2, l)] in
		  let m1 = (loc_dule m1, M_Memo (nw, m1)) in
		  `OK (lr1, (l, S_Ww (m1, s2)), (l, M_Inst (m1, m2)),
		       lfg1 @ lfg2 @ lfg3, lgt1 @ lgt2, 
		       var_num, l_stamp)
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | BDule.M_With (nw, m1, m2) ->
	  (match el_dule var_num l_stamp m1 pe' me' with
	  |`OK (lr1, s1, m1, lfg1, lgt1, var_num, l_stamp) ->
	      (match el_dule var_num l_stamp m2 pe' me' with
	      |	`OK (lr2, s2, m2, lfg2, lgt2, var_num, l_stamp) ->
		  let r2 = (loc_dule m2, S_Pp lr2) in
		  let m1 = (l, M_Trim (m1, r2)) in
		  let m1 = (l, M_Memo (nw, m1)) in
		  `OK (lr1, (l, S_Ww (m1, s2)), (l, M_Inst (m1, m2)),
		       lfg1 @ lfg2, lgt1 @ lgt2, 
		       var_num, l_stamp)
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | BDule.M_Trim (m1, p2) ->
	  (match el_dule var_num l_stamp m1 pe' me' with
	  |`OK (lr1, s1, m1, lfg1, lgt1, var_num, l_stamp) ->
	      (match el_sp var_num l_stamp p2 pe' me' with
	      |`OK (lr2, s2, lfg2, lgt2, var_num, l_stamp) ->
		  let lgt3 = greater_lgt lr1 lr2 l in
		    `OK (lr1, s2, (l, M_Trim (m1, s2)), 
			 lfg1 @ lfg2, lgt1 @ lgt2 @ lgt3, 
			 var_num, l_stamp)
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | BDule.M_Link (pe, me, lm) -> (* no topological sorting, yet! *)
	  (match el_link var_num l_stamp l pe me lm pe' me' with
	  |`OK (lr, s, m, lfg, lgt, var_num, l_stamp, pe_new, me_new) ->
	      `OK (lr, s, m, lfg, lgt, var_num, l_stamp)
	  |`Error er -> `Error er)
      | BDule.M_Ind lm ->
	  (match el_dule_ilist var_num l_stamp lm pe' me' with
	  |`OK (llr, ls, lm, lfg, lgt, var_num, l_stamp) -> 
	      let (lr2, s1, lfg1, lgt1, var_num, l_stamp) =
		s_AA_minus_ls (llr, ls, lfg, lgt, var_num, l_stamp) l
	      in
	      `OK (lr2, s1, (l, M_Ind (lr2, lm)), 
		   lfg1, lgt1, var_num, l_stamp)
	  |`Error er -> `Error er)
      | BDule.M_CoInd lm ->
	  (match el_dule_ilist var_num l_stamp lm pe' me' with
	  |`OK (llr, ls, lm, lfg, lgt, var_num, l_stamp) -> 
	      let (lr2, s1, lfg1, lgt1, var_num, l_stamp) =
		s_AA_minus_ls (llr, ls, lfg, lgt, var_num, l_stamp) l
	      in
	      `OK (lr2, s1, (l, M_CoInd (lr2, lm)), 
		   lfg1, lgt1, var_num, l_stamp)
	  |`Error er -> `Error er)
      | BDule.M_Memo (n, m) -> (* for ind and coind, not finished *)
	  (match CacheIDule.find_mem_ok mem_m n with
	  |`OK (lr, s, m) -> 
	      `OK (lr, s, m, 
		   [], [], (* [lfg] and [lgt] already there *)
		   var_num, l_stamp)
	  |`Error er ->
	      (match el_dule var_num l_stamp m pe' me' with
	      |`OK (lr, s, m, lfg, lgt, var_num, l_stamp) ->
		  let m = (l, M_Memo (n, m)) in
		  let _ = CacheIDule.add_mem mem_m n (lr, s, m) in
		  `OK (lr, s, m,
		       lfg, lgt, (* first inductive module contributes these *)
                       var_num, l_stamp)
	      |`Error er -> `Error er))
      | BDule.M_Memo_Lib (n, m) -> (* will always be finished *)
	  (match CacheIDule.find_mem_ok mem_l n with
	  |`OK (lr, s, m) -> 
	      `OK (lr, s, m, 
		   [], [], (* no [lfg] and [lgt] for finished modules *)
                   var_num, l_stamp)
	  |`Error er ->
	      (match el_dule var_num l_stamp m pe' me' with
	      |`OK (lr, s, m, lfg, lgt, var_num, l_stamp) ->
		  let m = (l, M_Memo (n, m)) in
		  let _ = CacheIDule.add_mem mem_l n (lr, s, m) in
		  `OK (lr, s, m,
		       lfg, lgt, (assert (lfg = [] && lgt = []);
                       var_num), l_stamp)
	      |`Error er -> `Error er))
      | BDule.M_Finish m ->
	  let orig_var_num = var_num in
	  let orig_l_stamp = l_stamp in
	  (match 
	    el_dule IDule.VarStamp.first IListBasic.IListStamp.first m pe' me' 
	  with
	  |`OK (lr, s, m, lfg, lgt, var_num, l_stamp) ->
	      (match
		el_finish (lr, s, m, lfg, lgt, var_num, l_stamp) l
	      with
	      |`OK (lr, s, m) ->
		  `OK (lr, s, m, [], [], orig_var_num, orig_l_stamp)
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | BDule.M_Lookup i ->
	  (match IList.find_ok i me' with
	  |`OK (lr, s, m) -> 
	      `OK (lr, s, m, 
		   [], [], var_num, l_stamp)
	  |`Error er -> 
	      `Error
		(modFrontError#instance 
		   [Loc l; 
		    Msg (IdIndex.t2string i 
			 ^ " not found among the prelude libraries")]))

    and el_dule_ilist var_num l_stamp lm pe' me' =
      IList.bfold1ok 
	(IList.nil, IList.nil, IList.nil, [], [], var_num, l_stamp)
	(fun (i, m) (llr1, ls1, lm1, lfg1, lgt1, var_num, l_stamp) -> 
	  (match el_dule var_num l_stamp m pe' me' with
	  |`OK (lr2, s2, m2, lfg2, lgt2, var_num, l_stamp) -> 
	      let llr = IList.cons (i, lr2) llr1 in
	      let ls = IList.cons (i, s2) ls1 in
	      let lm = IList.cons (i, m2) lm1 in
	      `OK (llr, ls, lm, 
		   lfg1 @ lfg2, lgt1 @ lgt2, var_num, l_stamp)
	  |`Error er -> `Error er)) lm

(* why is this so slow? (4m30.801s vs 0m16.593s for test-short)
    and s_AA_minus_ls (llr1, ls1, lfg1, lgt1, var_num, l_stamp) l =
      let (lrs, l_stamp) = freshen_ilist l_stamp ls1 l in
      let lgt2 = fold_arg lrs llr1 l in
      let lr2 = IListBasic.move_flexible lrs IList.nil in
      (lr2, (l, S_Pp ls1), lfg1, lgt1 @ lgt2, var_num, l_stamp)
*)
    and s_AA_minus_ls (llr1, ls1, lfg1, lgt1, var_num, l_stamp) l =
      let (lr1, s1, lfg1, lgt1, var_num, l_stamp) =
	s_AA (llr1, ls1, lfg1, lgt1, var_num, l_stamp) l
      in
      let (lrs, l_stamp) = freshen_ilist l_stamp ls1 l in
      let lgt2 = greater_lgt lrs lr1 l in
      let lr2 = IListBasic.move_flexible lrs IList.nil in
      (lr2, s1, lfg1, lgt1 @ lgt2, var_num, l_stamp)

    and el_base_ilist li =
      IList.bfold (IList.nil, IList.nil)
	(fun (i, item) (lf, lt) ->
	  match item with 
	  | BDule.Base_type f -> 
	      (IList.cons (i, f) lf, lt)
	  | BDule.Base_value t -> 
	      (lf, IList.cons (i, t) lt)) li

    and el_link var_num l_stamp l pe me lm pe' me' =
      (* all in pe are S_Mm and in me are M_Finish, 
         but still l_stamp can change! *)
      let pe_ok = 
	IList.bfold1ok (IList.nil, var_num, l_stamp)
	  (fun (i, p) (pe1, var_num, l_stamp) -> 
	    (match el_sp var_num l_stamp p pe' me' with
	    |`OK (lr2, s2, _, _, var_num, l_stamp) -> 
		(match ElabIDule.el_sign s2 with
		|`OK _ -> (* only to verify, remains in cache *)
		    (match IList.vmap1ok ElabIDule.el_sign lr2 with
		    |`OK _ ->
			(* a hack: *)
			let lr2_rigid = IList.(@@) IList.nil lr2 in
			let pe_new = IList.cons (i, (lr2_rigid, s2)) pe1 in
			`OK (pe_new, var_num, l_stamp)
		    |`Error er -> `Error er)
		|`Error er -> `Error er)
	    |`Error er -> `Error er)) pe
      in
      (match pe_ok with
      |`OK (pe_new, var_num, l_stamp) ->
	  let me_ok = 
	    IList.bfold1ok (IList.nil, var_num, l_stamp)
	      (fun (i, m) (me1, var_num, l_stamp) -> 
		(match el_dule var_num l_stamp m pe' me' with
		|`OK (lr2, s2, m2, _, _, var_num, l_stamp) -> 
		    (* no [IDule] compilation of [me'] here, 
		       since each module in [me] is [Finish]
		       and so is compiled in [el_dule] above *)
		    let me_new = IList.cons (i, (lr2, s2, m2)) me1 in
		    `OK (me_new, var_num, l_stamp)
		|`Error er -> `Error er)) me
	  in
	  (match me_ok with
	  |`OK (me_new, var_num, l_stamp) -> 
	      (match el_dule_ilist var_num l_stamp lm pe' me' with
	      |`OK (llr, ls, lm, lfg, lgt, var_num, l_stamp) -> 
		  let (lr2, s1, lfg1, lgt1, var_num, l_stamp) =
		    s_AA_minus_ls 
		      (llr, ls, lfg, lgt, var_num, l_stamp) l
		  in
		  `OK (lr2, s1, (l, M_Link (lr2, lm)), 
		       lfg1, lgt1, var_num, l_stamp, pe_new, me_new)
	      |`Error er -> `Error er)
          |`Error er -> `Error er)
      |`Error er -> `Error er)

    and el_finish (lr, s, m, lfg, lgt, var_num, l_stamp) l =
      (match UnifyIDule.unify var_num l_stamp lfg lgt with
      |`OK (sub, _, _) ->
	  let sub = SubISign.finish_sub var_num l_stamp sub in
	  let lr = SubISign.sub_finish_lf sub lr in
	  let s = SubISign.sub_finish_f sub s in
	  let m = SubIDule.sub_finish_m sub m in
	  (match ElabIDule.el_dule m with
	  |`OK t -> 
	      let m = (l, M_Finish t) in
	      `OK (lr, s, m)
	  |`Error er -> `Error er)
      |`Error er -> `Error er)
	
    let elab_dule m pe' me' =
      CacheISign.clear mem_p;
      CacheIDule.clear mem_m;
      CacheIDule.clear mem_l;
     (* if [m] is not [Finish] then spec reconstruction will we be partial *)
      match 
	el_dule IDule.VarStamp.first IListBasic.IListStamp.first m pe' me'
      with
      |`OK (lr, s, m, lfg, lgt, var_num, l_stamp) ->
          (* because the outer [lfg] are ignored *)
	  `OK m
      |`Error er -> `Error er

    let elab_prelude m =
      CacheISign.clear mem_p;
      CacheIDule.clear mem_m;
      CacheIDule.clear mem_l;
      let l = BDule.loc_dule m in
      match BDule.term_dule m with 
      | BDule.M_Finish m ->
	  (match BDule.term_dule m with
	  | BDule.M_Link (pe, me, lm) ->
	      (match
		el_link 
		  IDule.VarStamp.first IListBasic.IListStamp.first l
		  pe me lm IList.nil IList.nil
	      with
	      |`OK (lr, s, m, lfg, lgt, var_num, l_stamp, pe', me') ->
		  (match
		    el_finish (lr, s, m, lfg, lgt, var_num, l_stamp) l
		  with
		  |`OK (lr, s, m) ->
		      `OK (m, pe', me')
		  |`Error er -> `Error er)
	      |`Error er -> `Error er)
	  | _ -> `Error
		(modFrontError#instance 
		   [Loc l; 
		    Msg ("The prelude is not a linking expression")]))
      | _ -> `Error
	    (modFrontError#instance 
	       [Loc l; 
		Msg ("The prelude is not isolated")])
  end 

module CacheISign = Cache' (Location) (ErrorRepLib)
    (struct type t = IDule.sign IList.t * IDule.sign end)

module CacheIDule = Cache' (Location) (ErrorRepLib)
    (struct type t = IDule.sign IList.t * IDule.sign * IDule.dule end)

module ElabBDule = ElabBDule' (IdIndex) (IListBasic) (IList) (Location)
    (ErrorRepLib) (BCore) (IDule) (SubISign) (UnifyIDule) (BDule)
    (SubIDule) (CacheISign) (CacheIDule) (ElabIDule)


module type EDule = (* environment-dependent module language *)
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module BCore : BCore with module IdIndex = IdIndex with module IList = IList
    module Location : Location
    type sp = Location.t * sp'
    and sp' =
      | S_Aa of sp IList.t
      | S_Cc of sp IList.t
      | S_Ee of sp IList.t * sp
      |	S_Bb of bb_item IList.t
      | S_Ii of dule * sp
      | S_Ww of dule * sp
      | S_Ll of IdIndex.t
    and bb_item =
      |	Bb_type
      | Bb_value of BCore.typ
    and dule = Location.t * dule'	  
    and dule' =
      | M_Id of sp IList.t
      | M_Comp of dule * dule
      | M_Pr of IdIndex.t
      | M_Accord of dule IList.t
      | M_Concord of dule IList.t
      | M_Spec of dule * sp
      |	M_Base of base_item IList.t
      | M_Inst of dule * dule
      | M_With of dule * dule
      | M_Trim of dule * sp
      | M_Link of link_item list
      | M_Load of IdIndex.t
    and base_item = 
      |	Base_type of BCore.typ
      | Base_value of BCore.valu
    and link_item =
      |	Link_Dule of IdIndex.t * dule
      |	Link_Ind_Dule of dule IList.t
      |	Link_CoInd_Dule of dule IList.t
      |	Link_Sp of IdIndex.t * sp
      |	Link_Rec_Sp of sp IList.t
      |	Link_Lib of IdIndex.t * dule
      |	Link_Ind_Lib of dule IList.t
      |	Link_CoInd_Lib of dule IList.t
    and start = Location.t * link_item list

    val term_sp : sp -> sp'
    val loc_sp : sp -> Location.t
    val term_dule : dule -> dule'
    val loc_dule : dule -> Location.t
  end

module EDule' 
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (BCore : BCore with module IdIndex = IdIndex with module IList = IList)
    (Location : Location)
    : (EDule 
    with module IdIndex = IdIndex
    with module IList = IList
    with module BCore = BCore
    with module Location = Location) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module BCore = BCore
    module Location = Location
    type sp = Location.t * sp'
    and sp' =
      | S_Aa of sp IList.t
      | S_Cc of sp IList.t
      | S_Ee of sp IList.t * sp
      |	S_Bb of bb_item IList.t
      | S_Ii of dule * sp
      | S_Ww of dule * sp
      | S_Ll of IdIndex.t
    and bb_item =
      |	Bb_type
      | Bb_value of BCore.typ
    and dule = Location.t * dule'	    
    and dule' =
      | M_Id of sp IList.t
      | M_Comp of dule * dule
      | M_Pr of IdIndex.t
      | M_Accord of dule IList.t
      | M_Concord of dule IList.t
      | M_Spec of dule * sp
      |	M_Base of base_item IList.t
      | M_Inst of dule * dule
      | M_With of dule * dule
      | M_Trim of dule * sp
      | M_Link of link_item list
      | M_Load of IdIndex.t
    and base_item = 
      |	Base_type of BCore.typ
      | Base_value of BCore.valu
    and link_item =
      |	Link_Dule of IdIndex.t * dule
      |	Link_Ind_Dule of dule IList.t
      |	Link_CoInd_Dule of dule IList.t
      |	Link_Sp of IdIndex.t * sp
      |	Link_Rec_Sp of sp IList.t
      |	Link_Lib of IdIndex.t * dule
      |	Link_Ind_Lib of dule IList.t
      |	Link_CoInd_Lib of dule IList.t
    and start = Location.t * link_item list

    let term_sp (_, p) = p
    let loc_sp (l, _) = l
    let term_dule (_, m) = m
    let loc_dule (l, _) = l
  end

module EDule = EDule' (IdIndex) (IList) (BCore) (Location)


module type ElabEDule = 
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Location : Location
    module ErrorRepLib : ErrorRepLib
    with module Location = Location
    module BCore : BCore
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location
    module BDule : BDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Location = Location
    with module BCore = BCore
    module EDule : EDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Location = Location
    with module BCore = BCore
    val allow_composition : bool ref
    val allow_overwriting : bool ref

    val elab_dule : EDule.dule -> 
      [`OK of BDule.dule|`Error of ErrorRepLib.error]
    val elab_start : EDule.start -> 
      [`OK of BDule.dule|`Error of ErrorRepLib.error]
    val el_dule : string -> BDule.sp IList.t -> BDule.dule IList.t -> 
      EDule.dule -> 
	[`OK of BDule.dule|`Error of ErrorRepLib.error]
    val el_sp : string -> BDule.sp IList.t -> BDule.dule IList.t -> 
      EDule.sp -> 
	[`OK of BDule.sp|`Error of ErrorRepLib.error]
  end

module ElabEDule' 
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Location : Location)
    (ErrorRepLib : ErrorRepLib
    with module Location = Location)
    (BCore : BCore 
    with module IdIndex = IdIndex
    with module IList = IList 
    with module Location = Location)
    (BDule : BDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Location = Location
    with module BCore = BCore)
    (EDule : EDule 
    with module IdIndex = IdIndex
    with module IList = IList 
    with module Location = Location
    with module BCore = BCore)
    : (ElabEDule
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with module BCore = BCore
    with module BDule = BDule
    with module EDule = EDule) =
  struct
    module IdIndex' = IdIndex
    module IList = IList
    module Location = Location
    module ErrorRepLib = ErrorRepLib
    module BCore = BCore
    module BDule = BDule
    module EDule = EDule
    open BDule
    open IList
    open ErrorRepLib
    module IdIndex = IdIndex'

    let allow_composition = ref false
    let allow_overwriting = ref false

    let append_unique l1 l2 =
      bfold1ok l2
	(fun ((i, (l, v)) as iv) r -> 
	  if not_in i l2 then `OK (cons iv r) 
	  else	
	    `Error
	      (modFrontError#instance 
		 [Loc l; 
		  Msg (IdIndex.t2string i ^ " is duplicated")])) l1

    let rec foldr init f l =
      match l with
      | [] -> init
      | v :: r -> f v (foldr init f r)

    let foldr1ok init f =
      foldr (`OK init)
	(fun v r -> 
	  (match r with
	  |`OK r -> f v r
	  |`Error er -> `Error er))

    let rec elab_dule m = 
      (match el_dule "" nil nil m with
      |`OK m -> `OK m
      |`Error er -> `Error er)

    and elab_start (l, li) = elab_dule (l, EDule.M_Link li)

    and el_sp_i n pe me (i, p) =
      let n = n ^ "@" ^ IdIndex.t2s i in
      el_sp n pe me p

    and el_sp n pe me p = 
      let l = EDule.loc_sp p in
      match EDule.term_sp p with
      | EDule.S_Aa lp -> 
	  (match bmap1ok (el_sp_i n pe me) lp with
	  |`OK lp -> 
	      `OK (l, S_Aa lp)
	  |`Error er -> `Error er)
      | EDule.S_Cc lp ->
	  (match bmap1ok (el_sp_i n pe me) lp with
	  |`OK lp -> 
	      `OK (l, S_Cc lp)
	  |`Error er -> `Error er)
      | EDule.S_Ee (lp, p) ->
	  (match bmap1ok (el_sp_i n pe me) lp with
	  |`OK lp -> 
	      (match el_sp (n ^ "@Ee") pe me p with
	      |`OK p -> 
		  `OK (l, S_Ee (lp, p))
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      |	EDule.S_Bb li ->
	  let li = vmap el_bb_item li in
	  `OK (l, S_Bb li)
      | EDule.S_Ii (m1, p2) ->
	  if !allow_composition then
	    (match el_dule (n ^ "@Ii1") pe me m1 with
	    |`OK m1 ->
		(match el_sp (n ^ "@Ii2") pe me p2 with
		|`OK p2 ->
		    let nw = n ^ "@Ww" in
		    `OK (l, S_Ii (nw, m1, p2))
 		|`Error er -> `Error er)
	    |`Error er -> `Error er)
	  else
	    `Error
	      (modFrontError#instance 
		 [Loc l; 
		  Msg ("Signature specialization without trimming not allowed "
		       ^ "without the \"--composition\" option")])
      | EDule.S_Ww (m1, p2) ->
	  (match el_dule (n ^ "@Ww1") pe me m1 with
	  |`OK m1 ->
	      (match el_sp (n ^ "@Ww2") pe me p2 with
	      |`OK p2 ->
		  let nw = n ^ "@Ww" in
		  `OK (l, S_Ww (nw, m1, p2))
 	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | EDule.S_Ll i ->
	  (match find_ok i pe with
	  |`OK p ->`OK p
	  |`Error er -> `OK (l, S_Ll i)) (* from other files *)

    and el_bb_item item =
      match item with 
      | EDule.Bb_type -> Bb_type
      | EDule.Bb_value f -> Bb_value f

    and el_dule_i n pe me (i, m) =
      let n = n ^ "@" ^ IdIndex.t2s i in
      el_dule n pe me m

    and el_dule n pe me m = 
      let l = EDule.loc_dule m in
      match EDule.term_dule m  with 
      | EDule.M_Id lp ->
	  if !allow_composition then
	    (match bmap1ok (el_sp_i n pe me) lp with
	    |`OK lp -> 
		`OK (l, M_Id lp)
	    |`Error er -> `Error er)
	  else
	    `Error
	      (modFrontError#instance 
		 [Loc l; 
		  Msg ("Module identity not allowed without the \"--composition\" option")])
      | EDule.M_Comp (m1, m2) ->
	  if !allow_composition then
	    (match el_dule (n ^ "@Comp1") pe me m1 with
	    |`OK m1 ->
		(match el_dule (n ^ "@Comp2") pe me m2 with
		|	`OK m2 ->
		    `OK (l, M_Comp (m1, m2))
		|`Error er -> `Error er)
	    |`Error er -> `Error er)
	  else
	    `Error
	      (modFrontError#instance 
		 [Loc l; 
		  Msg ("Module composition not allowed without the \"--composition\" option")])
      | EDule.M_Pr i -> 
	  let m = (l, M_Pr i) in
	  let i2 = AtIndex.dule2sp i in
	  (match find_ok i2 pe with
	  |`OK p ->
	      `OK (l, M_Spec (m, p))
	  |`Error er -> 
	      `OK m)
      | EDule.M_Accord lm ->
	  (match bmap1ok (el_dule_i n pe me) lm with
	  |`OK lm -> 
	      `OK (l, M_Accord lm)
	  |`Error er -> `Error er)
      | EDule.M_Concord lm ->
	  (match bmap1ok (el_dule_i n pe me) lm with
	  |`OK lm -> 
	      `OK (l, M_Concord lm)
	  |`Error er -> `Error er)
      |	EDule.M_Spec (m1, p2) ->
	  (match el_dule n pe me m1 with
	    `OK m1 ->
	      (match el_sp (n ^ "@Spec") pe me p2 with
	      |`OK p2 ->
		  `OK (l, M_Spec (m1, p2))
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      |	EDule.M_Base li -> 
	  let li = vmap el_base_item li in
	  `OK (l, M_Base li)
      | EDule.M_Inst (m1, m2) ->
	  (match el_dule (n ^ "@Inst1") pe me m1 with
	  |`OK m1 ->
	      (match el_dule (n ^ "@Inst2") pe me m2 with
	      |	`OK m2 ->
		  let nw = n ^ "@Ww" in
		  `OK (l, M_Inst (nw, m1, m2))
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | EDule.M_With (m1, m2) ->
	  (match el_dule (n ^ "@With1") pe me m1 with
	  |`OK m1 ->
	      (match el_dule (n ^ "@With2") pe me m2 with
	      |	`OK m2 ->
		  let nw = n ^ "@Ww" in
		  `OK (l, M_With (nw, m1, m2))
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | EDule.M_Trim (m1, p2) ->
	  (match el_dule n pe me m1 with
	  |`OK m1 ->
	      (match el_sp (n ^ "@Trim") pe me p2 with
	      |`OK p2 ->
		  `OK (l, M_Trim (m1, p2))		  
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | EDule.M_Link li ->
	  (match el_link_ilist n pe me li with
	  |`OK (_, _, pe1, me1, mer) ->
	      `OK (l, M_Finish (l, M_Link (pe1, me1, mer)))
	  |`Error er -> `Error er) 
      | EDule.M_Load i ->
	  (match find_ok i me with
	  |`OK m -> `OK m
	  |`Error er -> `OK (l, M_Lookup i))

    and el_base_item item =
      match item with 
      | EDule.Base_type f -> Base_type f
      | EDule.Base_value t -> Base_value t

    and el_link_ilist n pe0 me0 li =
      foldr1ok (pe0, me0, nil, nil, nil) 
	(fun item (pe10, me10, pe1, me1, mer1) ->
	  (match el_link_item n pe10 me10 item with
	  |`OK (pe, me, mer) ->
	      (match append_unique pe pe1 with
	      |`OK pe2 -> 
		  (match append_unique me me1 with
		  |`OK me2 -> 
		      let (pe10s, me10s) = 
			if !allow_overwriting 
			then
			  (subtract pe10 pe, subtract me10 me)
			else
			  (pe10, me10)
		      in
		      (match append_unique pe pe10s with
		      |`OK pe20 -> 
			  (match append_unique me me10s with
			  |`OK me20 -> 
			      (match append_unique mer mer1 with
			      |`OK mer2 -> 
				  `OK (pe20, me20, pe2, me2, mer2)
			      |`Error er -> `Error er)
			  |`Error er -> `Error er)
		      |`Error er -> `Error er)
		  |`Error er -> `Error er)
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)) li

    and el_link_item n pe me item =
      match item with 
      | EDule.Link_Dule (i, m) -> 
	  (match el_def_dule n pe me (i, m) with
	  |`OK m ->
	      `OK (nil, nil, cons (i, m) nil)
	  |`Error er -> `Error er)
      | EDule.Link_Ind_Dule z -> 
	  (match el_ind_dule n pe me z false with
	  |`OK mez -> 
	      `OK (nil, nil, mez)
	  |`Error er -> `Error er)
      | EDule.Link_CoInd_Dule z ->
	  (match el_coind_dule n pe me z false with
	  |`OK mez -> 
	      `OK (nil, nil, mez)
	  |`Error er -> `Error er)
      | EDule.Link_Sp (i, p) ->
	  let n = n ^ "@Sp" in
	  (match el_def_sp n pe me (i, p) with
	  |`OK p -> 
	      let n = n ^ "@" ^ IdIndex.t2s i in
	      let l = loc_sp p in
	      let p = (l, S_Mm (n, p)) in
	      `OK (cons (i, p) nil, nil, nil)
	  |`Error er -> `Error er)
      | EDule.Link_Rec_Sp y -> 
	  let n = n ^ "@Sp" in
	  let pe = subtract pe y in
	  (match el_rec_sp n pe me y with
	  |`OK pey ->
(* optimized away:
              let pey = bmap (fun (i, p) -> 
		let n = n ^ "@" ^ IdIndex.t2s i in
		let l = loc_sp p in
		(l, S_Mm (n, p))) pey
	      in
*)
	      `OK (pey, nil, nil)
	  |`Error er -> `Error er)
      | EDule.Link_Lib (i, m) -> 
	  let n = n ^ "@Lib" in
	  (match el_def_dule n pe me (i, m) with
	  |`OK m ->
	      let n = n ^ "@" ^ IdIndex.t2s i in
	      let l = loc_dule m in
	      let m = (l, M_Memo_Lib (n, (l, M_Finish m))) in
	      `OK (nil, cons (i, m) nil, nil)
	  |`Error er -> `Error er)
      | EDule.Link_Ind_Lib z -> 
	  let me = subtract me z in
	  let n = n ^ "@Lib" in
	  (match el_ind_dule n pe me z true with
	  |`OK mez -> 
	      let mez = bmap (fun (i, m) -> 
		let n = n ^ "@" ^ IdIndex.t2s i in
		let l = loc_dule m in
		(l, M_Memo_Lib (n, (l, M_Finish m)))) mez 
	      in
	      `OK (nil, mez, nil)
	  |`Error er -> `Error er)
      | EDule.Link_CoInd_Lib z ->
	  let me = subtract me z in
	  let n = n ^ "@Lib" in
	  (match el_coind_dule n pe me z true with
	  |`OK mez -> 
	      let mez = bmap (fun (i, m) -> 
		let n = n ^ "@" ^ IdIndex.t2s i in
		let l = loc_dule m in
		(l, M_Memo_Lib (n, (l, M_Finish m)))) mez 
	      in
	      `OK (nil, mez, nil)
	  |`Error er -> `Error er)

    and el_def_dule n pe me (i, m) =
      let n = n ^ "@" ^ IdIndex.t2s i in
      (match el_dule n pe me m with
      |`OK m ->
	  let i2 = AtIndex.dule2sp i in
	  (match find_ok i2 pe with
	  |`OK p ->
	      let l = loc_dule m in
	      `OK (l, M_Spec (m, p))
	  |`Error er ->
	      `OK m)
      |`Error er -> `Error er)

    and el_ind_dule n pe me lm finishp =
      match bchoose lm with
      |`OK (i, m) -> 
	  (match bmap1ok (el_def_dule n pe me) lm with
	  |`OK me -> 
	      let n = n ^ "@Ind" ^ IdIndex.t2s i in
	      let l = EDule.loc_dule m in
	      let m = (l, M_Ind me) in
	      let m = if finishp then (l, M_Finish m) else m in
	      let m = (l, M_Memo (n, m)) in
	      let lm = bmap (fun (i, m') -> 
		let l = loc_dule m' in
		(l, M_Comp (m, (l, M_Pr i)))) me in
	      `OK lm
	  |`Error er -> `Error er)
      |`Error er -> assert false

    and el_coind_dule n pe me lm finishp =
      match bchoose lm with
      |`OK (i, m) -> 
	  (match bmap1ok (el_def_dule n pe me) lm with
	  |`OK me -> 
	      let n = n ^ "@Ind" ^ IdIndex.t2s i in
	      let l = EDule.loc_dule m in
	      let m = (l, M_CoInd me) in
	      let m = if finishp then (l, M_Finish m) else m in
              (* surprisingly, without this type reconstruction may fail
                 (though typing through typing rules is still correct): *)
	      let m = (l, M_Memo (n, m)) in
	      let lm = bmap (fun (i, m') -> 
		let l = loc_dule m' in
		(l, M_Comp (m, (l, M_Pr i)))) me in
	      `OK lm
	  |`Error er -> `Error er)
      |`Error er -> assert false

    and el_def_sp n pe me (i, p) =
      let n = n ^ "@" ^ IdIndex.t2s i in
      el_sp n pe me p

    and el_rec_sp n pe me lp =
      match bchoose lp with
      |`OK (i, p) -> 
	  (match bmap1ok (el_def_sp n pe me) lp with
	  |`OK lp -> 
	      let lp' = rmap AtIndex.sp2dule lp in
	      `OK (bmap (fun (i, p) ->
		let l = loc_sp p in
		(l, S_Rr (n, lp', AtIndex.sp2dule i))) lp)
	  |`Error er -> `Error er)
      |`Error er -> assert false
  end 

module ElabEDule = ElabEDule' (IdIndex) (AtIndex) (IList) (Location)
    (ErrorRepLib) (BCore) (BDule) (EDule)
