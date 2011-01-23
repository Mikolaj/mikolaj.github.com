(* Copyright (C) 2003--2006 Mikolaj Konarski
 * Copyright (C) 2003 Andrzej Gasienica-Samek
 * Copyright (C) 2006 Tomasz Kolinko
 *
 * This file is part of the Dule compiler.
 * The Dule compiler is released under the GNU General Public License (GPL).
 * Please see the file Dule-LICENSE for license information.
 *
 * $Id: core_back.ml,v 1.106 2006-10-14 01:56:18 mikon Exp $
 *) 

open Tools open Error_rep

module type T =
  sig	
    type t
  end

module type FCat = (* categories of f-Core from the PhD thesis *)
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t

    type t

    val t2de : t ->
      [`C_PP of t IList.t
      |`C_BB]
    val t2stamp : t -> int

    val c_PP : t IList.t -> t
    val c_BB : t
  end

module type DeFCat = (* only means for destruction *)
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t

    type t

    val t2de : t ->
      [`C_PP of t IList.t
      |`C_BB]
    val t2stamp : t -> int
  end

module type ConFCat = (* only constructive view *)
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t

    type t

    val c_PP : t IList.t -> t
    val c_BB : t
  end


module type PairFCat = (* more concrete than DeFCat *)
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t

    type t = t' Hashcons.hash_consed
    and t' =
      [`C_PP of t IList.t
      |`C_BB]

    val t2de : t -> t'
    val t2stamp : t -> int
  end

module PairFCat'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    : (PairFCat
    with module IdIndex = IdIndex
    with module IList = IList) =
  struct
    module IdIndex = IdIndex
    module IList = IList

    type t = t' Hashcons.hash_consed
    and t' =
      [`C_PP of t IList.t
      |`C_BB]

    let t2de c = c.Hashcons.node
    let t2stamp n = n.Hashcons.hkey
  end

module PairFCat = PairFCat' (IdIndex) (IList)

module DeFCat = PairFCat

module ACat = PairFCat (* : T --- totally abstract categories *)


module type EqFCat =
  sig
    module Cat : T
    val eq : Cat.t -> Cat.t -> bool
  end

module EqFCat' 
    (Cat : DeFCat) 
    : (EqFCat
    with module Cat = Cat) =
  struct
    module Cat = Cat

    let eq c d = (c == d) (* correct, because of the hash-consing *)
  end

module EqFCat = EqFCat' (DeFCat)


module Cat2HC'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : PairFCat
    with module IdIndex = IdIndex 
    with module IList = IList)
    (EqFCat : EqFCat with type Cat.t = Cat.t)
    : (Hashcons.HashedType
    with type t = Cat.t') =
  struct
    type t = Cat.t'
    let eq_inner eq c d =
      match c, d with
      |`C_PP lc,`C_PP ld -> IList.eqset eq lc ld
      |`C_BB,`C_BB -> true 
      | _ -> false
    let equal = eq_inner EqFCat.eq
    let hash c = abs
      (match c with
      |`C_BB -> 1
      |`C_PP lc -> 
	  IList.bfold 0
	    (fun (i, c) n -> 
	      let ni = IdIndex.stamp_t i in
	      let nc = Cat.t2stamp c in
	      n + (combine nc ni)) lc)
  end

module Cat2HC = Cat2HC' (IdIndex) (IList) (PairFCat) (EqFCat)


module HashCat = Hashcons.Make (Cat2HC) 


module NFCat' (* only normal forms, no rewriting *)
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (PairFCat : PairFCat 
    with module IdIndex = IdIndex 
    with module IList = IList)
    (Cat2HC : Hashcons.HashedType 
    with type t = PairFCat.t')
    (HashCat : Hashcons.S
    with type key = Cat2HC.t)
    : (FCat 
    with module IdIndex = IdIndex
    with module IList = IList
    with type t = PairFCat.t) =
  struct

    include PairFCat

    let tbl = HashCat.create good_hash_size

    let c_PP lc = HashCat.hashcons tbl (`C_PP lc)
    let c_BB = HashCat.hashcons tbl `C_BB
  end

module NFCat = NFCat' (IdIndex) (IList) (PairFCat) 
    (Cat2HC) (HashCat)


module type SemFCat = 
  sig

    include FCat

    val coi : t -> t -> t IList.t (* a list of type argument and context *)
    val unPP : t -> t IList.t
    val unPPok : t -> [`OK of t IList.t|`Error of string]
    val isBB : t -> bool
    val isPP : t -> bool
  end

module SemFCat' (* no rewriting needed, only auxiliary operations *)
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList 
    with type Index.t = IdIndex.t)
    (NFCat : FCat
    with module IdIndex = IdIndex 
    with module IList = IList)
    : (SemFCat 
    with module IdIndex = IdIndex
    with module IList = IList
    with type t = NFCat.t) =
  struct
    include NFCat

    let coi atj atk = 
      IList.cons (AtIndex.atj, atj) (IList.cons (AtIndex.atk, atk) IList.nil)

    let unPP c =
      match t2de c with
      |`C_PP lc -> lc
      | _ -> failwith "SrcFCore.unPP"

    let unPPok c =
      match t2de c with
      |`C_PP lc -> `OK lc
      | _ -> `Error "unPPok: c not PP in "

    let isBB c = 
      match t2de c with
      |`C_PP _ -> false
      |`C_BB -> true

    let isPP c = 
      match t2de c with
      |`C_PP _ -> true
      |`C_BB -> false
  end

module SemFCat = SemFCat' (IdIndex) (AtIndex) (IList) (NFCat)

module FCat = SemFCat

module ConFCat = FCat


module type PpFCatTools =
  sig
    module Cat : DeFCat
    module PpTools : PpTools
    val pp_c : PpTools.outt -> Cat.t -> unit
    val pp_lc : PpTools.outt -> Cat.t Cat.IList.t -> unit
  end

module PpFCatTools'
    (IdIndex : IdIndex) 
    (IList : IList with type Index.t = IdIndex.t)
    (PpTools : PpTools
    with module IdIndex = IdIndex
    with module IList = IList)
    (Cat : DeFCat with module IdIndex = IdIndex and module IList = IList)
    : (PpFCatTools
    with module Cat = Cat
    with module PpTools = PpTools) =
  struct
    module Cat = Cat
    module PpTools = PpTools

    let rec pp_c (out : PpTools.outt) (c : Cat.t) =
      match Cat.t2de c with
      |`C_BB -> PpTools.printstring out "*"
      |`C_PP lc -> pp_lc out lc
    and pp_lc (out : PpTools.outt) (lc: Cat.t IList.t) =
      (PpTools.printlist "<" ">" "-" pp_c) out lc
   end

module PpFCatTools = PpFCatTools' (IdIndex) (IList) (PpTools) (DeFCat)


module type PpFCat =
  sig
    module PpTools : PpTools
    module Cat : DeFCat
    val pp_c : Cat.t -> string
    val pp_lc : Cat.t Cat.IList.t -> string
  end

module PpFCat'
    (IdIndex : IdIndex) 
    (IList : IList with type Index.t = IdIndex.t)
    (PpTools : PpTools
    with module IdIndex = IdIndex
    with module IList = IList)
    (Cat : DeFCat with module IdIndex = IdIndex and module IList = IList)
    (PpFCatTools : PpFCatTools
    with module PpTools = PpTools
    with module Cat = Cat)
    : (PpFCat
    with module Cat = Cat) =
  struct
    module Cat = Cat
    module PpTools = PpTools

    let pp_c = PpTools.pp2str PpFCatTools.pp_c
    let pp_lc = PpTools.pp2str PpFCatTools.pp_lc
   end

module PpFCat = PpFCat' (IdIndex) (IList) (PpTools) (DeFCat) (PpFCatTools)


module type FFunct = (* functors of f-Core *)
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module VarStamp : Stamp

    type t

    val t2de : t ->
      [`F_ID of Cat.t
      |`F_COMP of t * t
      |`F_PR of Cat.t IList.t * IdIndex.t
      |`F_RECORD of Cat.t * t IList.t
      |`F_pp of Cat.t * t IList.t
      |`F_ss of Cat.t * t IList.t
      |`F_ii of t
      |`F_tt of t
      |`F_ee of t IList.t * t
      |`F_finish of t
      |`F_var of VarStamp.t * Cat.t]
    val t2stamp : t -> int

    val f_ID : Cat.t -> t
    val f_COMP : t -> t -> t
    val f_PR : Cat.t IList.t -> IdIndex.t -> t
    val f_RECORD : Cat.t -> t IList.t -> t
    val f_pp : Cat.t -> t IList.t -> t
    val f_ss : Cat.t -> t IList.t -> t
    val f_ii : t -> t
    val f_tt : t -> t
    val f_ee : t IList.t -> t -> t
    val f_finish : t -> t
    val f_var : VarStamp.t -> Cat.t -> t
  end

module type DeFFunct = 
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module VarStamp : Stamp

    type t

    val t2de : t ->
      [`F_ID of Cat.t
      |`F_COMP of t * t
      |`F_PR of Cat.t IList.t * IdIndex.t
      |`F_RECORD of Cat.t * t IList.t
      |`F_pp of Cat.t * t IList.t
      |`F_ss of Cat.t * t IList.t
      |`F_ii of t
      |`F_tt of t
      |`F_ee of t IList.t * t
      |`F_finish of t
      |`F_var of VarStamp.t * Cat.t]
    val t2stamp : t -> int
  end

module type ConFFunct = 
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module VarStamp : Stamp

    type t

    val f_ID : Cat.t -> t
    val f_COMP : t -> t -> t
    val f_PR : Cat.t IList.t -> IdIndex.t -> t
    val f_RECORD : Cat.t -> t IList.t -> t
    val f_pp : Cat.t -> t IList.t -> t
    val f_ss : Cat.t -> t IList.t -> t
    val f_ii : t -> t
    val f_tt : t -> t
    val f_ee : t IList.t -> t -> t
    val f_finish : t -> t
    val f_var : VarStamp.t -> Cat.t -> t
  end


module type PairFFunct = (* more concrete than DeFFunct *)
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module VarStamp : Stamp

    type t = t' Hashcons.hash_consed
    and t' =
      [`F_ID of Cat.t
      |`F_COMP of t * t
      |`F_PR of Cat.t IList.t * IdIndex.t
      |`F_RECORD of Cat.t * t IList.t
      |`F_pp of Cat.t * t IList.t
      |`F_ss of Cat.t * t IList.t
      |`F_ii of t
      |`F_tt of t
      |`F_ee of t IList.t * t
      |`F_finish of t
      |`F_var of VarStamp.t * Cat.t]

    val t2de : t -> t'
    val t2stamp : t -> int
  end

module PairFFunct'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (VarStamp : Stamp)
    (Cat : T)
    : (PairFFunct
    with module IdIndex = IdIndex
    with module IList = IList
    with module Cat = Cat
    with module VarStamp = VarStamp) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat = Cat
    module VarStamp = VarStamp

    type t = t' Hashcons.hash_consed
    and t' =
      [`F_ID of Cat.t
      |`F_COMP of t * t
      |`F_PR of Cat.t IList.t * IdIndex.t
      |`F_RECORD of Cat.t * t IList.t
      |`F_pp of Cat.t * t IList.t
      |`F_ss of Cat.t * t IList.t
      |`F_ii of t
      |`F_tt of t
      |`F_ee of t IList.t * t
      |`F_finish of t
      |`F_var of VarStamp.t * Cat.t]

    let t2de f = f.Hashcons.node
    let t2stamp n = n.Hashcons.hkey
  end

module PairFFunct = PairFFunct' (IdIndex) (IList) (Stamp) (ACat)

module DeFFunct = PairFFunct

module AFunct = PairFFunct (* : T --- totally abstract functors *)


module type SrcFCore =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : DeFFunct 
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    val src : Funct.t -> Cat.t
    val trg : Funct.t -> Cat.t
  end

module SrcFCore'
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : SemFCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : DeFFunct 
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    : (SrcFCore
    with module IdIndex = IdIndex
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat = Cat
    module Funct = Funct

    let rec src f =
      match Funct.t2de f with
      |`F_ID c -> c
      |`F_COMP (f1, f2) -> src f1
      |`F_PR (lc, i) -> Cat.c_PP lc
      |`F_RECORD (c, lf) -> c
      |`F_pp (c, lf) -> c
      |`F_ss (c, lf) -> c
      |`F_ii g -> IList.find AtIndex.atk (Cat.unPP (src g))
      |`F_tt g -> IList.find AtIndex.atk (Cat.unPP (src g))
      |`F_ee (lg, h) -> src h
      |`F_finish f -> src f
      |`F_var (_, c) -> c

    let rec trg f =
      match Funct.t2de f with
      |`F_ID c -> c
      |`F_COMP (f1, f2) -> trg f2
      |`F_PR (lc, i) -> IList.find i lc
      |`F_RECORD (c, lf) -> Cat.c_PP (IList.vmap trg lf)
      |`F_pp (c, lf) -> Cat.c_BB
      |`F_ss (c, lf) -> Cat.c_BB
      |`F_ii g -> trg g
      |`F_tt g -> trg g
      |`F_ee (lg, h) -> Cat.c_BB
      |`F_finish f -> trg f
      |`F_var _ -> Cat.c_BB
   end

module SrcFCore = SrcFCore' (IdIndex) (AtIndex) (IList) (SemFCat) (DeFFunct) 


module type EqFFunct =
  sig
    module Funct : T
    val eq : Funct.t -> Funct.t -> bool
  end

module EqFFunct'
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : SemFCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : PairFFunct (* ugly *)
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (EqFCat : EqFCat with type Cat.t = Cat.t)
    (SrcFCore : SrcFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    : (EqFFunct
    with module Funct = Funct) =
  struct
    module Funct = Funct
    open Cat
    open IList
    open SrcFCore

(* if not for hash-consing this would be very laborious, as below,
   plus additional work to distinguish rigid and flexible lists

    (* [f] and [g] are in normal form and must have the same sources: *)    
    let rec eq_funct f g =
      f == g ||   
      match Funct.t2de f, Funct.t2de g with
      |`F_ID _,`F_ID _ -> true
      |`F_COMP (f1, f2),`F_COMP (g1, g2) -> 
	  eq_funct f1 g1 && eq_funct f2 g2
      |`F_PR (_, i),`F_PR (_, j) when IdIndex.eq i j -> true
      |`F_RECORD (_, lf),`F_RECORD (_, lg) -> eqset eq_funct lf lg
      |	_,`F_RECORD (d, lg) ->
	  let ef = trg f in 
	  let eg = trg g in 
	  EqFCat.eq ef eg && bforall (eq_pi d [] f) lg
      |`F_RECORD (c, lf), _ ->
	  let ef = trg f in 
	  let eg = trg g in 
	  EqFCat.eq ef eg && bforall (eq_pi c [] g) lf
      |`F_pp (_, lf),`F_pp (_, lg) -> eqset eq_funct lf lg
      |`F_ss (_, lf),`F_ss (_, lg) -> eqset eq_funct lf lg
      |`F_ii f,`F_ii g ->
	  let c1 = find AtIndex.atj (unPP (src f)) in
	  let d1 = find AtIndex.atj (unPP (src g)) in
	  EqFCat.eq c1 d1 && eq_funct f g
      |`F_tt f,`F_tt g ->
	  let c1 = find AtIndex.atj (unPP (src f)) in
	  let d1 = find AtIndex.atj (unPP (src g)) in
	  EqFCat.eq c1 d1 && eq_funct f g
      |`F_ee (lf, f),`F_ee (lg, g) ->
	  eq_funct f g && eqset eq_funct lf lg
      |`F_finish f,`F_finish g -> eq_funct f g 
      |`F_var (var_num, c),`F_var (var_num', c') -> 
	  (var_num = var_num')
      |	_ -> 
	  let ef = trg f in 
	  let eg = trg g in 
	  EqFCat.eq ef eg && is_trivial ef
    (* [eq_pi] gives [true] if and only if 
       [f] composed with projections at labels of [li] 
       (in reverse order) and the projection at [i]
       (as the last one) is equal to [g] *)
    and eq_pi d li f (i, g) =
      assert (EqFCat.eq d (src f) && EqFCat.eq d (src g));
      (* and [f] is not a record
         and [i] and [li] result in type-correct projections *)
      match Funct.t2de g with
      |`F_PR (_, j) when IdIndex.eq i j ->
          li = [] && eq_funct f (Stamp.dummy,`F_ID d)
      |`F_COMP (f', g) ->
	  (match Funct.t2de g with
	  |`F_PR (_, j) ->  
	      if IdIndex.eq i j then
		(match li with
		| [] -> eq_funct f f'
		| i1 :: r -> eq_pi d r f (i1, f'))
	      else false
	  | _ -> false)
      |`F_RECORD (_, lf) ->
          let ili = i :: li in
          bforall (eq_pi d ili f) lf
      | _ -> false
    and is_trivial c =
      match t2de c with
      |`C_BB -> false
      |`C_PP lc -> vforall is_trivial lc

    (* [f], [g] may have different sources and different targets: *)    
    let eq f g = 
      EqFCat.eq (src f) (src g) &&
      eq_funct f g

   But with hash-consing this suffices: *)

    let eq f g = (f == g)
  end

module EqFFunct = EqFFunct' (IdIndex) (AtIndex) (IList) (SemFCat) (PairFFunct)
    (EqFCat) (SrcFCore)


module Funct2HC'
    (IdIndex : IdIndex)
    (IListBasic : IListBasic with type Index.t = IdIndex.t)
    (IList : IList 
    with type Index.t = IdIndex.t
    with type 'a IListBasic.t = 'a IListBasic.t)
    (Cat : FCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : PairFFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (EqFCat : EqFCat with type Cat.t = Cat.t)
    (EqFFunct : EqFFunct with type Funct.t = Funct.t)
    (SrcFCore : SrcFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with type Funct.t = Funct.t)
    : (Hashcons.HashedType
    with type t = Funct.t') =
  struct

    type t = Funct.t'

    (* [f] and [g] in normal (beta-eta) form *)
    (* and may have different sources and different targets *)
    (* additionally immediate subterms of [f] and [g] have to be perfectly *)
    (* hash-consed so that [EqFFunct.eq] is a valid equality on them *)
    (* if IList.t were sorted, would this amount to structural eq ? *)
    let eq_inner eq f g =
      let eq_lf_inner c d lf lg = 
	((IListBasic.is_rigid lf && IListBasic.is_rigid lg && EqFCat.eq c d)
       || (IListBasic.is_flexible lf && IListBasic.is_flexible lg
	     && IListBasic.IListStamp.eq 
	          (IListBasic.get_stamp lf) (IListBasic.get_stamp lg)
	     && eq (IListBasic.get_elem lf) (IListBasic.get_elem lg))) 
	  && IList.eqset eq lf lg
      in
      match f, g with
      |`F_ID c,`F_ID d -> EqFCat.eq c d
      |`F_COMP (f1, f2),`F_COMP (g1, g2) -> eq f1 g1 && eq f2 g2
      |`F_PR (lc, i),`F_PR (ld, j) -> 
	  IdIndex.eq i j && EqFCat.eq (Cat.c_PP lc) (Cat.c_PP ld) 
      |`F_RECORD (c, lf),`F_RECORD (d, lg) -> eq_lf_inner c d lf lg
      |`F_pp (c, lf),`F_pp (d, lg) -> eq_lf_inner c d lf lg
      |`F_ss (c, lf),`F_ss (d, lg) -> eq_lf_inner c d lf lg
      |`F_ii f,`F_ii g -> eq f g
      |`F_tt f,`F_tt g -> eq f g
      |`F_ee (lf, f),`F_ee (lg, g) ->
	  eq f g && eq_lf_inner Cat.c_BB Cat.c_BB lf lg
      |`F_finish f,`F_finish g -> eq f g 
      |`F_var (var_num, c),`F_var (var_num', c') -> 
	  Funct.VarStamp.eq var_num var_num' && EqFCat.eq c c'
      |	_, _ -> false

    let equal f g = eq_inner EqFFunct.eq f g

    let hash_lf c lf =
      if IList.is_nil lf then 
	if IListBasic.is_flexible lf then
	  let ne = Funct.t2stamp (IListBasic.get_elem lf) in
	  let ns = IListBasic.IListStamp.t2int (IListBasic.get_stamp lf) in
	  combine ne ns
	else Cat.t2stamp c
      else
	let nl =
	  IList.bfold 0
	    (fun (i, f) n -> 
	      let ni = IdIndex.stamp_t i in
	      let nf = Funct.t2stamp f in
	      n + (combine ni nf)) lf
	in
	if IListBasic.is_flexible lf then
	  let ne = Funct.t2stamp (IListBasic.get_elem lf) in
	  let ns = IListBasic.IListStamp.t2int (IListBasic.get_stamp lf) in
	  nl + combine ne ns
	else nl

    let hash f = abs
      (match f with
      |`F_ID c -> 17 * Cat.t2stamp c
      |`F_COMP (f, g) -> 
	  19 * combine (Funct.t2stamp f) (Funct.t2stamp g)
      |`F_PR (lc, i) -> 
	  23 * combine (Cat.t2stamp (Cat.c_PP lc)) (IdIndex.stamp_t i)
      |`F_RECORD (c, lf) -> 29 * hash_lf c lf
      |`F_pp (c, lf) -> 31 * hash_lf c lf
      |`F_ss (c, lf) -> 37 * hash_lf c lf
      |`F_ii g -> 41 * Funct.t2stamp g
      |`F_tt g -> 43 * Funct.t2stamp g
      |`F_ee (lg, h) -> 47 * combine (hash_lf Cat.c_BB lg) (Funct.t2stamp h)
      |`F_finish f -> 53 * Funct.t2stamp f
      |`F_var (var_num, c) -> 
	  59 * combine (Funct.VarStamp.t2int var_num) (Cat.t2stamp c))
  end

module Funct2HC = Funct2HC' (IdIndex) (IListBasic) (IList) 
    (FCat) (PairFFunct) (EqFCat) (EqFFunct) (SrcFCore)


module HashFunct = Hashcons.Make (Funct2HC)


module NFFunct' (* only normal forms, no rewriting *)
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : T)
    (PairFFunct : PairFFunct 
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Funct2HC : Hashcons.HashedType with type t = PairFFunct.t')
    (HashFunct : Hashcons.S
    with type key = Funct2HC.t)
    : (FFunct 
    with module IdIndex = IdIndex
    with module IList = IList
    with module Cat = PairFFunct.Cat
    with type t = PairFFunct.t
    with module VarStamp = PairFFunct.VarStamp) =
  struct

    include PairFFunct

    let tbl = HashFunct.create good_hash_size

    let hashcons = HashFunct.hashcons tbl

    let f_ID c = hashcons (`F_ID c)
    let f_COMP f g = hashcons (`F_COMP (f, g))
    let f_PR lc i = hashcons (`F_PR (lc, i))
    let f_RECORD c lf = hashcons (`F_RECORD (c, lf))
    let f_pp c lf = hashcons (`F_pp (c, lf))
    let f_ss c lf = hashcons (`F_ss (c, lf))
    let f_ii g = hashcons (`F_ii g)
    let f_tt g = hashcons (`F_tt g)
    let f_ee lg h = hashcons (`F_ee (lg, h))
    let f_finish f = hashcons (`F_finish f) (* assert: no `F_finish in f *)
    let f_var var_num c = hashcons (`F_var (var_num, c))
  end

module NFFunct = NFFunct' (IdIndex) (AtIndex) (IList) (ACat)
    (PairFFunct) (Funct2HC) (HashFunct)


module type PpFFunctTools =
  sig
    module Funct : DeFFunct
    module PpTools : PpTools
    val pp_f : PpTools.outt -> Funct.t -> unit
    val pp_lf : PpTools.outt -> Funct.t Funct.IList.t -> unit
  end

module PpFFunctTools'
    (IdIndex : IdIndex) 
    (IList : IList with type Index.t = IdIndex.t)
    (PpTools : PpTools
    with module IdIndex = IdIndex
    with module IList = IList)
    (Cat : DeFCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : DeFFunct 
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (PpFCatTools : PpFCatTools
    with module PpTools = PpTools
    with module Cat = Cat)
    : (PpFFunctTools
    with module Funct = Funct
    with module PpTools = PpTools) =
  struct
    module Funct = Funct
    module PpTools = PpTools

    let rec pp_f (out : PpTools.outt) (f: Funct.t) =
      match Funct.t2de f with
      |`F_ID c -> PpTools.printraw "F_ID" PpFCatTools.pp_c out c
      |`F_COMP (f, g) -> PpTools.printbinop "." pp_f pp_f out f g
      |`F_PR (lc, i) -> 
	  PpTools.printraw2 "F_PR" PpFCatTools.pp_lc PpTools.printindex 
	    out lc i
      |`F_RECORD (c, lf) -> pp_lf out lf
      |`F_pp (c, lf) -> PpTools.printlist "{" "}" ":" pp_f out lf
      |`F_ss (c, lf) -> PpTools.printlistbackquote "[" "]" pp_f out lf
      |`F_ii g -> PpTools.printraw "F_ii" pp_f out g
      |`F_tt g -> PpTools.printraw "F_tt" pp_f out g
      |`F_ee (lg, h) -> PpTools.printraw2 "F_ee" pp_lf pp_f out lg h
      |`F_finish f -> PpTools.printraw "F_finish" pp_f out f
      |`F_var (var_num, c) ->
          PpTools.printraw2 "F_var" PpTools.printstring PpFCatTools.pp_c
            out (Funct.VarStamp.t2string var_num) c
    and pp_lf (out : PpTools.outt) (lf: Funct.t IList.t) =
      PpTools.printlist "<" ">" ":" pp_f out lf
   end

module PpFFunctTools = PpFFunctTools' (IdIndex) (IList) (PpTools)
    (DeFCat) (DeFFunct) (PpFCatTools) 


module type PpFFunct =
  sig
    module Funct : DeFFunct
    val pp_f : Funct.t -> string
    val pp_lf : Funct.t Funct.IList.t -> string
  end

module PpFFunct'
    (IdIndex : IdIndex) 
    (IList : IList with type Index.t = IdIndex.t)
    (PpTools : PpTools
    with module IdIndex = IdIndex
    with module IList = IList)
    (Cat : DeFCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : DeFFunct 
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (PpFFunctTools : PpFFunctTools
    with module PpTools = PpTools
    with module Funct = Funct)
    : (PpFFunct
    with module Funct = Funct) =
  struct
    module Funct = Funct

    let pp_f = PpTools.pp2str PpFFunctTools.pp_f
    let pp_lf = PpTools.pp2str PpFFunctTools.pp_lf
   end

module PpFFunct = PpFFunct' (IdIndex) (IList) (PpTools)
    (DeFCat) (DeFFunct) (PpFFunctTools)


module type SemFFunct =
  sig

    include FFunct

    val paf : t -> t -> t
    val fig : t -> t -> t
    val is_trivial : Cat.t -> bool
    val triv : Cat.t -> Cat.t -> t

    val cof : t -> t -> t IList.t (* a list of value argument and context *)
    val coi : t -> t -> t IList.t (* a list of type argument and context *)

    val unpp : t -> t IList.t
    val unpp_ok : t -> [`OK of t IList.t|`Error of string]
    val unii : t -> t
    val untt : t -> t
    val unfinish : t -> t 
  end

module SemFFunct' (* rewriting, or semantics in terms of other operations *)
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IListBasic : IListBasic with type Index.t = IdIndex.t)
    (IList : IList 
    with type Index.t = IdIndex.t
    with type 'a IListBasic.t = 'a IListBasic.t)
    (Cat : SemFCat with module IdIndex = IdIndex and module IList = IList)
    (NFFunct : FFunct 
    with type Cat.t = Cat.t
    with module IdIndex = IdIndex 
    with module IList = IList)
    (PpFCat : PpFCat with type Cat.t = Cat.t)
    (EqFCat : EqFCat with type Cat.t = Cat.t)
    (PpFFunct : PpFFunct
    with type Funct.t = NFFunct.t)
    (EqFFunct : EqFFunct with type Funct.t = NFFunct.t)
    (SrcFCore : SrcFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with type Funct.t = NFFunct.t)
    : (SemFFunct 
    with module IdIndex = IdIndex
    with module IList = IList
    with type Cat.t = Cat.t
    with type t = NFFunct.t
    with module VarStamp = NFFunct.VarStamp) =
  struct
    open NFFunct
    module VarStamp = VarStamp
    module Cat = Cat
    module IList = IList
    module IdIndex = IdIndex
    open SrcFCore

    let t2stamp = NFFunct.t2stamp
    let t2de= NFFunct.t2de
    type t = NFFunct.t

    let coi atj atk = 
      IList.cons (AtIndex.atj, atj) (IList.cons (AtIndex.atk, atk) IList.nil)

    let cof atd ate = 
      IList.cons (AtIndex.atd, atd) (IList.cons (AtIndex.ate, ate) IList.nil)

    let rec is_trivial c =
      match Cat.t2de c with
      |`C_BB -> false
      |`C_PP lc -> IList.vforall is_trivial lc

    let rec triv c e =
      if EqFCat.eq c e then f_ID c
      else
	match Cat.t2de e with
	|`C_BB -> failwith "SemFFunct.triv BB"
	|`C_PP lc ->
	    f_RECORD c
	      (IList.bfold IList.nil
		 (fun (i, d) lf ->
		   IList.cons (i, triv c d) lf) lc)

    let unfinish g =
      match Funct.t2de g with
      |`F_finish f -> f
      | _ -> g

    let f_ID = f_ID

    let f_PR lc i = 
      let e = IList.find i lc in
      if is_trivial e then triv (Cat.c_PP lc) e else f_PR lc i

    let f_pp c lf = 
      (* compound products not used, yet. Otherwise use triv, etc.
         too long:
      assert (IList.vforall (fun f -> Cat.isBB (trg f)) lf); *)
      f_pp c lf

    let f_ss c lf = (*
      assert (IList.vforall (fun f -> Cat.isBB (trg f)) lf); *)
      f_ss c lf

   let f_ii g =
      let e = trg g in
      if is_trivial e then 
	triv (IList.find AtIndex.atk (Cat.unPP (src g))) e 
      else f_ii g

    let f_tt g =
      let e = trg g in
      if is_trivial e then 
	triv (IList.find AtIndex.atk (Cat.unPP (src g))) e 
      else f_tt g

    let f_ee lg h =
      assert (IList.not_in AtIndex.atu lg) (* &&
	      Cat.isBB (trg h) && 
	      IList.vforall (fun f -> Cat.isBB (trg f)) lg) *) ;
      f_ee lg h

    let f_finish = f_finish

    let f_var = f_var

    let check lc lf eta f =
      match eta with 
      |`Suspect_eta sf -> 
	  if EqFFunct.eq f sf then `Suspect_eta sf
	  else `No_eta
      |`Undecided -> 
	 let ld = IList.vmap trg lf in
	 if IList.eqset EqFCat.eq lc ld then `Suspect_eta f
	 else `No_eta
      |`No_eta -> failwith "SemFFunct.check: `No_eta"

    let f_RECORD c lf = (* [f_i] in normal (beta-eta) form *)
      (* This will not remove ilists needed for unification,
         because src and trg is always know and fixed, so lf rigid.
         On the other hand ilist unification is not needed for eta. *)
      if IList.is_nil lf then (* for speed *)
	let pp_empty = Cat.c_PP IList.nil in 
	if EqFCat.eq c pp_empty then f_ID c
	else f_RECORD c lf
      else 
	(assert (IList.vforall (fun f -> EqFCat.eq (src f) c) lf);
         let eta =
	   IList.bfold `Undecided
	     (fun (i, f) eta ->
	       (match eta with
	       |`No_eta -> `No_eta
	       | _ ->
		   (match Funct.t2de (unfinish f) with
		   |`F_PR (lc, j) ->
		       if IdIndex.eq i j then
			 check lc lf eta (f_ID c)
		       else `No_eta
		   |`F_COMP (f', g) -> 
		       (match Funct.t2de (unfinish g) with
		       |`F_PR (lc, j) ->
			   if IdIndex.eq i j then
			     check lc lf eta f'
			   else `No_eta
		       | _ -> `No_eta)
		   |`F_RECORD (_, lf) -> 
    		       let e = trg f in (* if trivial then f_RECORD *)
		       if is_trivial e then eta (* fits everything. *)
		       else `No_eta (* because otherwise we would loop *)
		   | _ -> 
		       `No_eta))) lf
	 in
	 match eta with 
	 |`Suspect_eta f -> f
	 |`Undecided -> triv c (Cat.c_PP (IList.vmap trg lf))
	 |`No_eta -> f_RECORD c lf)
 
    let fig f_COMP f g =
      let cgf = Cat.coi (trg g) (src f) in
      let prf = f_COMP (f_PR cgf AtIndex.atk) f in
      let rep = f_RECORD (Cat.c_PP cgf) (coi (f_PR cgf AtIndex.atj) prf) in
      f_COMP rep g
	  
    let f_COMP f_COMP (* for recursive memoization *) (f, g) =
      let f_COMP f g = f_COMP (f, g) in
      let fig = fig f_COMP in
      let f_COMP_lg f lg = 
	let lfg = IList.vmap (fun g -> f_COMP f g) lg in
	if IListBasic.is_rigid lg then lfg
	else
	  let l_stamp = IListBasic.get_stamp lg in
	  let gs = IListBasic.get_elem lg in
	  let fgs = f_COMP f gs in
	  IListBasic.rigid2flexible l_stamp fgs lfg
      in
      (* assert
	 (if not (EqFCat.eq (src g) (trg f)) then
	  failwith ("SemFFunct.f_COMP: bad typing:\nc(" 
		    ^ PpFCat.pp_c (src g) 
		    ^ ") <> \ne("
		    ^ PpFCat.pp_c (trg f) 
		    ^ ")")
	else true); *)
      match Funct.t2de f, Funct.t2de g with (* left-associative *)
      | _,`F_ID d -> f
      |`F_ID c, _ -> g (* now (F_finish (F_ID);`F_Var) is a normal form! *)
      | _,`F_COMP (g1, g2) -> f_COMP (f_COMP f g1) g2
      |`F_RECORD (c, lf),`F_PR (ld, j) -> IList.find j lf
      | _,`F_RECORD (d, lg) ->
	  assert (IListBasic.is_rigid lg);
	  f_RECORD (src f) (IList.vmap (fun g -> f_COMP f g) lg)
      | _,`F_pp (d, lg) -> f_pp (src f) (f_COMP_lg f lg)
      | _,`F_ss (d, lg) -> f_ss (src f) (f_COMP_lg f lg)
      | _,`F_ii g1 -> f_ii (fig f g1)
      | _,`F_tt g1 -> f_tt (fig f g1)
      | _,`F_ee (lg, g1) -> f_ee (f_COMP_lg f lg) (f_COMP f g1)
(* TODO pp . PR, etc., but then eta-expansion fails! *)
      |`F_finish f,`F_finish g -> f_finish (f_COMP f g)
      |`F_finish f,`F_PR (ld, j) -> f_finish (f_COMP f g)
      | _,`F_finish g -> f_COMP f g 
      (* possible normal forms: *)
      | _,`F_PR _
      | _,`F_var _ 
	-> NFFunct.f_COMP f g
	    
(* Now memoizing f_COMP! *speed* :) *)
    module HC = 
      struct
	type t = NFFunct.t * NFFunct.t
	type result = NFFunct.t
	let equal (f1, f2) (g1, g2) = 
	  EqFFunct.eq f1 g1 && EqFFunct.eq f2 g2
	let hash (f1, f2) = abs (combine (Funct.t2stamp f1) (Funct.t2stamp f2))
      end
    module MemoComp = Memoize' (HC)

    let tbl = MemoComp.create good_hash_size

    let f_COMP f g = MemoComp.memorec tbl f_COMP (f, g)

    let fig = fig f_COMP

    let paf f g = 
      let c = src f in
      let lf = coi f (f_ID c) in
      f_COMP (f_RECORD c lf) g	

    let unpp f =
      match t2de f with
      |`F_pp (c, lf) -> lf
      | _ -> failwith "DomFCore.unpp" (* beware: compound products! *)
    let unpp_ok f =
      match t2de f with
      |`F_pp (c, lf) -> `OK lf
      | _ -> `Error "DomFCore.unpp_ok"
    let unii f = 
      match t2de f with
      |`F_ii g -> g
      | _ ->
	  let e = trg f in
	  if is_trivial e then
	    let c = src f in
	    triv (Cat.c_PP (coi e c)) e
	  else
	    failwith "DomFCore.unii"
    let untt f =
      match t2de f with
      |`F_tt g -> g
      | _ ->
	  let e = trg f in
	  if is_trivial e then
	    let c = src f in
	    triv (Cat.c_PP (coi e c)) e
	  else
	    failwith "DomFCore.untt"
(* I assume there is no `F_finish anywhere below *)
  end

module SemFFunct = SemFFunct' (IdIndex) (AtIndex) (IListBasic) (IList)
    (SemFCat) (NFFunct) (PpFCat) (EqFCat) (PpFFunct) (EqFFunct) (SrcFCore)

module FFunct = SemFFunct

module ConFFunct = FFunct


module type FTrans = (* transformations of f-Core *)
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Location : Location

    type t

    val t2de : t ->
      [`T_ID of Cat.t
      |`T_COMP of t * t
      |`T_PR of Cat.t IList.t * IdIndex.t
      |`T_RECORD of Cat.t * t IList.t
      |`T_FT of Funct.t * t
      |`T_TF of t * Funct.t
      |`T_id of Funct.t
      |`T_comp of t * t
      |`T_pp of Cat.t * t IList.t
      |`T_pr of Funct.t IList.t * IdIndex.t
      |`T_record of Funct.t * t IList.t
      |`T_ss of Cat.t * t IList.t
      |`T_in of Funct.t IList.t * IdIndex.t
      |`T_case of t IList.t * Funct.t
      |`TL_case of Funct.t * t IList.t * Funct.t
      |`T_map of Funct.t * t
      |`T_ii of t
      |`T_con of Funct.t
      |`T_fold of Funct.t * t
      |`TL_fold of Funct.t * t
      |`T_de of Funct.t
      |`T_tt of t
      |`T_uncon of Funct.t
      |`T_unfold of Funct.t * t
      |`TL_unfold of Funct.t * t
      |`T_unde of Funct.t
      |`T_ee of t IList.t * t
      |`T_appl of Funct.t IList.t * Funct.t
      |`T_curry of t
      |`T_assert of t * t * Location.t
      |`T_fail of Funct.t * Funct.t * Location.t
      |`T_fix of t
      |`TL_fix of t]
    val t2stamp : t -> int

    val t_ID : Cat.t -> t
    val t_COMP : t -> t -> t
    val t_PR : Cat.t IList.t -> IdIndex.t -> t
    val t_RECORD : Cat.t -> t IList.t -> t
    val t_FT : Funct.t -> t -> t
    val t_TF : t -> Funct.t -> t
    val t_id : Funct.t -> t
    val t_comp : t -> t -> t
    val t_pp : Cat.t -> t IList.t -> t
    val t_pr : Funct.t IList.t -> IdIndex.t -> t
    val t_record : Funct.t -> t IList.t -> t
    val t_ss : Cat.t -> t IList.t -> t
    val t_in : Funct.t IList.t -> IdIndex.t -> t
    val t_case : t IList.t -> Funct.t -> t
    val tl_case : Funct.t -> t IList.t -> Funct.t -> t
    val t_map : Funct.t -> t -> t
    val t_ii : t -> t
    val t_con : Funct.t -> t
    val t_fold : Funct.t -> t -> t
    val tl_fold : Funct.t -> t -> t
    val t_de : Funct.t -> t
    val t_tt : t -> t
    val t_uncon : Funct.t -> t
    val t_unfold : Funct.t -> t -> t
    val tl_unfold : Funct.t -> t -> t
    val t_unde : Funct.t -> t
    val t_ee : t IList.t -> t -> t
    val t_appl : Funct.t IList.t -> Funct.t -> t
    val t_curry : t -> t
    val t_assert : t -> t -> Location.t -> t
    val t_fail : Funct.t -> Funct.t -> Location.t -> t
    val t_fix : t -> t
    val tl_fix : t -> t
  end

module type DeFTrans = 
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Location : Location

    type t

    val t2de : t ->
      [`T_ID of Cat.t
      |`T_COMP of t * t
      |`T_PR of Cat.t IList.t * IdIndex.t
      |`T_RECORD of Cat.t * t IList.t
      |`T_FT of Funct.t * t
      |`T_TF of t * Funct.t
      |`T_id of Funct.t
      |`T_comp of t * t
      |`T_pp of Cat.t * t IList.t
      |`T_pr of Funct.t IList.t * IdIndex.t
      |`T_record of Funct.t * t IList.t
      |`T_ss of Cat.t * t IList.t
      |`T_in of Funct.t IList.t * IdIndex.t
      |`T_case of t IList.t * Funct.t
      |`TL_case of Funct.t * t IList.t * Funct.t
      |`T_map of Funct.t * t
      |`T_ii of t
      |`T_con of Funct.t
      |`T_fold of Funct.t * t
      |`TL_fold of Funct.t * t
      |`T_de of Funct.t
      |`T_tt of t
      |`T_uncon of Funct.t
      |`T_unfold of Funct.t * t
      |`TL_unfold of Funct.t * t
      |`T_unde of Funct.t
      |`T_ee of t IList.t * t
      |`T_appl of Funct.t IList.t * Funct.t
      |`T_curry of t
      |`T_assert of t * t * Location.t
      |`T_fail of Funct.t * Funct.t * Location.t
      |`T_fix of t
      |`TL_fix of t]
    val t2stamp : t -> int
  end

module type ConFTrans = 
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Location : Location

    type t

    val t_ID : Cat.t -> t
    val t_COMP : t -> t -> t
    val t_PR : Cat.t IList.t -> IdIndex.t -> t
    val t_RECORD : Cat.t -> t IList.t -> t
    val t_FT : Funct.t -> t -> t
    val t_TF : t -> Funct.t -> t
    val t_id : Funct.t -> t
    val t_comp : t -> t -> t
    val t_pp : Cat.t -> t IList.t -> t
    val t_pr : Funct.t IList.t -> IdIndex.t -> t
    val t_record : Funct.t -> t IList.t -> t
    val t_ss : Cat.t -> t IList.t -> t
    val t_in : Funct.t IList.t -> IdIndex.t -> t
    val t_case : t IList.t -> Funct.t -> t
    val tl_case : Funct.t -> t IList.t -> Funct.t -> t
    val t_map : Funct.t -> t -> t
    val t_ii : t -> t
    val t_con : Funct.t -> t
    val t_fold : Funct.t -> t -> t
    val tl_fold : Funct.t -> t -> t
    val t_de : Funct.t -> t
    val t_tt : t -> t
    val t_uncon : Funct.t -> t
    val t_unfold : Funct.t -> t -> t
    val tl_unfold : Funct.t -> t -> t
    val t_unde : Funct.t -> t
    val t_ee : t IList.t -> t -> t
    val t_appl : Funct.t IList.t -> Funct.t -> t
    val t_curry : t -> t
    val t_assert : t -> t -> Location.t -> t
    val t_fail : Funct.t -> Funct.t -> Location.t -> t
    val t_fix : t -> t
    val tl_fix : t -> t
  end


(* hash-consing transformations is costly, but worthwhile: *)
module type PairFTrans = (* more concrete than DeFTrans *)
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Location : Location

    type t = t' Hashcons.hash_consed
    and t' =
      [`T_ID of Cat.t
      |`T_COMP of t * t
      |`T_PR of Cat.t IList.t * IdIndex.t
      |`T_RECORD of Cat.t * t IList.t
      |`T_FT of Funct.t * t
      |`T_TF of t * Funct.t
      |`T_id of Funct.t
      |`T_comp of t * t
      |`T_pp of Cat.t * t IList.t
      |`T_pr of Funct.t IList.t * IdIndex.t
      |`T_record of Funct.t * t IList.t
      |`T_ss of Cat.t * t IList.t
      |`T_in of Funct.t IList.t * IdIndex.t
      |`T_case of t IList.t * Funct.t
      |`TL_case of Funct.t * t IList.t * Funct.t
      |`T_map of Funct.t * t
      |`T_ii of t
      |`T_con of Funct.t
      |`T_fold of Funct.t * t
      |`TL_fold of Funct.t * t
      |`T_de of Funct.t
      |`T_tt of t
      |`T_uncon of Funct.t
      |`T_unfold of Funct.t * t
      |`TL_unfold of Funct.t * t
      |`T_unde of Funct.t
      |`T_ee of t IList.t * t
      |`T_appl of Funct.t IList.t * Funct.t
      |`T_curry of t
      |`T_assert of t * t * Location.t
      |`T_fail of Funct.t * Funct.t * Location.t
      |`T_fix of t
      |`TL_fix of t]

    val t2de : t -> t'
    val t2stamp : t -> int
  end

module PairFTrans'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : T)
    (Funct : T)
    (Location : Location)
    : (PairFTrans
    with module IdIndex = IdIndex
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct
    with module Location = Location) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat = Cat
    module Funct = Funct
    module Location = Location

    type t = t' Hashcons.hash_consed
    and t' =
      [`T_ID of Cat.t
      |`T_COMP of t * t
      |`T_PR of Cat.t IList.t * IdIndex.t
      |`T_RECORD of Cat.t * t IList.t
      |`T_FT of Funct.t * t
      |`T_TF of t * Funct.t
      |`T_id of Funct.t
      |`T_comp of t * t
      |`T_pp of Cat.t * t IList.t
      |`T_pr of Funct.t IList.t * IdIndex.t
      |`T_record of Funct.t * t IList.t
      |`T_ss of Cat.t * t IList.t
      |`T_in of Funct.t IList.t * IdIndex.t
      |`T_case of t IList.t * Funct.t
      |`TL_case of Funct.t * t IList.t * Funct.t
      |`T_map of Funct.t * t
      |`T_ii of t
      |`T_con of Funct.t
      |`T_fold of Funct.t * t
      |`TL_fold of Funct.t * t
      |`T_de of Funct.t
      |`T_tt of t
      |`T_uncon of Funct.t
      |`T_unfold of Funct.t * t
      |`TL_unfold of Funct.t * t
      |`T_unde of Funct.t
      |`T_ee of t IList.t * t
      |`T_appl of Funct.t IList.t * Funct.t
      |`T_curry of t
      |`T_assert of t * t * Location.t
      |`T_fail of Funct.t * Funct.t * Location.t
      |`T_fix of t
      |`TL_fix of t]

    let t2de t = t.Hashcons.node
    let t2stamp n = n.Hashcons.hkey
  end

module PairFTrans = 
  PairFTrans' (IdIndex) (IList) (ACat) (AFunct) (Location)

module DeFTrans = PairFTrans

module ATrans = PairFTrans (* : T --- totally abstract transformations *)


module Trans2HC'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : FCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : DeFFunct 
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : PairFTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (EqFCat : EqFCat with type Cat.t = Cat.t)
    (EqFFunct : EqFFunct with type Funct.t = Funct.t)
    : (Hashcons.HashedType
    with type t = Trans.t') =
  struct
    type t = Trans.t'

    let eq_inner eq t u =
      match t, u with
      |`T_RECORD (c, lt),`T_RECORD (d, lu) -> 
	  EqFCat.eq c d && IList.eqset eq lt lu
      |`T_TF (t1, f2),`T_TF (u1, g2) -> EqFFunct.eq f2 g2 && eq t1 u1
      |`T_id f,`T_id g -> EqFFunct.eq f g
      |`T_comp (t1, t2),`T_comp (u1, u2) -> eq t1 u1 && eq t2 u2
      |`T_pr (lf, i),`T_pr (lg, j) -> 
	  IdIndex.eq i j && IList.eqset EqFFunct.eq lf lg
      |`T_record (f, lt),`T_record (g, lu) -> 
	  EqFFunct.eq f g && IList.eqset eq lt lu
      |`T_in (lf, i),`T_in (lg, j) ->
	  IdIndex.eq i j && IList.eqset EqFFunct.eq lf lg
      |`TL_case (f, lt, f2),`TL_case (g, lu, g2) -> 
	  EqFFunct.eq f g && EqFFunct.eq f2 g2 && IList.eqset eq lt lu
      |`T_appl (lf, f),`T_appl (lg, g) ->
	  EqFFunct.eq f g && IList.eqset EqFFunct.eq lf lg
      |`T_curry t,`T_curry u -> eq t u
      |`T_con f,`T_con g -> EqFFunct.eq f g
      |`TL_fold (f, t),`TL_fold (g, u) -> EqFFunct.eq f g && eq t u
         (* f is needed, because f inside t may be equated, e.g. by {} *)
      |`T_de f,`T_de g -> EqFFunct.eq f g
      |`T_uncon f,`T_uncon g -> EqFFunct.eq f g
      |`TL_unfold (f, t),`TL_unfold (g, u) -> EqFFunct.eq f g && eq t u
      |`T_unde f,`T_unde g -> EqFFunct.eq f g
      |`T_assert (t1, t2, l),`T_assert (u1, u2, k) -> 
	  eq t1 u1 && eq t2 u2  && Trans.Location.eq l k
      |`T_fail (f1, f2, l),`T_fail (g1, g2, k) ->
	  EqFFunct.eq f1 g1 && EqFFunct.eq f2 g2 && Trans.Location.eq l k
      |`TL_fix t,`TL_fix u -> eq t u
      (* complete for no-execution! *)
      | _, _ -> false

    let equal t u = eq_inner (==) t u 

    let hash_ult u lt =
      if IList.is_nil lt then u ()
      else
	IList.bfold 0
	  (fun (i, t) n -> 
	    let ni = IdIndex.stamp_t i in
	    let nt = Trans.t2stamp t in
	    n + (combine ni nt)) lt

    let hash_lf lf = (* lf always rigid inside transformations *)
      IList.bfold 0
	(fun (i, f) n -> 
	  let ni = IdIndex.stamp_t i in
	  let nf = Funct.t2stamp f in
	  n + (combine ni nf)) lf

    let hash_lfi lf i =
      combine (hash_lf lf) (IdIndex.stamp_t i)

    let hash t = abs
      (match t with
      |`T_ID c -> 3 * Cat.t2stamp c
      |`T_COMP (t, u) -> 5 * combine (Trans.t2stamp t) (Trans.t2stamp u)
      |`T_PR (lc, i) -> 
	  7 * combine (Cat.t2stamp (Cat.c_PP lc)) (IdIndex.stamp_t i)
      |`T_RECORD (c, lt) ->
	  11 * hash_ult (fun () -> Cat.t2stamp c) lt
      |`T_FT (f1, t2) -> 
	  13 * combine (Funct.t2stamp f1) (Trans.t2stamp t2)
      |`T_TF (t1, f2) -> 
	  17 * combine (Trans.t2stamp t1) (Funct.t2stamp f2)
      |`T_id g -> 19 * Funct.t2stamp g
      |`T_comp (t, u) -> 23 * combine (Trans.t2stamp t) (Trans.t2stamp u)
      |`T_pp (c, lt) -> 29 * hash_ult (fun () -> Cat.t2stamp c) lt
      |`T_pr (lf, i) -> 
	  31 * hash_lfi lf i
      |`T_record (f, lt) ->
	  37 * hash_ult (fun () -> Funct.t2stamp f) lt
      |`T_ss (c, lt) -> 41 * hash_ult (fun () -> Cat.t2stamp c) lt
      |`T_in (lf, i) ->
	  43 * hash_lfi lf i
      |`T_case (lt, h) -> 
	  47 * hash_ult (fun () -> Funct.t2stamp h) lt
      |`TL_case (f, lt, h) -> 
	  53 * hash_ult (fun () -> 
	    combine (Funct.t2stamp f) (Funct.t2stamp h)) lt
      |`T_map (g, t) -> 59 * combine (Funct.t2stamp g) (Trans.t2stamp t)
      |`T_ii t -> 61 * Trans.t2stamp t   
      |`T_con g -> 67 * Funct.t2stamp g
      |`T_fold (g, t) -> 71 * combine (Funct.t2stamp g) (Trans.t2stamp t)
      |`TL_fold (g, t) -> 73 * combine (Funct.t2stamp g) (Trans.t2stamp t)
      |`T_de g -> 79 * Funct.t2stamp g
      |`T_tt t -> 83 * Trans.t2stamp t   
      |`T_uncon g -> 89 * Funct.t2stamp g
      |`T_unfold (g, t) -> 97 * combine (Funct.t2stamp g) (Trans.t2stamp t)
      |`TL_unfold (g, t) -> 101 * combine (Funct.t2stamp g) (Trans.t2stamp t)
      |`T_unde g -> 103 * Funct.t2stamp g
      |`T_ee (lt, t) -> 
          107 * combine (Trans.t2stamp t) (hash_ult (fun () -> 7) lt)
      |`T_appl (lg, h) ->
	  109 * combine (Funct.t2stamp h) (hash_lf lg)
      |`T_curry t -> 113 * Trans.t2stamp t
      |`T_assert (u, t, l) -> 
	  127 * combine (combine (Trans.t2stamp u) (Trans.t2stamp t))
	    (Trans.Location.t2int l)
      |`T_fail (f, h, l) -> 
	  131 * combine (combine (Funct.t2stamp f) (Funct.t2stamp h)) 
	    (Trans.Location.t2int l)
      |`T_fix t -> 137 * Trans.t2stamp t
      |`TL_fix t -> 139 * Trans.t2stamp t)
  end

module Trans2HC = Trans2HC' (IdIndex) (IList) (FCat) (DeFFunct)
    (PairFTrans) (EqFCat) (EqFFunct)


module HashTrans = Hashcons.Make (Trans2HC)


module NFTransHashed'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : T)
    (Funct : T)
    (Location : Location)
    (PairFTrans : PairFTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with module Location = Location)
    (Trans2HC : Hashcons.HashedType with type t = PairFTrans.t')
    (HashTrans : Hashcons.S
    with type key = Trans2HC.t)
    : (FTrans
    with module IdIndex = IdIndex
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type t = PairFTrans.t
    with module Location = Location) =
  struct

    include PairFTrans

    let tbl = HashTrans.create good_hash_size

    let hashcons = HashTrans.hashcons tbl

    let t_ID c = hashcons (`T_ID c)
    let t_COMP t1 t2 = hashcons (`T_COMP (t1, t2))
    let t_PR lc i = hashcons (`T_PR (lc, i))
    let t_RECORD c lt = hashcons (`T_RECORD (c, lt))
    let t_FT f1 t2 = hashcons (`T_FT (f1, t2))
    let t_TF t1 f2 = hashcons (`T_TF (t1, f2))
    let t_id g = hashcons (`T_id g)
    let t_comp t u = hashcons (`T_comp (t, u))
    let t_pp c lt = hashcons (`T_pp (c, lt))
    let t_pr lf i = hashcons (`T_pr (lf, i))
    let t_record f lt = hashcons (`T_record (f, lt))
    let t_ss c lt = hashcons (`T_ss (c, lt))
    let t_in lf i = hashcons (`T_in (lf, i))
    let t_case lt h = hashcons (`T_case (lt, h))
    let tl_case f lt h = hashcons (`TL_case (f, lt, h))
    let t_map g t = hashcons (`T_map (g, t))
    let t_ii t = hashcons (`T_ii t)
    let t_con g = hashcons (`T_con g)
    let t_fold g t = hashcons (`T_fold (g, t))
    let tl_fold g t = hashcons (`TL_fold (g, t))
    let t_de g = hashcons (`T_de g)
    let t_tt t = hashcons (`T_tt t)
    let t_uncon g = hashcons (`T_uncon g)
    let t_unfold g t = hashcons (`T_unfold (g, t))
    let tl_unfold g t = hashcons (`TL_unfold (g, t))
    let t_unde g = hashcons (`T_unde g)
    let t_ee lt u = hashcons (`T_ee (lt, u))
    let t_appl lg h = hashcons (`T_appl (lg, h))
    let t_curry t = hashcons (`T_curry t)
    let t_assert u t l = hashcons (`T_assert (u, t, l))
    let t_fail f h l = hashcons (`T_fail (f, h, l))
    let t_fix t = hashcons (`T_fix t)
    let tl_fix t = hashcons (`TL_fix t)
  end

module NFTrans = NFTransHashed' (IdIndex) (IList) (ACat) (AFunct)
    (Location) (PairFTrans) (Trans2HC) (HashTrans)


(* this is used, when not hash-consing transformations: (* *)
module type PairFTrans = (* more concrete than DeFTrans *)
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Location : Location

    type t = t'
    and t' =
      [`T_ID of Cat.t
      |`T_COMP of t * t
      |`T_PR of Cat.t IList.t * IdIndex.t
      |`T_RECORD of Cat.t * t IList.t
      |`T_FT of Funct.t * t
      |`T_TF of t * Funct.t
      |`T_id of Funct.t
      |`T_comp of t * t
      |`T_pp of Cat.t * t IList.t
      |`T_pr of Funct.t IList.t * IdIndex.t
      |`T_record of Funct.t * t IList.t
      |`T_ss of Cat.t * t IList.t
      |`T_in of Funct.t IList.t * IdIndex.t
      |`T_case of t IList.t * Funct.t
      |`TL_case of Funct.t * t IList.t * Funct.t
      |`T_map of Funct.t * t
      |`T_ii of t
      |`T_con of Funct.t
      |`T_fold of Funct.t * t
      |`TL_fold of Funct.t * t
      |`T_de of Funct.t
      |`T_tt of t
      |`T_uncon of Funct.t
      |`T_unfold of Funct.t * t
      |`TL_unfold of Funct.t * t
      |`T_unde of Funct.t
      |`T_ee of t IList.t * t
      |`T_appl of Funct.t IList.t * Funct.t
      |`T_curry of t
      |`T_assert of t * t * Location.t
      |`T_fail of Funct.t * Funct.t * Location.t
      |`T_fix of t
      |`TL_fix of t]

    val t2de : t -> t'
    val t2stamp : t -> int
  end

module PairFTrans'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : T)
    (Funct : T)
    (Location : Location)
    : (PairFTrans
    with module IdIndex = IdIndex
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct
    with module Location = Location) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat = Cat
    module Funct = Funct
    module Location = Location

    type t = t'
    and t' =
      [`T_ID of Cat.t
      |`T_COMP of t * t
      |`T_PR of Cat.t IList.t * IdIndex.t
      |`T_RECORD of Cat.t * t IList.t
      |`T_FT of Funct.t * t
      |`T_TF of t * Funct.t
      |`T_id of Funct.t
      |`T_comp of t * t
      |`T_pp of Cat.t * t IList.t
      |`T_pr of Funct.t IList.t * IdIndex.t
      |`T_record of Funct.t * t IList.t
      |`T_ss of Cat.t * t IList.t
      |`T_in of Funct.t IList.t * IdIndex.t
      |`T_case of t IList.t * Funct.t
      |`TL_case of Funct.t * t IList.t * Funct.t
      |`T_map of Funct.t * t
      |`T_ii of t
      |`T_con of Funct.t
      |`T_fold of Funct.t * t
      |`TL_fold of Funct.t * t
      |`T_de of Funct.t
      |`T_tt of t
      |`T_uncon of Funct.t
      |`T_unfold of Funct.t * t
      |`TL_unfold of Funct.t * t
      |`T_unde of Funct.t
      |`T_ee of t IList.t * t
      |`T_appl of Funct.t IList.t * Funct.t
      |`T_curry of t
      |`T_assert of t * t * Location.t
      |`T_fail of Funct.t * Funct.t * Location.t
      |`T_fix of t
      |`TL_fix of t]

    let t2de t = t
    let t2stamp t = -1
  end

module PairFTrans = 
  PairFTrans' (IdIndex) (IList) (ACat) (AFunct) (Location)

module DeFTrans = PairFTrans

module ATrans = PairFTrans (* : T --- totally abstract transformations *)


module NFTransDummy'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : T)
    (Funct : T)
    (Location : Location)
    (PairFTrans : PairFTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with module Location = Location)
    : (FTrans
    with module IdIndex = IdIndex
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type t = PairFTrans.t
    with module Location = Location) =
  struct

    include PairFTrans

    let hashcons t = t

    let t_ID c = hashcons (`T_ID c)
    let t_COMP t1 t2 = hashcons (`T_COMP (t1, t2))
    let t_PR lc i = hashcons (`T_PR (lc, i))
    let t_RECORD c lt = hashcons (`T_RECORD (c, lt))
    let t_FT f1 t2 = hashcons (`T_FT (f1, t2))
    let t_TF t1 f2 = hashcons (`T_TF (t1, f2))
    let t_id g = hashcons (`T_id g)
    let t_comp t u = hashcons (`T_comp (t, u))
    let t_pp c lt = hashcons (`T_pp (c, lt))
    let t_pr lf i = hashcons (`T_pr (lf, i))
    let t_record f lt = hashcons (`T_record (f, lt))
    let t_ss c lt = hashcons (`T_ss (c, lt))
    let t_in lf i = hashcons (`T_in (lf, i))
    let t_case lt h = hashcons (`T_case (lt, h))
    let tl_case f lt h = hashcons (`TL_case (f, lt, h))
    let t_map g t = hashcons (`T_map (g, t))
    let t_ii t = hashcons (`T_ii t)
    let t_con g = hashcons (`T_con g)
    let t_fold g t = hashcons (`T_fold (g, t))
    let tl_fold g t = hashcons (`TL_fold (g, t))
    let t_de g = hashcons (`T_de g)
    let t_tt t = hashcons (`T_tt t)
    let t_uncon g = hashcons (`T_uncon g)
    let t_unfold g t = hashcons (`T_unfold (g, t))
    let tl_unfold g t = hashcons (`TL_unfold (g, t))
    let t_unde g = hashcons (`T_unde g)
    let t_ee lt u = hashcons (`T_ee (lt, u))
    let t_appl lg h = hashcons (`T_appl (lg, h))
    let t_curry t = hashcons (`T_curry t)
    let t_assert u t l = hashcons (`T_assert (u, t, l))
    let t_fail f h l = hashcons (`T_fail (f, h, l))
    let t_fix t = hashcons (`T_fix t)
    let tl_fix t = hashcons (`TL_fix t)
  end

module NFTrans = NFTransDummy' (IdIndex) (IList) (ACat) (AFunct)
     (Location) (PairFTrans)
*)


module type PpFTransTools =
  sig
    module Trans : DeFTrans
    module PpTools : PpTools
    val pp_t : PpTools.outt -> Trans.t -> unit
    val pp_lt : PpTools.outt -> Trans.t Trans.IList.t -> unit
  end

module PpFTransTools'
    (IdIndex : IdIndex) 
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (PpTools : PpTools
    with module IdIndex = IdIndex
    with module IList = IList)
    (Cat : DeFCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : DeFFunct 
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : DeFTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (PpFCatTools : PpFCatTools
    with module PpTools = PpTools
    with module Cat = Cat)
    (PpFFunctTools : PpFFunctTools
    with module PpTools = PpTools
    with module Funct = Funct)
    : (PpFTransTools
    with module Trans = Trans
    with module PpTools = PpTools) =
  struct
    module Trans = Trans
    module PpTools = PpTools

    let rec get_short_in (t : Trans.t) : IdIndex.t option =
      (match Trans.t2de t with
      |`T_comp (erec, tin) ->
          (match Trans.t2de erec with
          |`T_record (_, empty) when IList.is_nil empty ->
              (match Trans.t2de tin with
              |`T_in (_, op) -> Some op
              | _ -> None)
          | _ -> None)
      | _ -> None)

    let rec get_con (tt : Trans.t) : unit option =
      (match Trans.t2de tt with
      |`T_con _ -> Some ()
      |`T_uncon _ -> Some ()
      |`T_TF (t, f) -> get_con t
      | _ -> None)

    let rec get_list_length (tt : Trans.t) : int option =
      (match Trans.t2de tt with
      |`T_comp (t, conn) -> 
	  (match get_con conn with
	  | Some _ -> 
	      (match Trans.t2de t with
	      |`T_comp (record, tin) -> 
                  (match Trans.t2de tin with
                  |`T_in (_, op) ->
                      (match IdIndex.t2s op with
                      | "Nil" ->
                          (match Trans.t2de record with
                          |`T_record (_, empty) when IList.is_nil empty ->
                              Some 0
                          | _ -> None)
                      | "Cons" ->
			  (match Trans.t2de record with
			  |`T_record (f, lt) ->
			      (match IList.find_ok AtIndex.head lt with
			      |`OK _ -> 
				  (match IList.find_ok AtIndex.tail lt with
				  |`OK tl -> 
                                  (* we disregard other elements of [lt] *)
                                      (match get_list_length tl with
   				      | Some n -> Some (n+1)
				      | None -> None)
				  |`Error _ -> None)
			      |`Error _ -> None)
			  | _ ->  None)
		      | _ ->  None)
		  | _ ->  None)
	      | _ ->  None)
	  | None -> None)
      |_ -> None)

    let is_nonempty_list (tt : Trans.t) : bool =
      (match get_list_length tt with
      | Some 0 -> false
      | Some _ -> true
      | None -> false)

    let rec get_num (tt : Trans.t) : int option =
      (match Trans.t2de tt with
      |`T_comp (t, conn) ->
          (match get_con conn with
          | Some _ ->
              (match Trans.t2de t with
              |`T_comp (num, tin) ->
                  (match Trans.t2de tin with
                  |`T_in (_, op) ->
                      (match IdIndex.t2s op with
                      | "Zero" ->
                          (match Trans.t2de num with
                          |`T_record (_, empty) when IList.is_nil empty ->
                              Some 0
                          | _ -> None)
                      | "Succ" ->
                          (match get_num num with
                          | Some n -> Some (n+1)
                          | None -> None)
                      | _ -> None)
                  | _ -> None)
              | _ -> None)
          | None -> None)
      | _ -> None)

(* (* if not for the [t2de] fancy *)	
    let rec get_num_old (tt : Trans.t) : int option =
      (match Trans.t2de tt with
      | T_comp (T_comp (T_record (_, empty), T_in (_, zero)), T_con _) ->
          if IdIndex.t2s zero = "Zero" && IList.is_nil empty then Some 0
          else None
      | T_comp (T_comp (num, T_in (_, succ)), T_con _) ->
          if IdIndex.t2s succ = "Succ" then
            (match get_num num with
            | Some n -> Some (n + 1)
            | None -> None)
          else None
      | _ -> None)
*)

    module Pp = PpTools
    open PpFCatTools
    open PpFFunctTools

    let rec pp_lst (out : Pp.outt) (tt : Trans.t) =
      Pp.printstring out "[";
      pp_lst2 out tt true;
      Pp.printstring out "]";
    and pp_lst2 (out : Pp.outt) (tt : Trans.t) (first : bool) =
      (match Trans.t2de tt with
      |`T_comp (t, conn) -> 
	  (match Trans.t2de t with
	  |`T_comp (record, tin) -> 
	      (match Trans.t2de record with
              |`T_record (f, lt) ->
		  (match IList.find_ok AtIndex.head lt with
		  |`OK hd -> 
		      (match IList.find_ok AtIndex.tail lt with
		      |`OK tl ->
			  (if not first then Pp.printstring out ";");
			  pp_t out hd;
			  pp_lst2 out tl false
		      |`Error _ -> failwith "pp_lst2: wrong list")
		  |`Error _ -> ()) (* the end of the list *)
              | _ -> ())
          | _ -> ())
      |_ -> ())
    and pp_t (out : Pp.outt) (t : Trans.t) =
      (match get_num t with
      | Some n -> Pp.printstring out (string_of_int n)
      | None -> 
      (match get_short_in t with
      | Some i -> Pp.printbackquote out i
      | None -> 
      (match is_nonempty_list t with
      | true -> pp_lst out t
      | false ->
	  (match Trans.t2de t with
          |`T_ID c -> Pp.printraw "T_ID" pp_c out c
          |`T_COMP (t1, t2) -> Pp.printbinop "*" pp_t pp_t out t1 t2
          |`T_PR (lc, i) -> 
	      Pp.printraw2 "T_PR" pp_lc Pp.printindex out lc i
          |`T_RECORD (c, lt) -> Pp.printlist "<" ">" "=" pp_t out lt
          |`T_FT (f1, t2) -> Pp.printbinop "*" pp_f pp_t out f1 t2
          |`T_TF (t1, f2) -> Pp.printbinop "*" pp_t pp_f out t1 f2
          |`T_id g -> Pp.printlabel ":" pp_f out g
          |`T_comp (t, u) -> Pp.printbinop "." pp_t pp_t out t u
          |`T_pp (c, lt) -> Pp.printraw2 "T_pp" pp_c pp_lt out c lt
          |`T_pr (lf, i) -> Pp.printindex out i
          |`T_record (f, lt) -> Pp.printlist "{" "}" "=" pp_t out lt
          |`T_ss (c, lt) -> Pp.printraw2 "T_ss" pp_c pp_lt out c lt
          |`T_in (lf, i) -> Pp.printbackquote out i
          |`T_case (lt, h) -> Pp.printraw2 "T_case" pp_lt pp_f out lt h
          |`TL_case (f, lt, h) -> Pp.printlistbackquote "[" "]" pp_t out lt
          |`T_map (g, t) -> Pp.printraw2 "T_map" pp_f pp_t out g t
          |`T_ii t -> Pp.printraw "T_ii" pp_t out t
          |`T_con g -> Pp.printraw "T_con" pp_f out g
          |`T_fold (g, t) -> Pp.printraw2 "T_fold" pp_f pp_t out g t
          |`TL_fold (g, t) -> Pp.printraw2 "TL_fold" pp_f pp_t out g t
          |`T_de g -> Pp.printraw "T_de" pp_f out g
          |`T_tt t -> Pp.printraw "T_tt" pp_t out t
          |`T_uncon g -> Pp.printraw "T_uncon" pp_f out g
          |`T_unfold (g, t) -> Pp.printraw2 "T_unfold" pp_f pp_t out g t
          |`TL_unfold (g, t) -> Pp.printraw2 "TL_unfold" pp_f pp_t out g t
          |`T_unde g -> Pp.printraw "T_unde" pp_f out g
          |`T_ee (lt, u) -> Pp.printraw2 "T_ee" pp_lt pp_t out lt u
          |`T_appl (lg, h) -> Pp.printraw2 "T_appl" pp_lf pp_f out lg h
          |`T_curry t -> Pp.printraw "T_curry" pp_t out t
          |`T_fix t -> Pp.printraw "T_fix" pp_t out t
          |`TL_fix t -> Pp.printraw "TL_fix" pp_t out t
          |`T_assert (t, u, l) -> Pp.printraw2 "T_assert" pp_t pp_t out t u
          |`T_fail (f, g, l) -> Pp.printraw2 "T_fail" pp_f pp_f out f g))))
    and pp_lt (out : Pp.outt) (lt: Trans.t IList.t) =
      Pp.printlist "<" ">" "=" pp_t out lt
   end

module PpFTransTools = PpFTransTools' (IdIndex) (AtIndex) (IList) (PpTools)
    (DeFCat) (DeFFunct) (DeFTrans) (PpFCatTools) (PpFFunctTools)


module type PpFTrans =
  sig
    module Trans : DeFTrans
    val pp_t : Trans.t -> string
    val pp_lt : Trans.t Trans.IList.t -> string
  end

module PpFTrans'
    (IdIndex : IdIndex) 
    (IList : IList with type Index.t = IdIndex.t)
    (PpTools : PpTools
    with module IdIndex = IdIndex
    with module IList = IList)
    (Cat : DeFCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : DeFFunct 
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : DeFTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (PpFTransTools : PpFTransTools
    with module PpTools = PpTools
    with module Trans = Trans)
    : (PpFTrans
    with module Trans = Trans) =
  struct
    module Trans = Trans

    let pp_t = PpTools.pp2str PpFTransTools.pp_t
    let pp_lt = PpTools.pp2str PpFTransTools.pp_lt
   end

module PpFTrans = PpFTrans' (IdIndex) (IList) (PpTools) (DeFCat) (DeFFunct)
    (DeFTrans) (PpFTransTools) 


module type DomFCore =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Trans : T

    val dom : Trans.t -> Funct.t
    val cod : Trans.t -> Funct.t
  end

module DomFCore'
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : ConFCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : SemFFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : DeFTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (PpFFunct : PpFFunct
    with type Funct.t = Funct.t)
    (SrcFCore : SrcFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with type Funct.t = Funct.t)
    : (DomFCore
    with module IdIndex = IdIndex
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct
    with module Trans = Trans) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat = Cat
    module Funct = Funct
    module Trans = Trans
    open IList
    open Funct
    open SrcFCore

    let rec dom t = (* simplified by assuming no `F_ee in `T_TF *)
      match Trans.t2de t with
      |`T_ID c -> f_ID c
      |`T_COMP (t1, t2) -> f_COMP (dom t1) (dom t2) 
          (* if `F_ee allowed in `T_TF, this is equal to dom (T_TF (t1, f2)) *)
      |`T_PR (lc, i) -> f_PR lc i
      |`T_RECORD (c, lt) -> f_RECORD c (vmap dom lt)
      |`T_FT (f1, t2) -> f_COMP f1 (dom t2) 
          (* if `F_ee allowed in `T_TF, this remains OK *)
      |`T_TF (t1, f2) -> f_COMP (dom t1) f2 
          (* if `F_ee allowed, should be done by cases on f2 (cod t1) *)
      |`T_id g -> g
      |`T_comp (t1, t2) -> dom t1
      |`T_pp (c, lt) -> f_pp c (vmap dom lt)
      |`T_pr (lf, i) -> f_pp (src (find i lf)) lf
      |`T_record (f, lt) -> f
      |`T_ss (c, lt) -> f_ss c (vmap dom lt)
      |`T_in (lf, i) -> find i lf
      |`T_case (lt, h) -> f_ss (src h) (vmap dom lt)
      |`TL_case (f, lt, h) ->
	  let c = src f in
	  let fs = 
	    (match find_ok AtIndex.atc lt with
	    |`OK tc -> (* default case *)
		find AtIndex.atd (unpp (dom tc))
	    |`Error er -> 
		f_ss c (vmap (fun t -> find AtIndex.atd (unpp (dom t))) lt))
	  in
	  f_pp c (cof fs f)
      |`T_map (g, t) -> 
	  let lup = unpp (dom t) in
	  let atd = find AtIndex.atd lup in
	  let ate = find AtIndex.ate lup in
	  f_pp (src ate) (cof (paf atd g) ate)
      |`T_ii t -> f_ii (dom t)
      |`T_con g -> paf (f_ii g) g
      |`T_fold (g, t) -> f_ii g
      |`TL_fold (g, t) ->
	  let lup = unpp (dom t) in
	  let ate = find AtIndex.ate lup in
	  f_pp (src ate) (cof (f_ii g) ate)
      |`T_de g -> f_ii g
      |`T_tt t -> f_tt (dom t)
      |`T_uncon g -> paf (f_tt g) g
      |`T_unfold (g, t) -> dom t
      |`TL_unfold (g, t) -> dom t
      |`T_unde g -> f_tt g
      |`T_ee (lt, u) -> f_ee (vmap cod lt) (dom u)
      |`T_appl (lg, h) -> f_pp (src h) (cons (AtIndex.atu, f_ee lg h) lg)
      |`T_curry t -> find AtIndex.atu (unpp (dom t))
      |`T_assert (u, t, l) -> dom u
      |`T_fail (f, h, l) -> f
      |`T_fix t -> f_pp (src (dom t)) nil
      |`TL_fix t -> 
	  let lup = unpp (dom t) in
	  let ate = find AtIndex.ate lup in
	  ate
    and cod t =
      match Trans.t2de t with
       `T_ID c -> f_ID c
      |`T_COMP (t1, t2) -> f_COMP (cod t1) (cod t2)
      |`T_PR (lc, i) -> f_PR lc i
      |`T_RECORD (c, lt) -> f_RECORD c (vmap cod lt)
      |`T_FT (f1, t2) -> f_COMP f1 (cod t2)
      |`T_TF (t1, f2) -> f_COMP (cod t1) f2
      |`T_id g -> g
      |`T_comp (t1, t2) -> cod t2
      |`T_pp (c, lt) -> f_pp c (vmap cod lt)
      |`T_pr (lf, i) -> find i lf
      |`T_record (f, lt) -> f_pp (src f) (vmap cod lt)
      |`T_ss (c, lt) -> f_ss c (vmap cod lt)
      |`T_in (lf, i) -> f_ss (src (find i lf)) lf
      |`T_case (lt, h) -> h
      |`TL_case (f, lt, h) -> h
      |`T_map (g, t) -> paf (cod t) g
      |`T_ii t -> f_ii (cod t)
      |`T_con g -> f_ii g
      |`T_fold (g, t) -> cod t
      |`TL_fold (g, t) -> cod t
      |`T_de g -> paf (f_ii g) g
      |`T_tt t -> f_tt (cod t)
      |`T_uncon g -> f_tt g
      |`T_unfold (g, t) -> f_tt g
      |`TL_unfold (g, t) -> f_tt g
      |`T_unde g -> paf (f_tt g) g
      |`T_ee (lt, u) -> f_ee (vmap dom lt) (cod u)
      |`T_appl (lg, h) -> h
      |`T_curry t ->
	  let lf = unpp (dom t) in
	  let lg = remove AtIndex.atu lf in
	  f_ee lg (cod t)
      |`T_assert (u, t, l) -> cod t
      |`T_fail (f, h, l) -> h
      |`T_fix t -> cod t
      |`TL_fix t -> cod t
  end

module DomFCore = DomFCore' (IdIndex) (AtIndex) (IList) (ConFCat) (SemFFunct)
    (DeFTrans) (PpFFunct) (SrcFCore)


module type SemFTrans =
  sig

    include FTrans

    val no_execution : bool ref
    val no_fixpoints : bool ref
    val pat : Funct.t -> t -> t
    val tig : t ->  Funct.t -> t
    val coi : t -> t -> t IList.t (* a list of type argument and context *)
    val cof : t -> t -> t IList.t (* a list of value argument and context *)
    val t_TF_coco : t * t ->  Funct.t -> t
    val nf_TL_fix : t -> t

    module ErrorRepLib : ErrorRepLib
    with module Location = Location

    exception AssertionError of ErrorRepLib.error
    exception FailError of ErrorRepLib.error
  end

module SemFTrans'
    (Location : Location)
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : SemFCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : SemFFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (NFTrans : FTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with module Location = Location)
    (EqFCat : EqFCat with type Cat.t = Cat.t)
    (EqFFunct : EqFFunct with type Funct.t = Funct.t)
    (SrcFCore : SrcFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with type Funct.t = Funct.t)
    (PpFCat : PpFCat with type Cat.t = Cat.t)
    (PpFFunct : PpFFunct
    with type Funct.t = Funct.t)
    (PpFTrans : PpFTrans
    with type Trans.t = NFTrans.t)
    (DomFCore : DomFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = NFTrans.t)
    (PpFTrans : PpFTrans
    with type Trans.t = NFTrans.t)
    (ErrorRepLib : ErrorRepLib
    with module Location = Location)
    : (SemFTrans
    with module IdIndex = IdIndex
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type t = NFTrans.t
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib) =
  struct (* this module is way too large!!! *)
    module IdIndex = IdIndex
    module IList = IList
    module Cat' = Cat
    module Funct' = Funct
    open Cat
    open Funct
    open IList
    open SrcFCore
    open DomFCore
    module Cat = Cat'
    module Funct = Funct'
    module Location = Location
    module ErrorRepLib = ErrorRepLib

    exception AssertionError of ErrorRepLib.error
    exception FailError of ErrorRepLib.error

    let no_execution = ref false
    let no_fixpoints = ref false

    type t = NFTrans.t
    let t2de = NFTrans.t2de
    let t2stamp = NFTrans.t2stamp

    let coi atj atk = 
      IList.cons (AtIndex.atj, atj) (IList.cons (AtIndex.atk, atk) IList.nil)

    let cof atd ate = 
      IList.cons (AtIndex.atd, atd) (IList.cons (AtIndex.ate, ate) IList.nil)

(* if transformations are hash-consed, which is costly,
   then memoizing t_comp helps a lot, but not with memory :/ *)
    module HC = 
      struct
	type t = NFTrans.t * NFTrans.t
	type result = NFTrans.t
	let equal (t1, t2) (u1, u2) = 
	  t1 == u1 && t2 == u2
	let hash (t1, t2) = 
	  abs (combine (NFTrans.t2stamp t1) (NFTrans.t2stamp t2))
      end
    module Memocomp = Memoize' (HC)

    let tbl = Memocomp.create good_hash_size

    module HT = 
      struct
	type t = Funct.t * NFTrans.t
	type result = NFTrans.t
	let equal (f1, t2) (g1, u2) = 
	  f1 == g1 && t2 == u2
	let hash (f1, t2) =
	  abs (combine (Funct.t2stamp f1) (NFTrans.t2stamp t2))
      end
    module MemoFT = Memoize' (HT)

    let tbl_FT = MemoFT.create good_hash_size

    let rec t_FT_l f u =
      try 
	MemoFT.find tbl_FT (f, u)
      with Not_found ->
	assert (EqFCat.eq (trg f) (src (dom u)));
	let b = t_FT_l' f u in
	let _ = MemoFT.add tbl_FT (f, u) b in
	b
    and t_FT_l' f u =
      match NFTrans.t2de u with
      |`T_RECORD (d, lu) -> NFTrans.t_RECORD (src f) (vmap (t_FT_l f) lu)
      |`T_TF (u1, g2) -> NFTrans.t_TF (t_FT_l f u1) g2
      |`T_id g -> NFTrans.t_id (f_COMP f g)
      |`T_comp (u1, u2) -> NFTrans.t_comp (t_FT_l f u1) (t_FT_l f u2) (* NF? *)
      |`T_pr (lg, j) -> NFTrans.t_pr (vmap (f_COMP f) lg) j
      |`T_record (g, lu) -> NFTrans.t_record (f_COMP f g) (vmap (t_FT_l f) lu)
      |`T_in (lg, j) -> NFTrans.t_in (vmap (f_COMP f) lg) j
      |`TL_case (g1, lu, g2) -> 
	  NFTrans.tl_case (f_COMP f g1) (vmap (t_FT_l f) lu) (f_COMP f g2) 
      |`T_appl (lg, g) -> NFTrans.t_appl (vmap (f_COMP f) lg) (f_COMP f g)
      |`T_curry u1 -> NFTrans.t_curry (t_FT_l f u1)
      |`T_con g ->	
	  let fg = fig f g in
	  NFTrans.t_con fg
      |`TL_fold (g, u1) -> 
	  let fg = fig f g in
	  NFTrans.tl_fold fg (t_FT_l f u1)
      |`T_de g -> 
	  let fg = fig f g in
	  NFTrans.t_de fg
      |`T_uncon g ->	
	  let fg = fig f g in
	  NFTrans.t_uncon fg
      |`TL_unfold (g, u1) -> 
	  let fg = fig f g in
	  NFTrans.tl_unfold fg (t_FT_l f u1)
      |`T_unde g -> 
	  let fg = fig f g in
	  NFTrans.t_unde fg
      |`T_assert (u1, u2, l) -> NFTrans.t_assert (t_FT_l f u1) (t_FT_l f u2) l
      |`T_fail (g1, g2, l) -> NFTrans.t_fail (f_COMP f g1) (f_COMP f g2) l
      |`TL_fix u1 -> NFTrans.tl_fix (t_FT_l f u1)
      | _ -> failwith "SemFTrans.t_FT: _ "

    let congrats = 
      ": another normal form (or redex) discovered!\n" 
      ^	"Congratulations and please report "
      ^ "(once you check with the latest version).\n"
      ^ "Arguments: t(\n"
    let fail2congratulate case t u =
      failwith (case ^ congrats 
		^ PpFTrans.pp_t t 
		^ "\n), u(\n"
		^ PpFTrans.pp_t u
		^ ")")

    let rec pat f t = 
      let c = src f in
      let cid = coi f (f_ID c) in
      t_FT (f_RECORD c cid) t

    and tig t g =
      let ld = unPP (src g) in
      let atj = find AtIndex.atj ld in (* = trg g *)
      let cgf = Cat.coi atj (src (dom t)) in
      let prf = t_FT (f_PR cgf AtIndex.atk) t in
      let rep = t_RECORD (c_PP cgf) (coi (t_PR cgf AtIndex.atj) prf) in
      t_TF rep g

    and t_ID c =
      t_id (f_ID c)
    and t_COMP t u =
      failwith "not so simple, use multiplications instead"
(*    not [t_comp (t_TF t (dom u)) (t_FT (cod t) u)]
      nor [t_comp (t_FT (dom t) u) (t_TF t (cod u))] *)
    and t_PR lc i =
      t_id (f_PR lc i)
    and t_RECORD c lt = NFTrans.t_RECORD c lt
    and t_FT f u = if !no_execution then NFTrans.t_FT f u else
      t_FT_l f u
    and t_TF t g = if !no_execution then NFTrans.t_TF t g else
     (assert (EqFCat.eq (trg (dom t)) (src g));
      match Funct.t2de g with
      |`F_ID d -> failwith "SemFTrans.t_TF:`F_ID ";
	  t
      |`F_COMP (g1, g2) ->
	  t_TF (t_TF t g1) g2
      |`F_PR (ld, j) ->
	  (match NFTrans.t2de t with
	  |`T_RECORD (c, lt) -> find j lt
	  |`T_id f -> t_id (f_COMP f g)
	  |`T_comp (t1, t2) -> failwith "SemFTrans.t_TF:`T_comp ";
	      t_comp (t_TF t1 g) (t_TF t2 g)
      (* possible normal forms: *)
	  |`T_con _
	  |`T_de _
	  |`T_uncon _
	  |`T_unde _ 
	    -> NFTrans.t_TF t g
	  |`T_TF (t1, f2) -> NFTrans.t_TF t1 (f_COMP f2 g)
	  | _ -> fail2congratulate "SemFTrans.t_TF" t (t_id g))
      |`F_RECORD (d, lg) -> (* happens with eta-contraction, lg rigid *)
	  t_RECORD (src (dom t)) (vmap (fun g -> t_TF t g) lg)
      |`F_pp (d, lg) -> failwith "SemFTrans.t_TF:`F_pp ";
	  t_pp (src (dom t)) (vmap (fun g -> t_TF t g) lg)
      |`F_ss (d, lg) -> failwith "SemFTrans.t_TF:`F_ss ";
	  t_ss (src (dom t)) (vmap (fun g -> t_TF t g) lg)
      |`F_ii g -> failwith "SemFTrans.t_TF:`F_ii ";
	  t_ii (tig t g)
      |`F_tt g -> failwith "SemFTrans.t_TF:`F_tt ";
	  t_tt (tig t g)
      |`F_ee (lg, g) -> failwith "SemFTrans.t_TF:`F_ee "; 
	  t_ee (vmap (fun g -> t_TF t g) lg) (t_TF t g)
          (* warning! here dom (t * u) <> dom t * dom u
             moreover dom now depends on both dom and cod of t! *)
      |`F_finish _ -> failwith "SemFTrans.t_TF:`F_finish "
      |`F_var _ -> failwith "SemFTrans.t_TF:`F_var ")
    and t_id g = NFTrans.t_id g
    and t_comp t u = if !no_execution then NFTrans.t_comp t u else
      (if !Tools.debugging then flush stderr;
      try 
	Memocomp.find tbl (t, u)
      with Not_found ->
	let b = t_comp' t u in
	let _ = 
	  (match NFTrans.t2de b with
	  |`T_fail (_, _, l) -> 
	      (match NFTrans.t2de u with
	      |`T_fail _ -> ()
	      | _ ->
		  raise (FailError 
		    (ErrorRepLib.coreBackError#instance 
		       [ErrorRepLib.Loc l; 
			ErrorRepLib.Msg 
			  "Your program signaled a failure!"]))) 
	  | _ -> ())
	in
	let _ = Memocomp.add tbl (t, u) b in
	b)
    and t_comp' t u = 
      assert (Cat.isBB (trg (dom u))); (* true only for current Dule *)
      assert 
	(if not (EqFFunct.eq (cod t) (dom u)) then
	  failwith ("SemFTrans.t_comp: \n(cod t)(" 
		    ^ PpFFunct.pp_f (cod t) ^ ") <> \n(dom u)("
		    ^ PpFFunct.pp_f (dom u) ^ ")\nt("
		    ^ PpFTrans.pp_t t ^ ")\nu(" 
		    ^ PpFTrans.pp_t u ^ ")")
	else true);
      match NFTrans.t2de t, NFTrans.t2de u with
      |	_,`T_id d -> pri "4"; t  
      |`T_id c, _ -> pri "5"; u
      | _,`T_comp (u1, u2) -> pri "_"; (* left-associative *)
	  t_comp (t_comp t u1) u2
      |`T_record (f, lt),`T_pr (lg, j) -> pri "+";
	  find j lt
      |`T_record (f, lt),`T_appl (lg, g) ->
	  let tj = NFTrans.t2de (find AtIndex.atu lt) in
          (match tj with
	  |`T_comp (t1, t2) ->
	      (match NFTrans.t2de t2 with
              |`T_curry tc -> pri "=1";
		  let lu = remove AtIndex.atu lt in
		  let tr = t_record f (cons (AtIndex.atu, t1) lu) in
		  let tx = t_comp tr tc in
		  fix_fix tx (* leaving lazy area *)
	      | _ -> pri "-1"; NFTrans.t_comp t u)
          |`T_curry tc -> pri "=2"; 
              (* restrict this if [lt] projections? *)
	      let lu = remove AtIndex.atu lt in
	      let tr = t_record f (cons (AtIndex.atu, t_id f) lu) in
	      let tx = t_comp tr tc in
	      fix_fix tx (* leaving lazy area *)
          | _ -> pri "-7"; NFTrans.t_comp t u)
      |`T_record (f, lt),`TL_case (g1, lu, g2) ->
	  let atd = find AtIndex.atd lt in
	  let ate = find AtIndex.ate lt in
	  (match NFTrans.t2de atd with
	  |`T_comp (t1, t2) ->
	      (match NFTrans.t2de t2 with
	      |`T_in (lg, j) -> pri "s: ";
		  let tx = 
		    (match find_ok j lu with
		    |`OK u -> 
			let tr = t_record f (cof t1 ate) in
			t_comp tr u
		    |`Error er -> (* default case *)
			let u = find AtIndex.atc lu in
			let tr = t_record f (cof atd ate) in
			t_comp tr u)
		  in
		  fix_fix tx (* leaving lazy area *)
	      | _ -> pri "-8"; NFTrans.t_comp t u)
	  |`T_in (lf, i) -> failwith "solo t_in in s: "
	  | _ -> pri "-8"; NFTrans.t_comp t u) 
      |`T_record (f, lt),`TL_fold (g, u1) ->
	  let atd = find AtIndex.atd lt in
	  let ate = find AtIndex.ate lt in
	  (match NFTrans.t2de atd with
	  |`T_comp (t1, t2) ->
	      (match NFTrans.t2de t2 with	      
	      |`T_con g -> pri "i: ";
                  (* g : coi atj atk -> atj *)
                  (* u : atk -> atj *)
		  let ma = t_map g u in
                  (* ma : atk -> atj *)
		  let tgma = t_comp (t_record f (cof t1 ate)) ma in
		  let tr = t_record f (cof tgma ate) in
		  let tx = t_comp tr u1 in
		  fix_fix tx (* leaving lazy area *)
	      | _ -> pri "-0"; NFTrans.t_comp t u)
	  |`T_con f1 -> failwith "solo t_con in i: "
	  | _ -> pri "-0"; NFTrans.t_comp t u)
      | _,`T_assert (u1, u2, l) -> pri "a"; 
	  t_assert (t_comp t u1) (t_comp t u2) l
      | _,`T_fail (g1, g2, l) -> pri "l"; 
	  t_fail (dom t) g2 l
      | _,`T_record (g, lu) -> pri "r"; 
	  t_record (dom t) (vmap (t_comp t) lu)
      |`T_con f,`T_de g -> 
	  failwith "solo t_con"; t_id (paf (f_ii f) f)
      |`T_uncon f,`T_unde g -> 
	  failwith "solo t_unde"; t_id (paf (f_tt f) f)
      |`TL_unfold (f, t3),`T_unde g -> failwith "solo TL_unfold, T_unde" 
      |`T_de f,`T_con g -> failwith "solo t_de t_con"
      |`T_unde f,`T_uncon g -> failwith "solo t_unde t_uncon"
      |`T_TF (t1, f1),`T_TF (u1, g1) ->
	  if EqFFunct.eq f1 g1 then
	    (match NFTrans.t2de t1, NFTrans.t2de u1 with
	    |`T_con f,`T_de g -> failwith "duo t_COMP t_con"
	    |`T_uncon f,`T_unde g -> failwith "duo t_COMP t_uncon"
	    |`TL_unfold _,`T_unde _ -> failwith "duo `TL_unfold ,`T_unde"
	    |`T_de f,`T_con g -> failwith "duo t_de t_con"
	    |`T_unde f,`T_uncon g -> failwith "duo t_unde t_uncon"
	    | _,`T_de _
	    | _,`T_unde _
	    | _,`T_con _ 
	    | _,`T_uncon _
	      -> NFTrans.t_comp t u
	    | _ -> fail2congratulate "SemFTrans.t_comp-A" t1 u1)
	  else (failwith "f1 <> g1 for TFTF"; NFTrans.t_comp t u)
      |`T_comp (t1, t2),`T_de g ->
	  (match NFTrans.t2de t2 with	    
	  |`T_con f -> pri "~i "; t1
	  | _ -> NFTrans.t_comp t u)
      |`T_comp (t1, t2),`T_unde g ->
	  (match NFTrans.t2de t2 with	    
	  |`T_uncon f -> pri "~i "; t1
	  |`TL_unfold (f, t3) ->
              (* g = f : coi atj atk -> atj *)
              (* t2 : atk -> atj *)
              (* t2 : f_pp atk (cof atd ate) -> f_tt f *)
	      let ma = t_map f t2 in
              (* ma : atk -> atj *)
              (* ma : f_pp atk (cof (paf atd f) ate) -> paf (f_tt f) f *)
	      let fd = dom t3 in
	      let lfg = unpp fd in
	      let tr = t_record fd (cof t3 (t_pr lfg AtIndex.ate)) in
              (* tr : f_pp atk (cof atd ate) -> paf atd f *)
	      let tx = t_comp (t_comp t1 tr) ma in
	      fix_fix tx (* leaving lazy area *)
	  | _ -> NFTrans.t_comp t u)
      |`T_comp (t1, t2),`T_TF (u1, g1) ->
	  (match NFTrans.t2de t2 with	    
	  |`T_TF (t3, f1) ->
	      if EqFFunct.eq f1 g1 then
		(match NFTrans.t2de t3, NFTrans.t2de u1 with	    
		|`T_con f,`T_de g -> t1
		|`T_uncon f,`T_unde g -> t1
		|`TL_unfold _,`T_unde _ -> failwith "tres `TL_unfold ,`T_unde"
		|`T_de f,`T_con g -> t1
		|`T_unde f,`T_uncon g -> failwith "tres t_unde t_uncon"
		| _,`T_de _
		| _,`T_unde _
		| _,`T_con _ 
		| _,`T_uncon _
		  -> NFTrans.t_comp t u
		| _ -> fail2congratulate "SemFTrans.t_comp-B" t3 u1)
	      else (failwith "f1 <> g1 for compTF"; NFTrans.t_comp t u)
	  | _ ->
	      (match NFTrans.t2de t2, NFTrans.t2de u1 with	    
	      | _,`T_de _
	      | _,`T_unde _
	      | _,`T_con _ 
	      | _,`T_uncon _
		-> NFTrans.t_comp t u
	      | _ -> fail2congratulate "SemFTrans.t_comp-C" t2 u1))
      (* possible normal forms: *)
      | _,`T_de _
      | _,`T_unde _
      | _,`T_con _ 
      | _,`T_uncon _
      | _,`T_TF _
      | _,`T_pr _
      | _,`T_in _
      |`T_record _,`TL_unfold _
      | _,`T_curry _
      | _,`TL_fix _
      (* as well as:
      |`T_record _,`T_appl _
      |`T_record _,`TL_case _
      |`T_record _,`TL_fold _ *)
	-> NFTrans.t_comp t u
      | _, _ ->	fail2congratulate "SemFTrans.t_comp-M" t u
    and t_pp c lt = if !no_execution then NFTrans.t_pp c lt else
      let lf = vmap dom lt in
      let lt = bmap (fun (i, t) -> t_comp (t_pr lf i) t) lt  in
      t_record (f_pp c lf) lt
    and t_pr lf i = NFTrans.t_pr lf i
    and t_record f lt = NFTrans.t_record f lt
    and t_ss c lt = if !no_execution then NFTrans.t_ss c lt else
      let lf = vmap cod lt in
      let lt = bmap (fun (i, t) -> t_comp t (t_in lf i)) lt  in
      t_case lt (f_ss c lf)
    and t_in lf i = NFTrans.t_in lf i
    and t_case lt h = if !no_execution then NFTrans.t_case lt h else
      let c = src h in
      let nof = f_pp c nil in
      let lf = vmap dom lt in
      let prof i = t_pr (cof (find i lf) nof) AtIndex.atd in
      let lap = bmap (fun (i, t) -> t_comp (prof i) t) lt in
      let casp = tl_case nof lap h in
      let fsf = f_ss c lf in
      let redt = t_record fsf (cof (t_id fsf) (t_record fsf nil)) in
      t_comp redt casp    
    and tl_case f lt h = NFTrans.tl_case f lt h
    and t_map f t = if !no_execution then NFTrans.t_map f t else
      match Funct.t2de f with (* f in normal form! *)
      |`F_ID c -> failwith "`F_ID "
      |`F_COMP (f1, f2) ->
	  (match Funct.t2de f2 with
	  |`F_PR (ld, j) -> pri "`F_COMP ";
(* [f] in normal form so [f1] cannot be *)
(* [F_ID], [F_RECORD], [F_pp], [F_ss], [F_ee] *)
(* so [f] is built from [F_COMP] and [F_PR], with first [F_PR] not [atj] *)
(* and from [F_ii] or [F_tt] but we ignore these cases for now *)
	      assert (* FIXME!!! *)
		(let rec allpr f = 
		  match Funct.t2de f with
		  |`F_PR _ -> true
		  |`F_COMP (f1, f2) -> allpr f1 && allpr f2
		  |`F_ii _ -> true
		  |`F_tt _ -> true
		  | _ -> false
		in allpr f1);
	      let cfg = unpp (dom t) in
(* not needed:let atd = find AtIndex.atd cfg in *)
	      let ate = find AtIndex.ate cfg in
(* besides, because first [F_PR] is not [atj], [paf atd f = paf ate f] *)
(* but not [paf atd f = f2], because [f1] may be composition *)
(* so this is wrong: t_pr (cof f2 ate) AtIndex.atd *)
              t_pr (cof (paf ate f) ate) AtIndex.atd 
	  | _ -> failwith "`F_COMP ")
      |`F_PR (lc, i)  when IdIndex.eq i AtIndex.atk -> pri "`F_PR2 ";
	  let cfg = unpp (dom t) in
	  let atd = find AtIndex.atd cfg in
	  let ate = find AtIndex.ate cfg in
	  t_pr (cof (paf atd f) ate) AtIndex.atd
(* besides, [paf atd f = f_ID c] *)
      |`F_PR (lc, i) -> pri "`F_PR1 "; 
	  assert (IdIndex.eq i AtIndex.atj); 
	  t
      |`F_RECORD (c, lf) -> failwith "`F_RECORD "
      |`F_pp (c, lf) -> pri "`p "; 
	  let cfg = unpp (dom t) in
	  let atd = find AtIndex.atd cfg in
	  let ate = find AtIndex.ate cfg in
	  let c = src ate in
	  let lpf = vmap (fun f -> paf atd f) lf in
	  let cfg = cof (f_pp c lpf) ate in
	  let pfg = f_pp c cfg in
	  let praf = t_pr cfg AtIndex.atd in
	  let pipi i = t_comp praf (t_pr lpf i) in
	  let prag = t_pr cfg AtIndex.ate in
	  let pa i = t_record pfg (cof (pipi i) prag) in
	  let lt = vmap (fun f -> t_map f t) lf in
	  let lt = bmap (fun (i, t) -> t_comp (pa i) t) lt in
	  t_record pfg lt
      |`F_ss (c, lf) -> pri "`s ";
	  let cfg = unpp (dom t) in
	  let ate = find AtIndex.ate cfg in
	  let c = src ate in
	  let cdt = cod t in
	  let lpf = vmap (fun f -> paf cdt f) lf in
	  let lt = vmap (fun f -> t_map f t) lf in
	  let lt = bmap (fun (i, t) -> t_comp t (t_in lpf i)) lt in
	  tl_case ate lt (f_ss c lpf)
      |`F_ii f1 -> pri "`i ";
	  let g = cod t in
	  let gf = paf g f in
	  let atd = find AtIndex.atd (unpp (dom t)) in
	  let aff = paf atd f in
          (* t : f_pp atk (cof atd ate) -> g *)
          (* t_map f t : f_pp atk (cof aff ate) -> gf *)
	  let atk = src g in
	  let atj = trg f1 in
          (* g : atk -> atj *)
	  let lc = coi atj atk in
	  let c = c_PP lc in
	  let ld = coi atj c in
 	  let d = c_PP ld in
          (* f : c -> atj *)
          (* f1 : d -> atj *)
	  let pri = f_PR ld AtIndex.atj in
	  let prj = f_PR ld AtIndex.atk in
	  let prji = f_COMP prj (f_PR lc AtIndex.atj) in
	  let prjj = f_COMP prj (f_PR lc AtIndex.atk) in
	  let rr = f_RECORD d (coi prji (f_RECORD d (coi pri prjj))) in
	  let fr = f_COMP rr f1 in
	  let prc = f_PR lc AtIndex.atk in
	  let tp = t_FT prc t in
	  let mtp = t_map fr tp in
	  let rg = f_RECORD atk (coi gf (f_ID atk)) in
	  let mt = t_FT rg mtp in
          (* mt : rg . f_pp c (cof (paf prc.atd fr) prc.ate) *)
	  (* -> rg . paf prc.g fr *)
          (* mt : f_pp atk (cof (paf atd rgfr) ate -> paf g rgfr *)
	  (* gj = f_COMP (f_PR lc AtIndex.atk) gf *)
          (* rgfr = f_COMP (f_RECORD (coi (f_PR lc AtIndex.atj) prc.rg)) fr =*)
          (* f_COMP (f_RECORD (coi gj f_ID)) f1 *)
          (* paf g rgfr = f_COMP (f_RECORD (coi g f_ID)) rgfr = *)
          (* f_COMP (f_RECORD (coi g f_ID)) verte! *)
          (* (f_COMP (f_RECORD (coi gj f_ID)) f1) = *)
          (* f_COMP (f_RECORD (coi gf rd)) f1 *)
          (* where rd = f_RECORD (coi g f_ID) *)
          (* gf = f_COMP (f_RECORD (coi g f_ID)) f = *)
          (* f_COMP (f_RECORD (coi g f_ID)) (f_ii f1) = *)
          (* f_ii (fig (f_RECORD (coi g f_ID)) f1) = *)
          (* f_ii (f_COMP (f_RECORD (coi f_PRatj (f_COMP f_PRatk rd))) f1) *)
          (* paf gf (unii gf) = f_COMP (f_RECORD (coi gf f_ID)) (unii gf) = *)
          (* f_COMP (f_RECORD (coi gf rd)) f1 *)
          (* paf g rgfr = paf gf (unii gf) *)
          (* aff = f_COMP (f_RECORD (coi atd f_ID)) f = *)
          (* f_ii (f_COMP (f_RECORD (coi f_PRatj (f_COMP f_PRatk rf))) f1) = *)
          (* where rf = f_RECORD (coi atd f_ID) *)
          (* paf gf uf = f_COMP (f_RECORD (coi gf f_ID)) uf = *)
          (* f_COMP (f_RECORD (coi gf rf)) f1 *)
          (* paf atd rgfr = paf gf uf *)
          (* mt : f_pp atk (cof (paf gf uf) ate -> paf gf (unii gf) *)
	  let cui = t_con (unii gf) in
          (* cui : paf gf (unii gf) -> gf *)
	  let tc = t_comp mt cui in
	  let uf = unii aff in
          (* tc : f_pp atk (cof (paf gf uf) ate -> gf *)
	  tl_fold uf tc
          (* tl_fold uf tc : f_pp atk (cof aff ate) -> gf *)
      |`F_tt f1 -> pri "`t ";
	  let g = cod t in
	  let gf = paf g f in
	  let unp = unpp (dom t) in
	  let atd = find AtIndex.atd unp in
	  let ate = find AtIndex.ate unp in
	  let aff = paf atd f in
          (* t : f_pp atk (cof atd ate) -> g *)
          (* t_map f t : f_pp atk (cof aff ate) -> gf *)
	  let atk = src g in
	  let atj = trg f1 in
          (* g : atk -> atj *)
	  let lc = coi atj atk in
	  let c = c_PP lc in
	  let ld = coi atj c in
 	  let d = c_PP ld in
          (* f : c -> atj *)
          (* f1 : d -> atj *)
	  let pri = f_PR ld AtIndex.atj in
	  let prj = f_PR ld AtIndex.atk in
	  let prji = f_COMP prj (f_PR lc AtIndex.atj) in
	  let prjj = f_COMP prj (f_PR lc AtIndex.atk) in
	  let rr = f_RECORD d (coi prji (f_RECORD d (coi pri prjj))) in
	  let fr = f_COMP rr f1 in
	  let prc = f_PR lc AtIndex.atk in
	  let tp = t_FT prc t in
	  let mtp = t_map fr tp in
	  let rg = f_RECORD atk (coi aff (f_ID atk)) in
	  let mt = t_FT rg mtp in
          (* mt : rg . f_pp c (cof (paf prc.atd fr) prc.ate) *)
	  (* -> rg . paf prc.g fr *)
          (* mt : f_pp atk (cof (paf atd rgfr) ate -> paf g rgfr *)
	  (* gj = f_COMP (f_PR lc AtIndex.atk) aff *)
          (* rgfr = f_COMP (f_RECORD (coi (f_PR lc AtIndex.atj) prc.rg)) fr =*)
          (* f_COMP (f_RECORD (coi gj f_ID)) f1 *)
          (* paf g rgfr = f_COMP (f_RECORD (coi g f_ID)) rgfr = *)
          (* f_COMP (f_RECORD (coi g f_ID)) verte! *)
          (* (f_COMP (f_RECORD (coi gj f_ID)) f1) = *)
          (* f_COMP (f_RECORD (coi aff rd)) f1 *)
          (* where rd = f_RECORD (coi g f_ID) *)
          (* gf = f_COMP (f_RECORD (coi g f_ID)) f = *)
          (* f_COMP (f_RECORD (coi g f_ID)) (f_tt f1) = *)
          (* f_tt (fig (f_RECORD (coi g f_ID)) f1) = *)
          (* f_tt (f_COMP (f_RECORD (coi f_PRatj (f_COMP f_PRatk rd))) f1) *)
          (* paf aff (untt gf) = *)
          (* f_COMP (f_RECORD (coi aff f_ID)) (untt gf) = *)
          (* f_COMP (f_RECORD (coi aff rd)) f1 *)
          (* paf g rgfr = paf aff (untt gf) *)
          (* aff = f_COMP (f_RECORD (coi atd f_ID)) f = *)
          (* f_tt (f_COMP (f_RECORD (coi f_PRatj (f_COMP f_PRatk rf))) f1) = *)
          (* where rf = f_RECORD (coi atd f_ID) *)
          (* paf aff (untt aff) = *)
          (* f_COMP (f_RECORD (coi aff f_ID)) (untt aff) = *)
          (* f_COMP (f_RECORD (coi aff rd)) f1 *)
          (* paf atd rgfr = paf aff (untt aff) *)
          (* mt : f_pp atk (cof (paf aff (untt aff))) ate -> paf aff uf *)
	  let cut = t_unde (untt aff) in
          (* cut : aff  -> paf aff (untt aff) *)
	  let lag = cof aff ate in
	  let pag = f_pp atk lag in
	  let put = t_comp (t_pr lag AtIndex.atd) cut in
	  let rag = t_record pag (cof put (t_pr lag AtIndex.ate)) in
	  let tc = t_comp rag mt in
	  let uf = untt gf in
          (* tc :  f_pp atk (cof aff ate) -> paf aff (untt gf) *)
	  tl_unfold uf tc
          (* tl_unfold uf tc : f_pp atk (cof aff ate) -> gf *)
      |`F_ee (lf, f) -> failwith "`e "
      |`F_finish f -> failwith "`f "
      |`F_var (var_num, c) -> failwith "'v "
    and t_ii t = if !no_execution then NFTrans.t_ii t else
      let h = cod t in
      let pa = pat (f_ii h) t in
      let paco = t_comp pa (t_con h) in
      t_fold (dom t) paco
    and t_con f = NFTrans.t_con f
    and t_fold f t = if !no_execution then NFTrans.t_fold f t else
      let g = dom t in
      let c = src g in
      let nof = f_pp c nil in
      let ap = t_comp (t_pr (cof g nof) AtIndex.atd) t in
      let foap = tl_fold f ap in
      let fsf = f_ii f in
      let redt = t_record fsf (cof (t_id fsf) (t_record fsf nil)) in
      t_comp redt foap
    and tl_fold f t = NFTrans.tl_fold f t
    and t_de f = NFTrans.t_de f (* = t_fold f (paf (t_con f) f) *)
    and t_tt t = if !no_execution then NFTrans.t_tt t else
      let f = dom t in
      let pa = pat (f_tt f) t in
      let paco = t_comp (t_unde f) pa in
      t_unfold (cod t) paco
    and t_uncon f = NFTrans.t_uncon f (* = t_unfold f (paf (t_unde f) f) *)
    and t_unfold f t = if !no_execution then NFTrans.t_unfold f t else
      let g = dom t in
      let c = src g in
      let nof = f_pp c nil in
      let ap = t_comp (t_pr (cof g nof) AtIndex.atd) t in
      let foap = tl_unfold f ap in
      let fsf = g in
      let redt = t_record fsf (cof (t_id fsf) (t_record fsf nil)) in
      t_comp redt foap
    and tl_unfold f t = NFTrans.tl_unfold f t
    and t_unde f = NFTrans.t_unde f
    and t_ee lt t = if !no_execution then NFTrans.t_ee lt t else
      let f = dom t in
      let c = src f in
      let lf = vmap dom lt in
      let lh = vmap cod lt in
      let lfe = cons (AtIndex.atu, f_ee lh f) lf in
      let lt = bmap (fun (i, t) -> t_comp (t_pr lfe i) t) lt in
      let ltp = cons (AtIndex.atu, t_pr lfe AtIndex.atu) lt in
      let re = t_record (f_pp c lfe) ltp in
      let rap = t_comp re (t_appl lh f) in
      let apt = t_comp rap t in
      t_curry apt
    and t_appl lf f = NFTrans.t_appl lf f
    and t_curry t = 
      let out = NFTrans.t_curry t in
      match NFTrans.t2de t with (* so that | _ -> fail is possible *)
      |`T_fail (f, h, l) ->
	  let out = NFTrans.t_curry t in
	  t_fail (dom out) (cod out) l
      | _ -> out
    and t_assert u t l = if !no_execution then NFTrans.t_assert u t l else
      (match NFTrans.t2de u with
      |`T_comp (t1, t2) ->
	  (match NFTrans.t2de t2 with
	  |`T_in (lg, j) ->
	      if j = AtIndex.tt then t
	      else if j = AtIndex.ff then 
		raise (AssertionError 
		  (ErrorRepLib.coreBackError#instance 
		     [ErrorRepLib.Loc l; 
		      ErrorRepLib.Msg 
			"Your assertion failed in your program!"]))
	      else failwith "SemFTrans.t_assert: j <> tt && j <> ff"
	  | _ -> NFTrans.t_assert u t l)
      | _ -> NFTrans.t_assert u t l)
    and t_fail f h l = NFTrans.t_fail f h l
    and t_fix t = if !no_execution then NFTrans.t_fix t else
      let g = dom t in
      let c = src g in
      let nof = f_pp c nil in
      let ap = t_comp (t_pr (cof g nof) AtIndex.atd) t in
      tl_fix ap
    and tl_fix t1 = 
      fix_fix (NFTrans.tl_fix t1)
    and fix_fix t = 
      if !no_execution || !no_fixpoints then t else
      let (changed, u) = step_fix t in
      if changed then fix_fix u else t
    and un_fix t1 =
      let t = NFTrans.tl_fix t1 in
      let f = dom t in
      let idf = t_id f in
      let rti = t_record f (cof t idf) in
      t_comp rti t1
    and step_fix u = pri " step_fix ";
      (* invariant: if (cht, t) = step_fix u then (not cht) => t == u *)
      match NFTrans.t2de u with
      |`T_TF _
      |`T_id _
      |`T_pr _
      |`T_in _
      |`TL_case _
      |`T_con _
      |`TL_fold _
      |`T_de _
      |`T_uncon _
      |`TL_unfold _
      |`T_unde _
      |`T_appl _
      |`T_curry _ 
      |`T_fail _
	-> (* no sense going inside, since substitution does not go there *)
	  (false, u)
      |`T_comp (u1, u2) ->
	  (match NFTrans.t2de u1, NFTrans.t2de u2 with
	  |`T_record (f, lt),`TL_case _ (* this stops fixpoint unwinding *)
	  |`T_record (f, lt),`TL_fold _   (* these may have `TL_case inside *)
	  |`T_record (f, lt),`TL_unfold _ (* so we skip their substitution *)
	    ->
	      let atd = find AtIndex.atd lt in (* argument *)
	      let ate = find AtIndex.ate lt in (* substitution *)
	      let (changed, new_atd) = step_fix atd in
	      let r = t_record f (cof new_atd ate) in
	      (changed, t_comp r u2)
	  | _,`T_curry _ (* this consumes all substitution *)
	    ->
	      (false, u)
	  | _, _ ->
	      let (changed2, t2) = step_fix u2 in
	      if changed2 then (changed2, t_comp u1 t2)
	      else
		let (changed1, t1) = step_fix u1 in
		(changed1, t_comp t1 u2))
      |`T_record (g, lu) -> (* substitution goes inside, so we go, too *)
	  let (changed, lt) = 
	    bfold (false, nil)
	      (fun (i, u) (ch, lt) ->
		let (cht, t) = step_fix u in
		(ch || cht, cons (i, t) lt)) lu
	  in	  
	  (changed, t_record g lt)
      |`T_assert (u1, u2, l) -> (* check these all the time *)
	  let (changed1, t1) = step_fix u1 in
	  let (changed2, t2) = step_fix u2 in
	  (changed1 || changed2, t_assert t1 t2 l)
      |`TL_fix u1 ->  (* here we are *)
	  (true, un_fix u1)
      | _ -> failwith "SemFTrans.step_fix: _ "

    let rec t_TF_coco (tc, td) f = 
    (* for [m_XInd_ordinary]
       to approximately simulate [t_TF tc f]
       [f] in normal form *)
      let result = t_TF_coco' (tc, td) f in
      assert 
	(let dtc = dom tc in
	let dtd = dom td in
        (* [tc] is the inverse of [td], so in particular: *)
	EqFFunct.eq dtc (cod td) &&
	EqFFunct.eq dtd (cod tc) &&
        (* we simulate [t_TF tc f], but with simpler typing: *)
        EqFFunct.eq (dom result) (f_COMP dtc f) &&
        EqFFunct.eq (cod result) (f_COMP dtd f)
	); 
      result
    and t_TF_coco' ((tc, td) as tctd) f = 
      let d = src (dom tc) in
      match Funct.t2de f with
      |`F_ID c -> failwith "SemFTrans.t_TF_coco:`F_ID "; tc
      |`F_COMP (f1, f2) ->
	  (match Funct.t2de f2 with
	  |`F_PR (lc, i) -> pri "F_COMP ";
              let ad = t_TF_coco tctd f1 in
	      t_TF ad f2
	  | _ -> failwith "SemFTrans.t_TF_coco:`F_COMP ")
      |`F_PR (lc, i) -> pri "F_PR ";
	  t_TF tc f
      |`F_RECORD (c, lf) -> failwith "SemFTrans.t_TF_coco:`F_RECORD ";
	  let lad = vmap (t_TF_coco tctd) lf in
	  t_RECORD d lad
      |`F_pp (c, lf) -> pri "RR "; 
	  let lad = vmap (t_TF_coco tctd) lf in
	  t_pp d lad
      |`F_ss (c, lf) -> pri "SS ";
	  let lad = vmap (t_TF_coco tctd) lf in
	  t_ss d lad
      |`F_ii f1 -> pri "II ";
	  let b = find AtIndex.atj (unPP (src f1)) in
	  let ci = coi b d in
	  let pj = f_PR ci AtIndex.atk in
	  let ptc = t_FT pj tc in
	  let ptd = t_FT pj td in
	  let idpi = t_id (f_PR ci AtIndex.atj) in
	  let cp = c_PP ci in
	  let ntc = t_RECORD cp (coi idpi ptc) in
	  let ntd = t_RECORD cp (coi idpi ptd) in
	  let ad = t_TF_coco (ntc, ntd) f1 in
	  t_ii ad
      |`F_tt f1 -> pri "TT "; 
	  let b = find AtIndex.atj (unPP (src f1)) in
	  let ci = coi b d in
	  let pj = f_PR ci AtIndex.atk in
	  let ptc = t_FT pj tc in
	  let ptd = t_FT pj td in
	  let idpi = t_id (f_PR ci AtIndex.atj) in
	  let cp = c_PP ci in
	  let ntc = t_RECORD cp (coi idpi ptc) in
	  let ntd = t_RECORD cp (coi idpi ptd) in
	  let ad = t_TF_coco (ntc, ntd) f1 in
	  t_tt ad
      |`F_ee (lf, f) -> pri "EE ";
	  let lad = vmap (t_TF_coco (td(*!!!*), tc(*!!!*))) lf in
	  let ad = t_TF_coco tctd f in
	  t_ee lad ad
      |`F_finish f -> failwith "FF "
      |`F_var (var_num, c) -> failwith "VV "

    let nf_TL_fix t = NFTrans.tl_fix t
  end

module SemFTrans = SemFTrans' (Location) (IdIndex) (AtIndex) (IList) (SemFCat)
    (SemFFunct) (NFTrans) (EqFCat) (EqFFunct) (SrcFCore) (PpFCat) (PpFFunct)
    (PpFTrans) (DomFCore) (PpFTrans) (ErrorRepLib)

module FTrans = SemFTrans

module ConFTrans = FTrans


module type ElabFCore = (* used for verification only, see --verification *)
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Trans : T
    module FCat : T
    module FFunct : T
    module FTrans : T

    val el_funct : FFunct.t ->
      [`OK of Cat.t * Cat.t * Funct.t
      |`Error of string]
    val el_trans : FTrans.t ->
      [`OK of Cat.t * Cat.t * Funct.t * Funct.t * Trans.t
      |`Error of string]
  end

module ElabFCore'
    (Location : Location)
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : FCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : SemFFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : SemFTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with module Location = Location)
    (FFunct : DeFFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with module VarStamp = Funct.VarStamp)
    (FTrans : DeFTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = FFunct.t
    with module Location = Location)
    (PpFFunct : PpFFunct
    with type Funct.t = Funct.t)
    (OkError : OkError
    with module IdIndex = IdIndex
    with module IList = IList) 
    (EqFCat : EqFCat with type Cat.t = Cat.t)
    (EqFFunct : EqFFunct with type Funct.t = Funct.t)
    (FFunctEqFFunct : EqFFunct with type Funct.t = FFunct.t)
    : (ElabFCore
    with module IdIndex = IdIndex
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct
    with module Trans = Trans
    with module FCat = Cat
    with module FFunct = FFunct
    with module FTrans = FTrans) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat' = Cat
    module Funct' = Funct
    module Trans = Trans
    module FCat = Cat
    module FFunct = FFunct
    module FTrans = FTrans
    open IList
    open OkError
    open Cat
    open Funct
    open Trans
    open EqFCat
    open EqFFunct
    module Cat = Cat'
    module Funct = Funct'

    let el_F_nn = for_all3 EqFCat.eq c_BB

    let el_T_nn = for_all5 EqFCat.eq c_BB

    let ij_ok1 acf_ok =
      match acf_ok with
      |`OK (c, e, f) ->
	  (match Cat.t2de c with
	  |`C_PP lc ->
	      (match find_ok AtIndex.atj lc with
	      |`OK atj -> 
		  (match find_ok AtIndex.atk lc with
		  |`OK atk -> 
		      if is_nil 
			  (remove AtIndex.atj (remove AtIndex.atk lc)) then
			if EqFCat.eq e atj then
			  `OK (f, atj, atk)
			else `Error "ij_ok: e <> atj in "
		      else `Error "ij_ok: lc too large in "
		  |`Error er -> `Error (er ^ "ij_ok in "))
	      |`Error er -> `Error (er ^ "ij_ok in "))
	  | _ -> `Error "ij_ok: src f not PP in ")
      |`Error er -> `Error (er ^ ", in ")

    let el_funct el_funct (* for recursive memoization *) f =
      let ij_ok f = ij_ok1 (el_funct f) in
      match FFunct.t2de f with
      |`F_ID c ->
	  `OK (c, c, f_ID c)
      |`F_COMP (f1, f2) ->
	  (match el_funct f1 with
	  |`OK (c1, e1, f1) -> 
	      (match el_funct f2 with
	      |	`OK (c2, e2, f2) ->
		  if EqFCat.eq e1 c2 then
		    `OK (c1, e2, f_COMP f1 f2)
		  else `Error "e1 <> c2 in`F_COMP"
	      |	`Error er -> `Error er)
	  |`Error er -> `Error er)
      |`F_PR (lc, i) ->
	  (match find_ok i lc with
	  |`OK e -> `OK (c_PP lc, e, f_PR lc i)
	  |`Error er -> `Error (er ^ "F_PR"))
      |`F_RECORD (c, lf) ->
	  (match vmap3ok el_funct lf with
	  |`OK (lc, le, lf) -> 
	      if vforall (EqFCat.eq c) lc then
		`OK (c, c_PP le, f_RECORD c lf)
	      else `Error "not vforall lc in`F_RECORD"
	  |`Error er -> `Error er)
      |`F_pp (c, lf) ->
	  (match el_F_nn c (vmap3ok el_funct lf) with
	  |`OK (c, e, lf) -> `OK (c, e, f_pp c lf)
	  |`Error er -> `Error (er ^ "F_pp"))
      |`F_ss (c, lf) ->
	  (match el_F_nn c (vmap3ok el_funct lf) with
	  |`OK (c, e, lf) -> `OK (c, e, f_ss c lf)
	  |`Error er -> `Error (er ^ "F_ss"))
      |`F_ii f ->
	  (match ij_ok f with
	  |`OK (f, atj, atk) ->
	      `OK (atk, atj, f_ii f)
	  |`Error er -> `Error (er ^ "F_ii"))
      |`F_tt f ->
	  (match ij_ok f with
	  |`OK (f, atj, atk) ->
	      `OK (atk, atj, f_tt f)
	  |`Error er -> `Error (er ^ "F_tt"))
      |`F_ee (lf, f) ->
	  (match el_funct f with
	  |`OK (c, e, f) ->
	      (match el_F_nn c (vmap3ok el_funct lf) with
	      |	`OK (c, e', lf) ->
		  if EqFCat.eq e' e then
		    if not_in AtIndex.atu lf then
		      `OK (c, e, f_ee lf f)
		    else `Error "AtIndex.atu is in lf in`F_ee"
		  else `Error "e' <> e in`F_ee"
	      |`Error er -> `Error (er ^ "F_ee"))
	  |`Error er -> `Error er)
      |`F_finish f -> assert false
      |`F_var (var_num, c) -> assert false

    module HC = 
      struct
	type t = FFunct.t
	type result = [`OK of Cat.t * Cat.t * Funct.t
        	      |`Error of string]
	let equal f g = FFunctEqFFunct.eq f g
	let hash f = FFunct.t2stamp f
      end
    module MemoEF = Memoize' (HC)

    let tbl = MemoEF.create good_hash_size

    let el_funct = MemoEF.memorec tbl el_funct

    let ij_ok f = ij_ok1 (el_funct f)

    let fg_ok f =
      match Funct.t2de f with
      |`F_pp (c, lf) ->
	  (match find_ok AtIndex.atd lf with
	  |`OK atd -> 
	      (match find_ok AtIndex.ate lf with
	      |`OK ate -> 
		  if is_nil (remove AtIndex.atd (remove AtIndex.ate lf)) then
		    `OK (c, atd, ate)
		  else `Error "fg_ok: lf too large in "
	      |`Error er -> `Error (er ^ "fg_ok in "))
	  |`Error er -> `Error (er ^ "fg_ok in "))
      | _ -> `Error "fg_ok: f not f_pp in "

(* if transformations are hash-consed, this is faster: *)
    let rec el_trans el_trans t =
      match FTrans.t2de t with
      |`T_ID c ->
	  let g = f_ID c in
	  `OK (c, c, g, g, t_ID c)
      |`T_COMP (t1, t2) ->
	  (match el_trans t1 with
	  |`OK (c1, e1, f1, h1, t1) -> 
	      (match el_trans t2 with
	      |`OK (c2, e2, f2, h2, t2) ->
		  if EqFCat.eq e1 c2 then
		    `OK (c1, e2, f_COMP f1 f2, f_COMP h1 h2,
			t_COMP t1 t2)
		  else `Error "e1 <> c2 in`T_COMP"
	      |	`Error er -> `Error er)
	  |`Error er -> `Error er)
      |`T_FT (f1, t2) ->
	  (match el_funct f1 with
	  |`OK (c1, e1, f1) -> 
	      (match el_trans t2 with
	      |`OK (c2, e2, f2, h2, t2) ->
		  if EqFCat.eq e1 c2 then
		    `OK (c1, e2, f_COMP f1 f2, f_COMP f1 h2,
			t_FT f1 t2)
		  else `Error "e1 <> c2 in`T_FT"
	      |	`Error er -> `Error er)
	  |`Error er -> `Error er)
      |`T_TF (t1, f2) ->
	  (match el_trans t1 with
	  |`OK (c1, e1, f1, h1, t1) -> 
	      (match el_funct f2 with
	      |`OK (c2, e2, f2) ->
		  if EqFCat.eq e1 c2 then
		    `OK (c1, e2, f_COMP f1 f2, f_COMP h1 f2,
			t_TF t1 f2)
		  else `Error "e1 <> c2 in`T_TF"
	      |	`Error er -> `Error er)
	  |`Error er -> `Error er)
      |`T_PR (lc, i) ->
	  (match find_ok i lc with
	  |`OK e ->
	      let g = f_PR lc i in
	      `OK (c_PP lc, e, g, g, t_PR lc i)
	  |`Error er -> `Error (er ^ "T_PR"))
      |`T_RECORD (c, lt) ->
	  (match vmap5ok el_trans lt with
	  |`OK (lc, le, lf, lh, lt) -> 
	      if vforall (EqFCat.eq c) lc then
		`OK (c, c_PP le, f_RECORD c lf, f_RECORD c lh,
		   t_RECORD c lt)
	      else `Error "not vforall lc in`T_RECORD"
	  |`Error er -> `Error er)
      |`T_id f ->
	  (match el_funct f with
	  |`OK (c, e, f) ->
	      `OK (c, e, f, f, t_id f)
	  |`Error er -> `Error er)
      |`T_comp (t1, t2) ->
	  (match el_trans t1 with
	  |`OK (c1, e1, f1, h1, t1) ->
	      (match el_trans t2 with
	      |`OK (c2, e2, f2, h2, t2) ->
		  if EqFFunct.eq h1 f2 then
		    `OK (c1, e1, f1, h2, t_comp t1 t2)
		  else `Error ("h1(" ^ PpFFunct.pp_f h1 ^ ") <> f2("
		    ^ PpFFunct.pp_f f2 ^ ") in`T_comp")
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      |`T_pp (c, lt) ->
	  (match el_T_nn c (vmap5ok el_trans lt) with
	  |`OK (c, e, lf, lh, lt) -> `OK (c, e, f_pp c lf, f_pp c lh,
					 t_pp c lt)
	  |`Error er -> `Error (er ^ "T_pp"))
      |`T_pr (lf, i) ->
	  (match find_ok i lf with
	  |`OK h ->
	      (match el_funct h with
	      |`OK (c, e, h) ->
		  let lg = remove i lf in
		  (match el_F_nn c (vmap3ok el_funct lg) with
		  |`OK (c, e, lg) ->
		      let lf = cons (i, h) lg in
		      `OK (c, e, f_pp c lf, h,
			  t_pr lf i)			
		  |`Error er -> `Error (er ^ "T_pr"))
	      |`Error er -> `Error er)
	  |`Error er -> `Error (er ^ "T_pr"))
      |`T_record (f, lt) ->
	  (match el_funct f with
	  |`OK (c, e, f) ->
	      (match el_T_nn c (vmap5ok el_trans lt) with
	      |`OK (c, e, lf, lh, lt) -> 
		  if vforall (EqFFunct.eq f) lf then
		    `OK (c, e, f, f_pp c lh,
			t_record f lt)
		  else `Error "not vforall lf in`T_record"
	      |`Error er -> `Error (er ^ "T_record"))
	  |`Error er -> `Error er)
      |`T_ss (c, lt) ->
	  (match el_T_nn c (vmap5ok el_trans lt) with
	  |`OK (c, e, lf, lh, lt) -> `OK (c, e, f_ss c lf, f_ss c lh,
					 t_ss c lt)
	  |`Error er -> `Error (er ^ "T_ss"))
      |`T_in (lf, i) ->
	  (match find_ok i lf with
	  |`OK f ->
	      (match el_funct f with
	      |`OK (c, e, f) ->
		  let lg = remove i lf in
		  (match el_F_nn c (vmap3ok el_funct lg) with
		  |`OK (c, e, lg) ->
		      let lf = cons (i, f) lg in
		      `OK (c, e, f, f_ss c lf,
			  t_in lf i)			
		  |`Error er -> `Error (er ^ "T_in"))
	      |`Error er -> `Error er)
	  |`Error er -> `Error (er ^ "T_in"))
      |`T_case (lt, h) ->
	  (match el_funct h with
	  |`OK (c, e, h) ->
	      (match el_T_nn c (vmap5ok el_trans lt) with
	      |`OK (c, e, lf, lh, lt) -> 
		  if vforall (EqFFunct.eq h) lh then
		    `OK (c, e, f_ss c lf, h,
			t_case lt h)
		  else `Error "not vforall lh in`T_case"
	      |`Error er -> `Error (er ^ "T_case"))
	  |`Error er -> `Error er)
      |`TL_case (f1, lt, f2) ->
	  (match el_funct f1 with
	  |`OK (c1, e1, f1) ->
	      (match el_funct f2 with
	      |`OK (c2, e2, f2) ->
		  (match el_T_nn c1 (vmap5ok el_trans lt) with
		  |`OK (c, e, lf, lh, lt) -> 
		      if vforall (EqFFunct.eq f2) lh then
			(match vmap3ok fg_ok lf with
			|`OK (lc', lctf, lctg) ->
			    if vforall (EqFFunct.eq f1) lctg then
			      let dom = 
				(match find_ok AtIndex.atc lctf with
				|`OK fc -> (* default case *)
				    fc
				|`Error er -> 
				    f_ss c lctf)
			      in			      
			      `OK (c, e, f_pp c (Funct.cof dom f1), f2,
				   tl_case f1 lt f2)
			    else `Error "not vforall lctg in TL_case"
			|`Error er -> `Error (er ^ "TL_case"))
		      else `Error "not vforall lh in TL_case"
		  |`Error er -> `Error (er ^ "TL_case"))
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      |`T_map (f, t) -> (* FIXME:ensure that trg f = BB, somehow *)
	  (match ij_ok f with
	  |`OK (f, atj, atk) ->
	      (match el_trans t with
              (* [c'] is [BB] because of [pp] in [fg_ok] *)
	      |`OK (c', e', f', g, t) -> 
		  (match fg_ok f' with
		  |`OK (c', atd, ate) ->
		      if EqFCat.eq c' atk then
			if EqFCat.eq e' atj then
			  `OK (atk, atj, 
			       f_pp c' (Funct.cof (paf atd f) ate), paf g f, 
			       t_map f t)
			else `Error "e' <> atj in`T_map"
		      else `Error "c' <> atk in`T_map"
		  |`Error er -> `Error (er ^ "T_map"))
	      |`Error er -> `Error er)
	  |`Error er -> `Error (er ^ "T_map"))      
      |`T_ii t ->
	  (match el_trans t with
	  |`OK (c, e, f, h, t) ->
	      (match ij_ok1 (`OK (c, e, f)) with
	      |`OK (f, atj, atk) ->
		  `OK (atk, atj, f_ii f, f_ii h,
		      t_ii t)
	      |`Error er -> `Error (er ^ "T_ii"))
	  |`Error er -> `Error er)
      |`T_con f ->
	  (match ij_ok f with
	  |`OK (f, atj, atk) ->
	      let fii = f_ii f in
	      `OK (atk, atj, 
		  paf fii f, fii, 
		  t_con f)
	  |`Error er -> `Error (er ^ "T_con"))
      |`T_fold (f, t) ->
	  (match ij_ok f with
	  |`OK (f, atj, atk) ->
	      (match el_trans t with
	      |`OK (c', e', f', g, t) ->
		  if EqFCat.eq c' atk then
		    if EqFCat.eq e' atj then
		      let gf = paf g f in
		      if EqFFunct.eq f' gf then
			`OK (atk, atj, 
			    f_ii f, g, 
			    t_fold f t)
		      else `Error "f' <> gf in`T_fold"
		    else `Error "e' <> atj in`T_fold"
		  else `Error "c' <> atk in`T_fold"
	      |`Error er -> `Error er)  
	  |`Error er -> `Error (er ^ "T_fold"))
      |`TL_fold (f, t) ->
	  (match ij_ok f with
	  |`OK (f, atj, atk) ->
	      (match el_trans t with
	      |`OK (c', e', f', g, t) ->
		  (match fg_ok f' with
		  |`OK (c', atd, ate) ->
		      if EqFCat.eq c' atk then
			if EqFCat.eq e' atj then
			  let gf = paf g f in
			  if EqFFunct.eq atd gf then
			    `OK (atk, atj, 
				f_pp c' (Funct.cof (f_ii f) ate), g, 
				tl_fold f t)
			  else `Error "atd <> gf in TL_fold"
			else `Error "e' <> atj in TL_fold"
		      else `Error "c' <> atk in TL_fold"
		  |`Error er -> `Error (er ^ "TL_fold"))
	      |`Error er -> `Error er)
	  |`Error er -> `Error (er ^ "TL_fold"))
      |`T_de f ->
	  (match ij_ok f with
	  |`OK (f, atj, atk) ->
	      let fii = f_ii f in
	      `OK (atk, atj, 
		  fii, paf fii f, 
		  t_de f)
	  |`Error er -> `Error (er ^ "T_de"))
      |`T_tt t ->
	  (match el_trans t with
	  |`OK (c, e, f, h, t) ->
	      (match ij_ok1 (`OK (c, e, f)) with
	      |`OK (f, atj, atk) ->
		  `OK (atk, atj, f_tt f, f_tt h,
		      t_tt t)
	      |`Error er -> `Error (er ^ "T_tt"))
	  |`Error er -> `Error er)
      |`T_uncon f ->
	  (match ij_ok f with
	  |`OK (f, atj, atk) ->
	      let ftt = f_tt f in
	      `OK (atk, atj, 
		  paf ftt f, ftt, 
		  t_uncon f)
	  |`Error er -> `Error (er ^ "T_uncon"))
      |`T_unfold (f, t) ->
	  (match ij_ok f with
	  |`OK (f, atj, atk) ->
	      (match el_trans t with
	      |`OK (c', e', f', g, t) ->
		  if EqFCat.eq c' atk then
		    if EqFCat.eq e' atj then
		      let gf = paf f' f in
		      if EqFFunct.eq g gf then
			`OK (atk, atj, 
			    f', f_tt f, 
			    t_unfold f t)
		      else `Error "g <> gf in`T_unfold"
		    else `Error "e' <> atj in`T_unfold"
		  else `Error "c' <> atk in`T_unfold"
	      |`Error er -> `Error er)  
	  |`Error er -> `Error (er ^ "T_unfold"))  
      |`TL_unfold (f, t) ->
	  (match ij_ok f with
	  |`OK (f, atj, atk) ->
	      (match el_trans t with
	      |`OK (c', e', f', g, t) ->
		  (match fg_ok f' with
		  |`OK (c', atd, ate) ->
		      if EqFCat.eq c' atk then
			if EqFCat.eq e' atj then
			  let gf = paf atd f in
			  if EqFFunct.eq g gf then
			    `OK (atk, atj, 
				f', f_tt f, 
				tl_unfold f t)
			  else `Error "g <> gf in TL_unfold"
			else `Error "e' <> atj in TL_unfold"
		      else `Error "c' <> atk in TL_unfold"
		  |`Error er -> `Error (er ^ "TL_unfold"))
	      |`Error er -> `Error er)
	  |`Error er -> `Error (er ^ "TL_unfold"))
      |`T_unde f ->
	  (match ij_ok f with
	  |`OK (f, atj, atk) ->
	      let ftt = f_tt f in
	      `OK (atk, atj, 
		  ftt, paf ftt f, 
		  t_unde f)
	  |`Error er -> `Error (er ^ "T_unde"))
      |`T_ee (lt, t) ->
	  (match el_trans t with
	  |`OK (c, e, f, h, t) ->
	      (match el_T_nn c (vmap5ok el_trans lt) with
	      |`OK (c, e', lf, lh, lt) ->
		  if EqFCat.eq e' e then
		    if not_in AtIndex.atu lt then
		      `OK (c, e, f_ee lh f, f_ee lf h,
			  t_ee lt t)
		    else `Error "AtIndex.atu is in lt in`T_ee"
		  else `Error "e' <> e in`T_ee"
	      |`Error er -> `Error (er ^ "T_ee"))
	  |`Error er -> `Error er)
      |`T_appl (lf, f) ->
	  (match el_funct f with
	  |`OK (c, e, f) ->
	      (match el_F_nn c (vmap3ok el_funct lf) with
	      |`OK (c, e', lf) ->
		  if EqFCat.eq e' e then
		    if not_in AtIndex.atu lf then
		      `OK (c, e, f_pp c (cons (AtIndex.atu, f_ee lf f) lf), f,
			  t_appl lf f)			
		    else `Error "AtIndex.atu is in lf in`T_appl"
		  else `Error "e' <> e in`T_appl"
	      |`Error er -> `Error (er ^ "T_appl"))
	  |`Error er -> `Error er)
      |`T_curry t ->
	  (match el_trans t with
	  |`OK (c, e, g, h, t) ->
	      (match Funct.t2de g with
	      |`F_pp (_, lf) ->
		  (match find_ok AtIndex.atu lf with
		  |`OK f -> `OK (c, e, f, f_ee (remove AtIndex.atu lf) h, 
				 t_curry t)
		  |`Error er -> `Error (er ^ "T_curry"))
	      | _ -> `Error "f not f_pp in`T_curry")
	  |`Error er -> `Error er)
      |`T_assert (t1, t2, l) ->
	  (match el_trans t1 with
	  |`OK (c1, e1, f1, h1, t1) ->
	      (match el_trans t2 with
	      |`OK (c2, e2, f2, h2, t2) ->
		  if EqFFunct.eq f1 f2 then
		    let type_bool = 
		      f_ss c1 (IList.cons (AtIndex.tt, f_pp c1 (IList.nil))
				(IList.cons (AtIndex.ff, f_pp c1 (IList.nil))
				   IList.nil))
		    in
		    if EqFFunct.eq h1 type_bool then
		      `OK (c1, e1, f1, h2, 
			   t_assert t1 t2 l)
		    else `Error ("h1 <> type_bool in `T_assert")
		  else `Error ("f1 <> f2 in `T_assert")
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      |`T_fail (f1, f2, l) ->
	  (match el_funct f1 with
	  |`OK (c1, e1, f1) ->
	      (match el_funct f2 with
	      |`OK (c2, e2, f2) ->
		  if EqFCat.eq c1 c2 then
		    if EqFCat.eq e1 c_BB then
		      if EqFCat.eq e2 c_BB then
			`OK (c1, e1, f1, f2,
			     t_fail f1 f2 l)			
		      else `Error "e2 not BB in `T_fail"
		    else `Error "e1 not BB in `T_fail"
		  else `Error "c1 <> c2 in `T_fail"
	      |`Error er -> `Error (er ^ "T_fail"))
	  |`Error er -> `Error er)
      |`T_fix t ->
	  (match el_trans t with
	  |`OK (c, e, f, g, t) ->
	      (match Cat.t2de e with
	      |`C_BB ->
		  if EqFFunct.eq f g then
		    `OK (c, c_BB, 
			 f_pp c nil, g, 
			 t_fix t)
		  else `Error "f <> g in`T_fix"
	      | _ -> `Error "c not BB in`T_fix")
	  |`Error er -> `Error er)
      |`TL_fix t ->
	  (match el_trans t with
	  |`OK (c, e, f, g, t) ->
	      (match Cat.t2de e with
	      |`C_BB ->
		  (match fg_ok f with
		  |`OK (c, atd, ate) ->
		      if EqFFunct.eq atd g then
			`OK (c, c_BB, 
			     ate, g, 
			     nf_TL_fix t) 
                             (* not tl_fix to avoid reaching fixpoint again *)
		      else `Error "atd <> g in TL_fix"
		  |`Error er -> `Error (er ^ "TL_fix"))
	      | _ -> `Error "c not BB in TL_fix")
	  |`Error er -> `Error er)

(* if el_trans is hash-consed, this is needed:  *)
    module HT = 
      struct
	type t = FTrans.t
	type result = [`OK of Cat.t * Cat.t * Funct.t * Funct.t * Trans.t
        	      |`Error of string]
	let equal t u = t == u
	let hash t = FTrans.t2stamp t
      end
    module MemoET = Memoize' (HT)

    let tbl_t = MemoET.create good_hash_size

    let el_trans = MemoET.memorec tbl_t el_trans
  end

module ElabFCore = 
  ElabFCore' (Location) (IdIndex) (AtIndex) (IList) (FCat) (SemFFunct)
    (SemFTrans) (DeFFunct) (DeFTrans) (PpFFunct) (OkError) (EqFCat) (EqFFunct) (EqFFunct)
