(* Copyright (C) 2003--2006 Mikolaj Konarski
 *
 * This file is part of the Dule compiler.
 * The Dule compiler is released under the GNU General Public License (GPL).
 * Please see the file Dule-LICENSE for license information.
 *
 * $Id: core_front.ml,v 1.85 2006-05-17 10:15:11 mikon Exp $
 *) 

open Core_back open Core_middle open Tools open Error_rep

module type DeTTrans = (* typed bare core language *)
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Location : Location

    type t = 
      | T_ID of Cat.t
      | T_COMP of t * t
      | T_PR of Cat.t IList.t * IdIndex.t
      | T_RECORD of Cat.t * t IList.t
      | T_FT of Funct.t * t
      | T_TF of t * Funct.t
      | T_id of Funct.t
      | T_comp of t * t
      | T_pp of Cat.t * t IList.t
      | T_pr of Funct.t IList.t * IdIndex.t
      | T_record of Funct.t * t IList.t
      | T_ss of Cat.t * t IList.t
      | T_in of Funct.t IList.t * IdIndex.t
      | T_case of t IList.t * Funct.t
      | TL_case of Funct.t * t IList.t * Funct.t
      | TE_case of Funct.t * t IList.t * 
	    Funct.t IList.t * Funct.t IList.t * Funct.t
      | T_map of Funct.t * t
      | TE_map of Funct.t * t * Funct.t IList.t * Funct.t
      | T_ii of t
      | T_con of Funct.t
      | T_fold of Funct.t * t
      | TL_fold of Funct.t * t
      | TE_fold of Funct.t * t * Funct.t IList.t * Funct.t
      | T_de of Funct.t
      | T_tt of t
      | T_uncon of Funct.t
      | T_unfold of Funct.t * t
      | TL_unfold of Funct.t * t
      | TE_unfold of Funct.t * t * Funct.t IList.t * Funct.t
      | T_unde of Funct.t
      | T_ee of t IList.t * t
      | T_appl of Funct.t IList.t * Funct.t
      | TE_appl of t IList.t * t * Funct.t IList.t * Funct.t
      | T_curry of t
      | TE_curry of Funct.t * (IdIndex.t * Funct.t) IList.t * t
      | T_pappl of t IList.t * t * Funct.t IList.t * Funct.t
      | T_assert of t * t  * Location.t
      | T_fail of Funct.t * Funct.t * Location.t
      | T_fix of t
      | TL_fix of t
      | TE_fix of Funct.t * IdIndex.t * t * Funct.t
  end

module type ConTTrans =
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
    val te_case : Funct.t -> t IList.t -> 
      Funct.t IList.t -> Funct.t IList.t -> Funct.t -> t
    val t_map : Funct.t -> t -> t
    val te_map : Funct.t -> t -> Funct.t IList.t -> Funct.t -> t
    val t_ii : t -> t
    val t_con : Funct.t -> t
    val t_fold : Funct.t -> t -> t
    val tl_fold : Funct.t -> t -> t
    val te_fold : Funct.t -> t -> Funct.t IList.t -> Funct.t -> t
    val t_de : Funct.t -> t
    val t_tt : t -> t
    val t_uncon : Funct.t -> t
    val t_unfold : Funct.t -> t -> t
    val tl_unfold : Funct.t -> t -> t
    val te_unfold : Funct.t -> t -> Funct.t IList.t -> Funct.t -> t
    val t_unde : Funct.t -> t
    val t_ee : t IList.t -> t -> t
    val t_appl : Funct.t IList.t -> Funct.t -> t
    val te_appl : t IList.t -> t -> Funct.t IList.t -> Funct.t -> t
    val t_curry : t -> t
    val te_curry : Funct.t -> (IdIndex.t * Funct.t) IList.t -> t -> t
    val t_pappl : t IList.t -> t -> Funct.t IList.t -> Funct.t -> t
    val t_assert :  t -> t -> Location.t -> t
    val t_fail : Funct.t -> Funct.t -> Location.t -> t
    val t_fix : t -> t
    val tl_fix : t -> t
    val te_fix : Funct.t -> IdIndex.t -> t -> Funct.t -> t
  end


module DeTTrans'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : T)
    (Funct : T)
    (Location : Location)
    : (DeTTrans
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

    type t = 
      | T_ID of Cat.t
      | T_COMP of t * t
      | T_PR of Cat.t IList.t * IdIndex.t
      | T_RECORD of Cat.t * t IList.t
      | T_FT of Funct.t * t
      | T_TF of t * Funct.t
      | T_id of Funct.t
      | T_comp of t * t
      | T_pp of Cat.t * t IList.t
      | T_pr of Funct.t IList.t * IdIndex.t
      | T_record of Funct.t * t IList.t
      | T_ss of Cat.t * t IList.t
      | T_in of Funct.t IList.t * IdIndex.t
      | T_case of t IList.t * Funct.t
      | TL_case of Funct.t * t IList.t * Funct.t
      | TE_case of Funct.t * t IList.t * 
	    Funct.t IList.t * Funct.t IList.t * Funct.t
      | T_map of Funct.t * t
      | TE_map of Funct.t * t * Funct.t IList.t * Funct.t
      | T_ii of t
      | T_con of Funct.t
      | T_fold of Funct.t * t
      | TL_fold of Funct.t * t
      | TE_fold of Funct.t * t * Funct.t IList.t * Funct.t
      | T_de of Funct.t
      | T_tt of t
      | T_uncon of Funct.t
      | T_unfold of Funct.t * t
      | TL_unfold of Funct.t * t
      | TE_unfold of Funct.t * t * Funct.t IList.t * Funct.t
      | T_unde of Funct.t
      | T_ee of t IList.t * t
      | T_appl of Funct.t IList.t * Funct.t
      | TE_appl of t IList.t * t * Funct.t IList.t * Funct.t
      | T_curry of t
      | TE_curry of Funct.t * (IdIndex.t * Funct.t) IList.t * t
      | T_pappl of t IList.t * t * Funct.t IList.t * Funct.t
      | T_assert of t * t * Location.t
      | T_fail of Funct.t * Funct.t * Location.t
      | T_fix of t
      | TL_fix of t
      | TE_fix of Funct.t * IdIndex.t * t * Funct.t
  end

module DeTTrans = DeTTrans' (IdIndex) (IList) (ACat) (AFunct) (Location)


module NormalConTTrans' 
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : T)
    (Funct : T)
    (DeTTrans : DeTTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    : (ConTTrans
    with module IdIndex = IdIndex
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct
    with type t = DeTTrans.t
    with module Location = DeTTrans.Location) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat = Cat
    module Funct = Funct
    module Location = DeTTrans.Location
    open DeTTrans

    type t = DeTTrans.t

    let t_ID c = T_ID c
    let t_COMP t1 t2 = T_COMP (t1, t2)
    let t_PR lc i = T_PR (lc, i)
    let t_RECORD c lt = T_RECORD (c, lt)
    let t_FT f1 t2 = T_FT (f1, t2)
    let t_TF t1 f2 = T_TF (t1, f2)
    let t_id g = T_id g
    let t_comp t u = T_comp (t, u)
    let t_pp c lt = T_pp (c, lt)
    let t_pr lf i = T_pr (lf, i)
    let t_record f lt = T_record (f, lt)
    let t_ss c lt = T_ss (c, lt)
    let t_in lf i = T_in (lf, i)
    let t_case lt h = T_case (lt, h)
    let tl_case f lt h = TL_case (f, lt, h)
    let te_case f lt lg lf h = TE_case (f, lt, lg, lf, h)
    let t_map g t = T_map (g, t)
    let te_map g t lgin h = TE_map (g, t, lgin, h)
    let t_ii t = T_ii t
    let t_con g = T_con g
    let t_fold g t = T_fold (g, t)
    let tl_fold g t = TL_fold (g, t)
    let te_fold g t lgin h = TE_fold (g, t, lgin, h)
    let t_de g = T_de g
    let t_tt t = T_tt t
    let t_uncon g = T_uncon g
    let t_unfold g t = T_unfold (g, t)
    let tl_unfold g t = TL_unfold (g, t)
    let te_unfold g t lgin hg = TE_unfold (g, t, lgin, hg)
    let t_unde g = T_unde g
    let t_ee lt u = T_ee (lt, u)
    let t_appl lg h = T_appl (lg, h)
    let te_appl lt t lg h = TE_appl (lt, t, lg, h)
    let t_curry t = T_curry t
    let te_curry f lig t = TE_curry (f, lig, t)
    let t_pappl lt t lg h = T_pappl (lt, t, lg, h)
    let t_assert u t l = T_assert (u, t, l)
    let t_fail f h l = T_fail (f, h, l)
    let t_fix t = T_fix t
    let tl_fix t = TL_fix t
    let te_fix f i t h = TE_fix (f, i, t, h)
  end

module NormalConTTrans = NormalConTTrans' (IdIndex) (IList) (ACat) (AFunct)
    (DeTTrans)


module SemConTTrans' 
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : T)
    (Funct : SemFFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (PpFFunct : PpFFunct
    with type Funct.t = Funct.t)
    (Trans : SemFTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (EqFFunct : EqFFunct with type Funct.t = Funct.t)
    (SrcFCore : SrcFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with type Funct.t = Funct.t)
    (DomFCore : DomFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t)
    : (ConTTrans
    with module IdIndex = IdIndex
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct
    with type t = Trans.t
    with module Location = Trans.Location) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat = Cat
    module Location = Trans.Location
    module Funct' = Funct
    open IList
    open Funct
    open Trans
    open SrcFCore
    open DomFCore
    module Funct = Funct'

    type t = Trans.t

    let ass t f h =
      EqFFunct.eq (dom t) f &&
      EqFFunct.eq (cod t) h

    let t_ID c = t_ID c
    let t_COMP t1 t2 = t_COMP t1 t2
    let t_PR lc i = t_PR lc i
    let t_RECORD c lt = t_RECORD c lt
    let t_FT f1 t2 = t_FT f1 t2
    let t_TF t1 f2 = t_TF t1 f2
    let t_id g = t_id g
    let t_comp t u = t_comp t u
    let t_pp c lt = t_pp c lt
    let t_pr lf i = t_pr lf i
    let t_record f lt = t_record f lt
    let t_ss c lt = t_ss c lt
    let t_in lf i = t_in lf i
    let t_case lt h = t_case lt h
    let tl_case f lt h = tl_case f lt h

    let te_case f lt lg lf h =
      let c = src f in
      let algf = cons (AtIndex.atu, f) lg in
      let pratu = t_pr algf AtIndex.atu in
      let pal = f_pp c algf in
      let cgn = cons (AtIndex.ate, pal) nil in
      let gss = f_ss c lf in
      let down (i, t) =
	if not (IdIndex.eq i AtIndex.atc) then
	  let f_i = find i lf in
	  let lgin = cons (AtIndex.it, f_i) lg in
	  assert (ass t f (f_ee lgin h));
	  let cgf = cons (AtIndex.atd, f_i) cgn in
	  let pgf = f_pp c cgf in
	  let prg = t_pr cgf AtIndex.ate in
	  let lpr = imap (fun i -> t_comp prg (t_pr algf i)) lg in
	  let ljpr = cons (AtIndex.it, t_pr cgf AtIndex.atd) lpr in
	  let pit = t_comp (t_comp prg pratu) t in
	  let rap = t_record pgf (cons (AtIndex.atu, pit) ljpr) in
	  let tap = t_comp rap (t_appl lgin h) in
	  tap
	else (* default case of case expression *)
	  let f_i = f_pp c nil in
	  let lgin = cons (AtIndex.it, f_i) lg in
	  assert (ass t f (f_ee lgin h));
	  let f_i = gss in
	  let cgf = cons (AtIndex.atd, f_i) cgn in
	  let pgf = f_pp c cgf in
	  let prg = t_pr cgf AtIndex.ate in
	  let lpr = imap (fun i -> t_comp prg (t_pr algf i)) lg in
	  let ljpr = cons (AtIndex.it, t_record pgf nil) lpr in
	  let pit = t_comp (t_comp prg pratu) t in
	  let rap = t_record pgf (cons (AtIndex.atu, pit) ljpr) in
	  let tap = t_comp rap (t_appl lgin h) in
	  tap
      in
      let ltd = bmap down lt in
      let nut = tl_case pal ltd h in
      assert (ass nut (f_pp c (Funct.cof gss pal)) h);
      let lng = cons (AtIndex.it, gss) algf in
      let png = f_pp c lng in
      let lpra = imap (t_pr lng) algf in
      let pin = t_pr lng AtIndex.it in
      let cpia = cof pin (t_record png lpra) in
      let rep = t_comp (t_record png cpia) nut in
      t_curry rep

    let t_map g t = t_map g t

    let te_map g t lgin h =
      let f = dom t in
      let c = src f in
      assert (ass t f (f_ee lgin h));
      let g' = find AtIndex.it lgin in
      let lg = remove AtIndex.it lgin in
      let lr = cons (AtIndex.atu, f) lg in
      let cgr = f_pp c lr in
      let lrt = Funct.cof g' cgr in
      let prtg = t_pr lrt AtIndex.ate in
      let piha = imap (fun k -> t_comp prtg (t_pr lr k)) lg in
      let pit = t_comp (t_comp prtg (t_pr lr AtIndex.atu)) t in
      let pita = cons (AtIndex.atu, pit) piha in
      let pih = cons (AtIndex.it, t_pr lrt AtIndex.atd) pita in
      let t = t_comp (t_record (f_pp c lrt) pih) (t_appl lgin h) in
      let g'g = paf g' g in
      let rep = t_map g t in
      assert (ass rep (f_pp c (Funct.cof g'g cgr)) (paf h g));
      let lrn = cons (AtIndex.it, g'g) lr in
      let paff = f_pp c lrn in
      let pilr = imap (t_pr lrn) lr in
      let pirk = cof (t_pr lrn AtIndex.it) (t_record paff pilr) in
      let rep = t_comp (t_record paff pirk) rep in
      t_curry rep

    let t_ii t = t_ii t
    let t_con g = t_con g
    let t_fold g t = t_fold g t
    let tl_fold g t = tl_fold g t

    let te_fold g t lgin h =
      let f = dom t in
      let c = src f in
      assert (ass t f (f_ee lgin h));
      let hg = find AtIndex.it lgin in
      assert (EqFFunct.eq hg (paf h g));
      let lg = remove AtIndex.it lgin in
      let lr = cons (AtIndex.atu, f) lg in
      let cgr = f_pp c lr in
      let lrt = Funct.cof hg cgr in
      let prtg = t_pr lrt AtIndex.ate in
      let piha = imap (fun k -> t_comp prtg (t_pr lr k)) lg in
      let pit = t_comp (t_comp prtg (t_pr lr AtIndex.atu)) t in
      let pita = cons (AtIndex.atu, pit) piha in
      let pih = cons (AtIndex.it, t_pr lrt AtIndex.atd) pita in
      let t = t_comp (t_record (f_pp c lrt) pih) (t_appl lgin h) in
      let rep = tl_fold g t in
      assert (ass rep (f_pp c (Funct.cof (f_ii g) cgr)) h);
      let lrn = cons (AtIndex.it, f_ii g) lr in
      let paff = f_pp c lrn in
      let pilr = imap (t_pr lrn) lr in
      let pirk = cof (t_pr lrn AtIndex.it) (t_record paff pilr) in
      let rep = t_comp (t_record paff pirk) rep in
      t_curry rep

    let t_de g = t_de g
    let t_tt t = t_tt t
    let t_uncon g = t_uncon g
    let t_unfold g t = t_unfold g t
    let tl_unfold g t = tl_unfold g t

    let te_unfold g t lgin hg =
      let f = dom t in
      let c = src f in
      assert (ass t f (f_ee lgin hg));
      let h = find AtIndex.it lgin in
      assert (EqFFunct.eq hg (paf h g));
      let lg = remove AtIndex.it lgin in
      let lr = cons (AtIndex.atu, f) lg in
      let cgr = f_pp c lr in
      let lrt = Funct.cof h cgr in
      let prtg = t_pr lrt AtIndex.ate in
      let piha = imap (fun k -> t_comp prtg (t_pr lr k)) lg in
      let pit = t_comp (t_comp prtg (t_pr lr AtIndex.atu)) t in
      let pita = cons (AtIndex.atu, pit) piha in
      let pih = cons (AtIndex.it, t_pr lrt AtIndex.atd) pita in
      let t = t_comp (t_record (f_pp c lrt) pih) (t_appl lgin hg) in
      let rep = tl_unfold g t in
      assert (ass rep (f_pp c lrt) (f_tt g));
      let lrn = cons (AtIndex.it, h) lr in
      let paff = f_pp c lrn in
      let pilr = imap (t_pr lrn) lr in
      let pirk = cof (t_pr lrn AtIndex.it) (t_record paff pilr) in
      let rep = t_comp (t_record paff pirk) rep in
      t_curry rep

    let t_unde g = t_unde g
    let t_ee lt u = t_ee lt u
    let t_appl lg h = t_appl lg h

    let te_appl lt t lg h =
      let f = dom t in
      let ret = t_record f (cons (AtIndex.atu, t) lt) in
      let aph = t_appl lg h in
      t_comp ret aph

    let t_curry t = t_curry t

    let te_curry f lig t = 
      let c = src f in
      let lgj = vmap (fun (k, f) -> f) lig in
      let lgu = cons (AtIndex.atu, f) lgj in
      let fpu = f_pp c lgu in
      let pifu = t_pr lgu AtIndex.atu in
      let prf = bfold nil (fun (j, (k, f)) r -> 
	if IdIndex.is_patt k then
	  let lf = unpp f in
	  let prj = t_pr lgu j in
	  let lt = imap (fun i -> t_comp prj (t_pr lf i)) lf in
	  lt @@ r
	else cons (k, t_pr lgu j) r) lig in
      let lfh = unpp f in
      let lf = ifilter (fun i -> not_in i prf) lfh in
      let prd = imap (fun i -> t_comp pifu (t_pr lfh i)) lf in
      let repr = t_record fpu (prf @@ prd) in
      let rt = t_comp repr t in
      t_curry rt

    let t_pappl lt t lg h =
      let lg_left = diff lg (vmap cod lt) in
      let f = dom t in
      let c = src f in
      let udom = cons (AtIndex.atu, f) lg_left in
      let cudo = f_pp c udom in 
      let lfpr = imap (t_pr udom) lg_left in
      let prudo = t_pr udom AtIndex.atu in
      let prutu = t_comp prudo t in
      let ltutu = vmap (fun t -> t_comp prudo t) lt in
      let ret = t_record cudo (cons (AtIndex.atu, prutu) (ltutu @@ lfpr)) in
      let aph = t_appl lg h in
      t_curry (t_comp ret aph)

    let t_assert u t l = t_assert u t l
    let t_fail f h l = t_fail f h l
    let t_fix t = t_fix t
    let tl_fix t = tl_fix t

    let te_fix f i t h = 
      let c = src f in
      let lff = Funct.cof h f in
      let prff = t_pr lff AtIndex.ate in
      let lfmg = unpp f in
      let lf = if is_in i lfmg then remove i lfmg else lfmg in
      let prd = imap (fun i -> t_comp prff (t_pr lfmg i)) lf in
      let prfd = cons (i, t_pr lff AtIndex.atd) prd in
      let repr = t_comp (t_record (f_pp c lff) prfd) t in
      tl_fix repr
  end

module SemConTTrans = SemConTTrans' (IdIndex) (AtIndex) (IList) (ACat)
    (SemFFunct) (PpFFunct) (SemFTrans) (EqFFunct) (SrcFCore)
    (DomFCore) 


module type SubTTrans =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module VarStamp : Stamp
    module Cat : T
    module Funct : DeFFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with module VarStamp = VarStamp
    module SubFFunct : Substitution
    with module IdIndex = IdIndex
    with module IList = IList
    with module VarStamp = VarStamp
    with type Type.t = Funct.t
    module Trans : sig type t module Location : Location end
    module TTrans : sig type t module Location : Location end
    with module Location = Trans.Location

    val sub_finish_t : SubFFunct.subst -> TTrans.t -> 
      Trans.t
    val sub_finish_lt : SubFFunct.subst -> TTrans.t IList.t -> 
      Trans.t IList.t
  end

module SubTTrans'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (VarStamp : Stamp)
    (Cat : FCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : FFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with module VarStamp = VarStamp)
    (Trans : FTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (TTrans : DeTTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with module Location = Trans.Location)
    (ConTTrans : ConTTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type t = Trans.t
    with module Location = Trans.Location)
    (SubFFunct : Substitution
    with module IdIndex = IdIndex
    with type 'a IListBasic.t = 'a IList.IListBasic.t
    with type 'a IList.IListBasic.t = 'a IList.IListBasic.t
    with module IList = IList
    with module VarStamp = VarStamp
    with type Type.t = Funct.t)
    : (SubTTrans
    with module IdIndex = IdIndex
    with module IList = IList
    with module VarStamp = VarStamp
    with module Cat = Cat
    with module Funct = Funct
    with module SubFFunct = SubFFunct
    with module Trans = Trans
    with module TTrans = TTrans) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module VarStamp = VarStamp
    module Cat = Cat
    module Funct = Funct
    module SubFFunct = SubFFunct
    module Trans = Trans
    module TTrans = TTrans
    open TTrans
    open ConTTrans
    open SubFFunct

    let rec sub_finish_t s t' =
      let sub_finish_f_s = sub_finish_f s in (*memoization *)
      let sub_finish_lf_s = sub_finish_lf s in
      match t' with 
      | T_RECORD (c, lt) -> t_RECORD c (sub_finish_lt s lt)
      | T_TF (t1, f2) -> t_TF (sub_finish_t s t1) (sub_finish_f_s f2)
      | T_id g -> t_id (sub_finish_f_s g)
      | T_comp (t, u) -> t_comp (sub_finish_t s t) (sub_finish_t s u)
      | T_pr (lf, i) -> t_pr (sub_finish_lf_s lf) i
      | T_record (f, lt) -> t_record (sub_finish_f_s f) (sub_finish_lt s lt)
      | T_in (lf, i) -> t_in (sub_finish_lf_s lf) i
      | TL_case (f, lt, h) -> 
	  tl_case (sub_finish_f_s f) (sub_finish_lt s lt) (sub_finish_f_s h)
      | TE_case (f, lt, lg, lf, h) ->
	  te_case (sub_finish_f_s f) (sub_finish_lt s lt) 
	    (sub_finish_lf_s lg) (sub_finish_lf_s lf) (sub_finish_f_s h)
      | TE_map (g, t, lgin, h) -> te_map (sub_finish_f_s g) (sub_finish_t s t)
	    (sub_finish_lf_s lgin) (sub_finish_f_s h)
      | T_con g -> t_con (sub_finish_f_s g)
      | TL_fold (g, t) -> tl_fold (sub_finish_f_s g) (sub_finish_t s t)
      | TE_fold (g, t, lgin, h) -> te_fold (sub_finish_f_s g)
	    (sub_finish_t s t) (sub_finish_lf_s lgin) (sub_finish_f_s h)
      | T_de g -> t_de (sub_finish_f_s g)
      | T_uncon g -> t_uncon (sub_finish_f_s g)
      | TL_unfold (g, t) -> tl_unfold (sub_finish_f_s g) (sub_finish_t s t)
      | TE_unfold (g, t, lgin, hg) -> te_unfold (sub_finish_f_s g)
	    (sub_finish_t s t) (sub_finish_lf_s lgin) (sub_finish_f_s hg)
      | T_unde g -> t_unde (sub_finish_f_s g)
      | T_appl (lg, h) -> t_appl (sub_finish_lf_s lg) (sub_finish_f_s h)
      | TE_appl (lt, t, lg, h) -> te_appl (sub_finish_lt s lt)
	    (sub_finish_t s t) (sub_finish_lf_s lg) (sub_finish_f_s h)
      | T_curry t -> t_curry (sub_finish_t s t)
      | TE_curry (f, lig, t) -> 
	  let sub_finish_ljf s l = IList.vmap (fun (j, f) -> 
	    (j, sub_finish_f_s f)) l 
	  in
	  te_curry (sub_finish_f_s f) (sub_finish_ljf s lig) (sub_finish_t s t)
      | T_pappl (lt, t, lg, h) -> t_pappl (sub_finish_lt s lt)
	    (sub_finish_t s t) (sub_finish_lf_s lg) (sub_finish_f_s h)
      | T_assert (u, t, l) -> 
	  t_assert (sub_finish_t s u) (sub_finish_t s t) l
      | T_fail (f, h, l) -> t_fail (sub_finish_f_s f) (sub_finish_f_s h) l
      | TL_fix t -> tl_fix (sub_finish_t s t)
      | TE_fix (f, i, t, h) -> te_fix (sub_finish_f_s f) i (sub_finish_t s t)
	    (sub_finish_f_s h)
      | _ -> failwith "SubTTrans.sub_finish_t: lazy me"
    and sub_finish_lt s lt = IList.vmap (sub_finish_t s) lt
  end

module SubTTrans = SubTTrans' (IdIndex) (IList) (Stamp) (FCat)
    (FFunct) (FTrans) (DeTTrans) (SemConTTrans) (SubFFunct)


module type BCore = (* untyped bare core language *)
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Location : Location

    type kind = unit
    type typ = Location.t * typ'
    and typ' = 
      | F_COMP of typ * typ
      |	F_PR of IdIndex.t 
      | F_pp of typ IList.t
      | F_ss of typ IList.t
      | F_ii of IdIndex.t * typ
      | F_tt of IdIndex.t * typ
      | F_ee of typ IList.t * typ
      |	F_x
    type valu = Location.t * valu'
    and valu' =
      | T_id of typ
      | T_comp of valu * valu
      | T_pr of IdIndex.t
      | T_record of valu IList.t
      | T_in of IdIndex.t
      | T_case of valu IList.t
      | T_map of valu
      | T_con
      | T_fold of valu
      | T_de
      | T_uncon
      | T_unfold of valu
      | T_unde
      | T_appl of valu IList.t * valu
      | T_curry of (IdIndex.t * typ) IList.t * valu
      | T_pappl of valu IList.t * valu
      | T_assert of valu * valu
      | T_fail
      | T_fix of IdIndex.t * valu

    val term_typ : typ -> typ'
    val loc_typ : typ -> Location.t
    val term_valu : valu -> valu'
    val loc_valu : valu -> Location.t
  end

module BCore'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Location : Location)
    : (BCore 
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Location = Location

    type kind = unit
    type typ = Location.t * typ'
    and typ' = 
      | F_COMP of typ * typ
      |	F_PR of IdIndex.t 
      | F_pp of typ IList.t
      | F_ss of typ IList.t
      | F_ii of IdIndex.t * typ
      | F_tt of IdIndex.t * typ
      | F_ee of typ IList.t * typ
      |	F_x
    type valu = Location.t * valu'
    and valu' =
      | T_id of typ
      | T_comp of valu * valu
      | T_pr of IdIndex.t
      | T_record of valu IList.t
      | T_in of IdIndex.t
      | T_case of valu IList.t
      | T_map of valu
      | T_con
      | T_fold of valu
      | T_de
      | T_uncon
      | T_unfold of valu
      | T_unde
      | T_appl of valu IList.t * valu
      | T_curry of (IdIndex.t * typ) IList.t * valu
      | T_pappl of valu IList.t * valu
      | T_assert of valu * valu
      | T_fail
      | T_fix of IdIndex.t * valu

    let term_typ (_, f) = f
    let loc_typ (l, _) = l
    let term_valu (_, t) = t
    let loc_valu (l, _) = l
   end

module BCore = BCore' (IdIndex) (IList) (Location)


module type ElabBCore =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Location : Location
    module ErrorRepLib : ErrorRepLib
    with module Location = Location
    module Cat : T
    module Funct : T
    module Trans : FTrans
    with module Location = Location
    module BCore : BCore
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location

    val check_assert : bool ref
    val elab_kind : BCore.kind -> Cat.t
    val elab_typ : Cat.t -> BCore.typ -> 
      [`OK of Funct.t|`Error of ErrorRepLib.error]
    val elab_valu : Funct.t -> Funct.t -> BCore.valu ->
      [`OK of Trans.t|`Error of ErrorRepLib.error]
    val elab_core : Trans.t -> BCore.valu ->
      [`OK of Funct.t * Trans.t|`Error of ErrorRepLib.error]
  end

module ElabBCore'
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IListBasic : IListBasic with type Index.t = IdIndex.t)
    (IList : IList 
    with type Index.t = IdIndex.t
    with type 'a IListBasic.t = 'a IListBasic.t)
    (Location : Location)
    (ErrorRepLib : ErrorRepLib
    with module Location = Location)
    (Cat : SemFCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : SemFFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : FTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with module Location = Location)
    (SemFTrans : SemFTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type t = Trans.t
    with module Location = Location)
    (ConTTrans : ConTTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with module Location = Location)
    (SrcFCore : SrcFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with type Funct.t = Funct.t)
    (PpFFunct : PpFFunct
    with type Funct.t = Funct.t
    with type 'a Funct.IList.IListBasic.t = 'a IListBasic.t)
    (PpFTrans : PpFTrans
    with type Trans.t = Trans.t)
    (EqFCat : EqFCat with type Cat.t = Cat.t)
    (EqFFunct : EqFFunct with type Funct.t = Funct.t)
    (SubFFunct : Substitution
    with module IdIndex = IdIndex
    with module IListStamp = IListBasic.IListStamp
    with module IListBasic = IListBasic
    with module IList = IList
    with module VarStamp = Funct.VarStamp
    with type Type.t = Funct.t)
    (SubTTrans : SubTTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with module VarStamp = Funct.VarStamp
    with module SubFFunct = SubFFunct
    with type Trans.t = Trans.t
    with type TTrans.t = ConTTrans.t)
    (UnifyFCore : UnifyFCore
    with module IdIndex = IdIndex 
    with module IListBasic = IListBasic
    with module IList = IList
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with module VarStamp = Funct.VarStamp
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with module SubFFunct = SubFFunct)
    (BCore : BCore 
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location)
    (DomFCore : DomFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t)
    : (ElabBCore
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with module Cat = Cat
    with module Funct = Funct
    with module Trans = Trans
    with module BCore = BCore) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Location = Location
    module ErrorRepLib = ErrorRepLib
    module Cat = Cat
    module Funct = Funct
    module Trans = Trans
    module BCore = BCore
    open Cat
    open Funct
    open Trans
    open ConTTrans
    open SrcFCore
    open ErrorRepLib

    let check_assert = ref true

    let fresh_ty_var var_num c = (* where to put this? *)
      (f_var var_num c, VarStamp.inc var_num)
    
    let rec el_typ var_num c f = (* c = src f *)
      (match el_typ' var_num c f with
      |`OK (g, var_num) ->
	  if EqFCat.eq (trg g) c_BB then
	    `OK (g, var_num)
	  else
	    `Error (coreFrontError#instance 
		      [Loc (BCore.loc_typ f); 
		       Msg "ordinary type should have basic kind"])
      |`Error er -> `Error er) 
    and el_typ' var_num c f = (* c = src f *)
      match BCore.term_typ f with
      | BCore.F_COMP (f1, f2) ->
          (match el_typ' var_num c f1 with
          |`OK (g1, var_num) ->
	      let d = trg g1 in
	      if EqFCat.eq d c_BB then
		`Error (coreFrontError#instance 
			  [Loc (BCore.loc_typ f); 
			   Msg "basic kind in composition not allowed"])
	      else
		(match el_typ' var_num d f2 with
		|`OK (g2, var_num) ->
		    `OK (f_COMP g1 g2, var_num)
		|`Error er -> `Error er)
          |`Error er -> `Error er) 
      | BCore.F_PR i ->
	  let lc = unPP c in
	  (match IList.find_ok i lc with
	  |`OK _ -> 
	      `OK (f_PR lc i, var_num)
	  |`Error er -> 
	      `Error (coreFrontError#instance 
			[Loc (BCore.loc_typ f); 
			 Msg ("unknown " ^ er)]))
      | BCore.F_pp lf ->
          (match el_typ_ilist var_num c lf with
          |`OK (lg, var_num) -> `OK (f_pp c lg, var_num)
          |`Error er -> `Error er)
      | BCore.F_ss lf ->
          (match el_typ_ilist var_num c lf with
          |`OK (lg, var_num) -> `OK (f_ss c lg, var_num)
          |`Error er -> `Error er)
      | BCore.F_ii (i, g) ->
	  let lc = unPP c in
          let lb = 
	    if IList.is_in i lc then lc 
	    else IList.cons (i, c_BB) lc 
	  in
          (match el_typ (* not el_typ' *) var_num (c_PP lb) g with
          |`OK (g, var_num) ->
	      let la = Cat.coi c_BB (c_PP lc) in
	      let pri = f_PR la AtIndex.atj in
	      let prj = f_PR la AtIndex.atk in
	      let pis = IList.imap (fun i -> 
		f_COMP prj (f_PR lc i)) (IList.remove i lb) in
	      let rep = f_RECORD (c_PP la) (IList.cons (i, pri) pis) in
	      `OK (f_ii (f_COMP rep g), var_num)
	  |`Error er -> `Error er)
      | BCore.F_tt (i, g) ->
	  let lc = unPP c in
          let lb = 
	    if IList.is_in i lc then lc 
	    else IList.cons (i, c_BB) lc 
	  in
          (match el_typ (* not el_typ' *) var_num (c_PP lb) g with
          |`OK (g, var_num) ->
	      let la = Cat.coi c_BB (c_PP lc) in
	      let pri = f_PR la AtIndex.atj in
	      let prj = f_PR la AtIndex.atk in
	      let pis = IList.imap (fun i -> 
		f_COMP prj (f_PR lc i)) (IList.remove i lb) in
	      let rep = f_RECORD (c_PP la) (IList.cons (i, pri) pis) in
	      `OK (f_tt (f_COMP rep g), var_num)
	  |`Error er -> `Error er)
      | BCore.F_ee (lg, h) ->
          (match el_typ_ilist var_num c lg with
          |`OK (lg, var_num) ->
              (match el_typ (* not el_typ' *) var_num c h with
              |`OK (h, var_num) -> `OK (f_ee lg h, var_num)
	      |`Error er -> `Error er)
          |`Error er -> `Error er)
      | BCore.F_x ->
          let (fv, var_num) = fresh_ty_var var_num c in
          `OK (fv, var_num)
    and el_typ_ilist var_num c lf = 
      IList.bfold1ok (IList.nil, var_num)
	(fun (i, f) (lg, var_num) -> 
	  (match el_typ (* not el_typ' *) var_num c f with
	  |`OK (g, var_num) -> `OK (IList.cons (i, g) lg, var_num)
	  |`Error er -> `Error er)) lf

    let freshen_ilist l_stamp c l = (* where to put that? *)
      (IListBasic.rigid2flexible l_stamp (f_ID c) l, 
      IListBasic.IListStamp.inc l_stamp)

    let ass result fdom fcod s t =
      (match result with
      |`OK (_, lfg, var_num, l_stamp) ->
	  let fd = DomFCore.dom t in
	  let fc = DomFCore.cod t in
	  let sub_finish_f_s = SubFFunct.sub_finish_f s in (*memoization *)
	  let sfdom = sub_finish_f_s fdom in
	  let sfcod = sub_finish_f_s fcod in
	  if not 
	      (EqFFunct.eq sfdom fd && EqFFunct.eq sfcod fc)
	  then
	    failwith ("ElabBCore.el_valu: \nt(" 
		      ^ PpFTrans.pp_t t ^ ")\n fdom ("
		      ^ PpFFunct.pp_f fdom ^ ")\nsfdom ("
		      ^ PpFFunct.pp_f sfdom ^ ")\n   fd ("
		      ^ PpFFunct.pp_f fd ^ ")\n fcod ("
		      ^ PpFFunct.pp_f fcod ^ ")\nsfcod ("
		      ^ PpFFunct.pp_f sfcod ^ ")\n   fc ("
		      ^ PpFFunct.pp_f fc ^ ")")
	  else 
	    let rec foldl init f =
	      function
		| [] -> init
		| v :: r -> foldl (f v init) f r
	    in
	    foldl true (fun (f', g', l) r ->
	      let f = sub_finish_f_s f' in
	      let g = sub_finish_f_s g' in
	      if EqFFunct.eq f g then r
	      else
		failwith ("ElabBCore.el_valu unify: \nf'(" 
			  ^ PpFFunct.pp_f f' 
			  ^ ")\ng'("
			  ^ PpFFunct.pp_f g' 
			  ^ ")\n f("
			  ^ PpFFunct.pp_f f 
			  ^ ")\n g("
			  ^ PpFFunct.pp_f g ^ ")\nlfg:"
			  ^ let pr2 (f, g) = 
			    "\n(f = " ^ PpFFunct.pp_f f
			    ^ ",\n g = " ^ PpFFunct.pp_f g ^ ")"
			  in
			  foldl "" (fun (f, g, l) r -> 
			    pr2 (f, g) ^ r) lfg)) lfg
      |`Error er -> true)

    let rec el_valu var_num l_stamp fdom fcod t =
      let result = el_valu' var_num l_stamp fdom fcod t in
(*      assert 
	(match result with
	|`OK (t, lfg, var_num, l_stamp) ->
	    (match UnifyFCore.unify var_num l_stamp lfg with
	    |`OK (_, s, _, _) ->
		let s = SubFFunct.finish_sub var_num l_stamp s in
		let t = SubTTrans.sub_finish_t s t in
		ass result fdom fcod s t
	    |`Error er -> true)
	|`Error er -> true); *)
      result
    and el_valu' var_num l_stamp fdom fcod t =
      let c = src fdom in
      let l = BCore.loc_valu t in
      match BCore.term_valu t with 
      | BCore.T_id g ->
          (match el_typ var_num c g with
          |`OK (g, var_num) ->
	      let lfg = [(fdom, g, l); (fcod, g, l)] in
	      `OK (t_id g, lfg, var_num, l_stamp)
          |`Error er -> `Error er)
      | BCore.T_comp (t1, t2) ->
	  let (g, var_num) = fresh_ty_var var_num c in
	  (match el_valu var_num l_stamp fdom g t1 with
	  |`OK (t1, lfg1, var_num, l_stamp) ->
              (match el_valu var_num l_stamp g fcod t2 with
              |`OK (t2, lfg2, var_num, l_stamp) ->
		  let lfg = lfg1 @ lfg2 in
		  `OK (t_comp t1 t2, lfg, var_num, l_stamp)
              |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | BCore.T_pr i ->
	  let (lf, l_stamp) = freshen_ilist l_stamp c
	      (IList.cons (i, fcod) IList.nil) in
	  let lfg = [(fdom, f_pp c lf, l)] in
          `OK (t_pr lf i, lfg, var_num, l_stamp)
      | BCore.T_record lt ->
	  (match el_valu_ilist_fresh_var var_num l_stamp fdom lt with
	  |`OK (lh, lt, lfg, var_num, l_stamp) ->
	      let h = f_pp c lh in
	      let lfg = (fcod, h, l)::lfg in
              `OK (t_record fdom lt, lfg, var_num, l_stamp)
	  |`Error er -> `Error er)
      | BCore.T_in i ->
	  let (lf, l_stamp) = freshen_ilist l_stamp c
	      (IList.cons (i, fdom) IList.nil) in
	  let lfg = [(fcod, f_ss c lf, l)] in
	  `OK (t_in lf i, lfg, var_num, l_stamp)
      | BCore.T_case lt ->
	  let (h, var_num) = fresh_ty_var var_num c in	
          let (lg, l_stamp) = freshen_ilist l_stamp c IList.nil in 
	  let starting = (IList.nil, IList.nil, [], var_num, l_stamp) in
	  let process (i, t_i) (lf, lt, lfg1, var_num, l_stamp) =
	    if not (IdIndex.eq i AtIndex.atc) then
              let (f_i, var_num) = fresh_ty_var var_num c in
              let lgin = IListBasic.move_flexible lg 
		  (IList.cons (AtIndex.it, f_i) lg) in
              let fee = f_ee lgin h in
              (match el_valu var_num l_stamp fdom fee t_i with
              |`OK (t_i, lfg2, var_num, l_stamp) ->
		  let lfg = lfg2 @ lfg1 in
		  let new_lf = IList.cons (i, f_i) lf in 
		  let lf = 
		    if IListBasic.is_flexible lf then
		      IListBasic.move_flexible lf new_lf
		    else new_lf
		  in
		  `OK (lf, IList.cons (i, t_i) lt, 
		       lfg, var_num, l_stamp)
	      |`Error er -> `Error er)	      
	    else (* default case of case expression *)
              let f_i = f_pp c IList.nil in
              let lgin = IListBasic.move_flexible lg 
		  (IList.cons (AtIndex.it, f_i) lg) in
              let fee = f_ee lgin h in
              (match el_valu var_num l_stamp fdom fee t_i with
              |`OK (t_i, lfg2, var_num, l_stamp) ->
		  let lfg = lfg2 @ lfg1 in
		  if IListBasic.is_flexible lf then
		    `Error (coreFrontError#instance 
			      [Loc l; 
			       Msg ("double default case")])
		  else
		    let (lf, l_stamp) = freshen_ilist l_stamp c lf in 
		    `OK (lf, IList.cons (i, t_i) lt, 
			 lfg, var_num, l_stamp)
	      |`Error er -> `Error er)
	  in
	  (match IList.bfold1ok starting process lt with
	  |`OK (lf, lt, lfg, var_num, l_stamp) ->
              let lgout = IListBasic.move_flexible lg
		  (IList.cons (AtIndex.it, f_ss c lf) lg) in
	      let lfg = (fcod, f_ee lgout h, l)::lfg in
              `OK (te_case fdom lt lg lf h, lfg, var_num, l_stamp)
	  |`Error er -> `Error er)
      | BCore.T_map t ->
          let (lg, l_stamp) = freshen_ilist l_stamp c IList.nil in 
          let (g', var_num) = fresh_ty_var var_num c in
          let lgin = IListBasic.move_flexible lg
	      (IList.cons (AtIndex.it, g') IList.nil) in
          let (h, var_num) = fresh_ty_var var_num c in
          let fee = f_ee lgin h in
	  (match el_valu var_num l_stamp fdom fee t with
	  |`OK (t, lfg, var_num, l_stamp) ->
 	      let b = c_PP (Cat.coi c_BB c) in
	      let (g, var_num) = fresh_ty_var var_num b in
              let lgout = IListBasic.move_flexible lg
		  (IList.cons (AtIndex.it, paf g' g) IList.nil) in 
	      let f_ee = f_ee lgout (paf h g) in
	      let lfg = (fcod, f_ee, l)::lfg in
	      `OK (te_map g t lgin h, lfg, var_num, l_stamp)
	  |`Error er -> `Error er)
      | BCore.T_con ->
	  let b = c_PP (Cat.coi c_BB c) in
	  let (g, var_num) = fresh_ty_var var_num b in
	  let lfg = [(fdom, paf (f_ii g) g, l); (fcod, f_ii g, l)] in
          `OK (t_con g, lfg, var_num, l_stamp)
      | BCore.T_fold t ->
          let (lg, l_stamp) = freshen_ilist l_stamp c IList.nil in 
	  let b = c_PP (Cat.coi c_BB c) in
          let (g, var_num) = fresh_ty_var var_num b in
          let (h, var_num) = fresh_ty_var var_num c in
          let hg = paf h g in
          let lgin = IListBasic.move_flexible lg
	    (IList.cons (AtIndex.it, hg) IList.nil) in
          let fee = f_ee lgin h in
	  (match el_valu var_num l_stamp fdom fee t with
	  |`OK (t, lfg, var_num, l_stamp) ->
              let lgout = IListBasic.move_flexible lg
		  (IList.cons (AtIndex.it, f_ii g) IList.nil) in 
	      let lfg = (fcod, f_ee lgout h, l)::lfg in
	      `OK (te_fold g t lgin h, lfg, var_num, l_stamp)
	  |`Error er -> `Error er)
      | BCore.T_de ->
	  let b = c_PP (Cat.coi c_BB c) in
	  let (g, var_num) = fresh_ty_var var_num b in
	  let lfg = [(fcod, paf (f_ii g) g, l); (fdom, f_ii g, l)] in
          `OK (t_de g, lfg, var_num, l_stamp)
      | BCore.T_uncon ->
	  let b = c_PP (Cat.coi c_BB c) in
	  let (g, var_num) = fresh_ty_var var_num b in
	  let lfg = [(fdom, paf (f_tt g) g, l); (fcod, f_tt g, l)] in
          `OK (t_uncon g, lfg, var_num, l_stamp)
      | BCore.T_unfold t ->
          let (lg, l_stamp) = freshen_ilist l_stamp c IList.nil in 
	  let b = c_PP (Cat.coi c_BB c) in
          let (g, var_num) = fresh_ty_var var_num b in
          let (h, var_num) = fresh_ty_var var_num c in
          let hg = paf h g in
          let lgin = IListBasic.move_flexible lg
	      (IList.cons (AtIndex.it, h) IList.nil) in
          let fee = f_ee lgin hg in
	  (match el_valu var_num l_stamp fdom fee t with
	  |`OK (t, lfg, var_num, l_stamp) ->
              let lgout = lgin in 
	      let lfg = (fcod, f_ee lgout (f_tt g), l)::lfg in
	      `OK (te_unfold g t lgin hg, lfg, var_num, l_stamp)
          |`Error er -> `Error er)	      
      | BCore.T_unde ->
	  let b = c_PP (Cat.coi c_BB c) in
	  let (g, var_num) = fresh_ty_var var_num b in
	  let lfg = [(fcod, paf (f_tt g) g, l); (fdom, f_tt g, l)] in
          `OK (t_unde g, lfg, var_num, l_stamp)
      | BCore.T_appl (lt, t) ->
	  (match el_valu_ilist_fresh_var var_num l_stamp fdom lt with
	  |`OK (lg, lt, lfg1, var_num, l_stamp) ->
              (match el_valu var_num l_stamp fdom (f_ee lg fcod) t with
              |`OK (t, lfg2, var_num, l_stamp) ->
		  let lfg = lfg1 @ lfg2 in
		  `OK (te_appl lt t lg fcod, lfg, var_num, l_stamp)
              |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | BCore.T_curry (lig, t) ->
	  (match 
	    IList.vfold1ok IList.nil (fun (k, f) r ->
	      if IdIndex.is_patt k then
		let lf = 
		  (match BCore.term_typ f with
		  | BCore.F_pp lf -> lf
		  | _ -> failwith "ElabBCore.el_valu: T_curry (1)")
		in
		let lg = IList.ifilter (fun i -> IList.is_in i r) lf in
		if not (IList.is_nil lg) then
		  `Error (coreFrontError#instance 
			    [Loc l; 
			     Msg ("duplicated argument names " 
				  (*^ PpFFunct.pp_lf lg*))])
		else 
		  `OK (IList.(@@) lf r)
	      else 
		if IList.is_in k r then 
		  `Error (coreFrontError#instance 
			    [Loc l; 
			     Msg ("duplicated argument name " 
				  ^ IdIndex.t2string k)])
		else 
		  `OK (IList.cons (k, f) r)) lig
	  with 
	    `OK lgk ->
	      (match el_typ_ilist var_num c lgk with
	      |`OK (lgk, var_num) ->
		  let (lfh, l_stamp) = freshen_ilist l_stamp c IList.nil in
		  let lgkf = IListBasic.move_flexible lfh lgk in
		  let local_fdom = f_pp c lgkf in
		  let (h, var_num) = fresh_ty_var var_num c in
		  (match el_valu var_num l_stamp local_fdom h t with
		  |`OK (t, lfg, var_num, l_stamp) ->
		      let lig = IList.vmap (fun (k, f) -> 
			if IdIndex.is_patt k then
			  let lf = 
			    (match BCore.term_typ f with
			    | BCore.F_pp lf -> lf
			    | _ -> failwith "ElabBCore.el_valu: T_curry (2)")
			  in
			  let lh = IList.imap (fun i -> IList.find i lgk) lf in
			  (k, f_pp c lh)
			else (k, IList.find k lgk)) lig in
		      let lgj = IList.bfold IList.nil (fun (j, (k, f)) r ->
			IList.cons (j, f) r) lig in
		      let lfg = (fcod, f_ee lgj h, l)::
			(fdom, f_pp c lfh, l)::lfg in
		      `OK (te_curry fdom lig t, lfg, var_num, l_stamp) 
		  |`Error er -> `Error er)
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | BCore.T_pappl (lt, t) ->
	  (match el_valu_ilist_fresh_var var_num l_stamp fdom lt with
	  |`OK (lg_arg, lt, lfg1, var_num, l_stamp) ->
              let (h, var_num) = fresh_ty_var var_num c in
	      let (lg_left, l_stamp) = freshen_ilist l_stamp c IList.nil in
	      let lg = IListBasic.move_flexible lg_left lg_arg in
              (match el_valu var_num l_stamp fdom (f_ee lg h) t with
              |`OK (t, lfg2, var_num, l_stamp) ->
		  let lfg = (fcod, f_ee lg_left h, l) :: lfg1 @ lfg2 in
		  `OK (t_pappl lt t lg h, lfg, var_num, l_stamp)
              |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | BCore.T_assert (u, t) -> 
	  let type_bool = 
	    f_ss c (IList.cons (AtIndex.tt, f_pp c (IList.nil))
		      (IList.cons (AtIndex.ff, f_pp c (IList.nil))
			 IList.nil))
	  in
	  (match el_valu var_num l_stamp fdom type_bool u with
	  |`OK (u, lfg1, var_num, l_stamp) ->
	      (match el_valu var_num l_stamp fdom fcod t with
	      |`OK (t, lfg2, var_num, l_stamp) ->
		  let lfg = lfg1 @ lfg2 in
		  let tas = if !check_assert then t_assert u t l else t in
		  `OK (tas, lfg, var_num, l_stamp) 
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      | BCore.T_fail ->
	  `OK (t_fail fdom fcod l, [], var_num, l_stamp)
      | BCore.T_fix (i, t) ->
	  let (lf, l_stamp) = freshen_ilist l_stamp c IList.nil in
          let lhf = IListBasic.move_flexible lf
	      (IList.cons (i, fcod) lf) in
          let local_fdom = f_pp c lhf in
          (match el_valu var_num l_stamp local_fdom fcod t with
          |`OK (t, lfg, var_num, l_stamp) ->
	      let lfg = (fdom, f_pp c lf, l)::lfg in
	      `OK (te_fix fdom i t fcod, lfg, var_num, l_stamp)
	  |`Error er -> `Error er)

    and el_valu_ilist_fresh_var var_num l_stamp fdom lt =
      let c = src fdom in
      let starting = 
	(IList.nil, IList.nil, [], var_num, l_stamp) in
      let process = fun (i, t) (lvar, lt, lfg1, var_num, l_stamp) ->
        let (fcod, var_num) = fresh_ty_var var_num c in
	(match el_valu var_num l_stamp fdom fcod t with
	|`OK (t, lfg2, var_num, l_stamp) ->
	    let lfg = lfg1 @ lfg2 in
            `OK (IList.cons (i, fcod) lvar, IList.cons (i, t) lt, 
		 lfg, var_num, l_stamp)
	|`Error er -> `Error er)
      in IList.bfold1ok starting process lt

    let elab_kind c = c_BB

    let elab_typ c f = 
      (match el_typ VarStamp.first c f with
      |	`OK (f, var_num) -> 
	  assert (var_num = VarStamp.first);
	  `OK f
      |`Error er -> `Error er)

    let elab_valu fdom fcod t =
      let fdom = f_finish fdom in
      let fcod = f_finish fcod in
      let result =
	el_valu VarStamp.first IListBasic.IListStamp.first fdom fcod t
      in
      match result with
      |`OK (t, lfg, var_num, l_stamp) ->
	  (match UnifyFCore.unify var_num l_stamp lfg with
	  |`OK (_, s, _, _) ->
	      let s = SubFFunct.finish_sub var_num l_stamp s in
	      let t = SubTTrans.sub_finish_t s t in
	      assert (ass result fdom fcod s t);
	      `OK t
	  |`Error er -> `Error er)
      |`Error er -> `Error er

    let elab_core prelude t =
      let var_num = VarStamp.first in
      let c = c_PP IList.nil in
      let fdom = f_finish (DomFCore.cod prelude) in
      let (fcod, var_num) = fresh_ty_var var_num c in
      let result =
	el_valu var_num IListBasic.IListStamp.first fdom fcod t
      in
      match result with
      |`OK (t, lfg, var_num, l_stamp) ->
	  (match UnifyFCore.unify var_num l_stamp lfg with
	  |`OK (_, s, _, _) ->
	      let s = SubFFunct.finish_sub var_num l_stamp s in
	      let f = SubFFunct.sub_finish_f s fcod in
	      let t = SubTTrans.sub_finish_t s t in
	      `OK (f, SemFTrans.t_comp prelude t)
	  |`Error er -> `Error er)
      |`Error er -> `Error er
  end

module ElabBCore = ElabBCore' (IdIndex) (AtIndex) (IListBasic) (IList) 
    (Location) (ErrorRepLib) (SemFCat) (SemFFunct) (NFTrans) (SemFTrans)
    (NormalConTTrans) (SrcFCore) (PpFFunct) (PpFTrans) (EqFCat) (EqFFunct)
    (SubFFunct) (SubTTrans) (UnifyFCore) (BCore) (DomFCore)
