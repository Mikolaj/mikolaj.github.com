(* Copyright (C) 2003--2006 Mikolaj Konarski
 * Copyright (C) 2007 Comarch SA
 *
 * This file is part of the Dule compiler.
 * The Dule compiler is released under the GNU General Public License (GPL).
 * Please see the file Dule-LICENSE for license information.
 *
 * $Id: mod_back.ml,v 1.123 2007-09-21 20:17:26 mikon Exp $
 *) 

open Middle_middle open Error_rep open Tools open Core_back open Core_front

module type Sign =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T

    type t
    val f2s : Funct.t -> t
(*
   the application [f2s f] results in a signature 
   iff [f] is of the form [f_pp (c_PP lc) lf]
*)
    val s2f : t -> Funct.t
    val type_part : t -> Cat.t IList.t
    val value_part : t -> Funct.t IList.t
  end

module Sign' 
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : ConPCat
    with module IdIndex = IdIndex 
    with module IList = IList)
    (Funct : ConPFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (SrcFCore : SrcFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with type Funct.t = Funct.t)
    : (Sign
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat = Cat
    module Funct = Funct

    type t = Funct.t

    let f2s f = 
      assert
        (match Funct.unpp_ok f with
        |`OK _ ->
            (match Cat.unPPok (SrcFCore.src f) with
            |`OK _ -> true
            |`Error er -> false)
        |`Error er -> false
        );
      f

    let s2f s = s

    let type_part s = Cat.unPP (SrcFCore.src s)

    let value_part s = Funct.unpp s
  end

module Sign = Sign' (IdIndex) (IList) (ConPCat) (ConPFunct) (SrcFCore)


module type Dule =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Trans : T
    module Sign : T

    type t
    val pack : Funct.t * Trans.t * Sign.t -> t
(*
   the application [pack (f, t, s)] results in a module 
   iff there is a signature [r] such that
   1. f : src r -> src s
   2. t : r -> f_COMP f s
*)
    val domain : t -> Sign.t
    val codomain : t -> Sign.t
    val type_part : t -> Funct.t
    val value_part : t -> Trans.t
  end

module Dule'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : ConPCat
    with module IdIndex = IdIndex 
    with module IList = IList)
    (Funct : ConPFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : ConPTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (Sign : Sign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (EqFCat : EqFCat with type Cat.t = Cat.t)
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
    : (Dule
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct
    with module Trans = Trans
    with module Sign = Sign) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat = Cat
    module Funct = Funct
    module Trans = Trans
    module Sign = Sign

    type t = Funct.t * Trans.t * Sign.t

    let pack (f, t, s) = 
      let g = DomFCore.dom t in
      let r = Sign.f2s g in
      let h = Sign.s2f s in
      assert 
        (EqFCat.eq (SrcFCore.src f) (SrcFCore.src g));
      assert 
        (EqFCat.eq (SrcFCore.trg f) (SrcFCore.src h));
      assert 
        (EqFFunct.eq (DomFCore.cod t) (Funct.f_COMP f h));
      (f, t, s)

    let domain (f, t, s) = Sign.f2s (DomFCore.dom t)

    let codomain (f, t, s) = s

    let type_part (f, t, s) = f

    let value_part (f, t, s) = t
  end

module Dule = Dule' (IdIndex) (IList) (ConPCat)
    (ConPFunct) (ConPTrans) (Sign) (EqFCat) (EqFFunct) (SrcFCore) (DomFCore)


module type PartitionWSign = (* W-Dule --- module system with specialization *)
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Sign : T

    val partition : Sign.t -> Cat.t IList.t * Cat.t IList.t
  end

module PartitionWSign' 
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : ConPCat
    with module IdIndex = IdIndex 
    with module IList = IList)
    (Funct : ConPFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Sign : Sign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    : (PartitionWSign
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct
    with module Sign = Sign) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat = Cat
    module Funct = Funct
    module Sign = Sign
    open IList

    let partition s =
      let le = Sign.type_part s in
      let lty = bfilter (fun (i, c) -> 
	Cat.isBB c
	(* a hack for the complex structures *)
        || IdIndex.eq AtIndex.atc i) le 
      in
      let lep = diff le lty in
      (* if [s] has been [S_Pp] (perhaps inside [S_Ww]) *)
      (* then [lty] is [nil] else [lePp] is [nil]: *)
      let lh = Sign.value_part s in
      let lePp = ifilter (fun i -> is_in i lh) lep in
      let la = lty @@ lePp in (* local types of [s] *)
      let lb = diff lep lePp in (* context types of [s] *)
      (la, lb)
  end

module PartitionWSign = 
  PartitionWSign' (IdIndex) (AtIndex) (IList) (ConPCat) (ConPFunct) (Sign)


module type TypesWSign = 
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Sign : T

    val typesPp : Sign.t IList.t -> 
      [`OK of Cat.t IList.t|`Error of string]
    val typesBb : Sign.t -> Cat.t IList.t -> 
      [`OK of Cat.t IList.t|`Error of string]
    val typesWw : Funct.t -> Sign.t ->
      [`OK of Cat.t IList.t * Cat.t IList.t * Cat.t IList.t 
          * Cat.t IList.t * Cat.t IList.t
      |`Error of string]
  end

module TypesWSign' 
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : ConPCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : ConPFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Sign : Sign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (SrcFCore : SrcFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with type Funct.t = Funct.t)
    (PpFCat : PpFCat
    with module Cat.IdIndex = IdIndex
    with module Cat.IList = IList
    with type Cat.t = Cat.t)
    (EqFCat : EqFCat with type Cat.t = Cat.t)
    (PartitionWSign : PartitionWSign
    with module IdIndex = IdIndex
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Sign.t = Sign.t)
    : (TypesWSign
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct
    with module Sign = Sign) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat = Cat
    module Funct = Funct
    module Sign = Sign
    open IList
    open Cat

    let append_cat l k = append_eq EqFCat.eq l k

    let typesPp ls = (* type part of [S_Pp(ls)] *)
      let name_local (i, s) r =
        let (la, lb) = PartitionWSign.partition s in
        let l1 = cons (i, c_PP la) nil in
        (match append_cat l1 lb with
        |`OK lc ->
            append_cat lc r
        |`Error er -> `Error er)
      in
      bfold1ok nil name_local ls

    let typesBb r la = (* type part of [S_Bb(r, la, lf)] *)
      let ld = Sign.type_part r in (* some will be hidden *)
      if vforall Cat.isPP ld then
        let lb = subtract ld la in (* context types *)
        if vforall Cat.isBB la then
          (* we merge local and context types: *)
          `OK (la @@ lb)
        else `Error "typesBb: some local types are not C_BB"
      else `Error "typesBb: some context types are not C_PP"

    let typesWw f1 s2 = 
      (* f1 = Dule.type_part m1, 
         m1 : r1 -> s1
       *)
      let lc1 = unPP (SrcFCore.src f1) in (* type part of [r1] *)
      let le1 = unPP (SrcFCore.trg f1) in (* type part of [s1] *)
      let (la, lb) = PartitionWSign.partition s2 in
      if subset EqFCat.eq lb le1 then
        (match append_cat la lc1 with
        |`OK lc -> (* type part of [S_Ww (m1, s2)] *)
            `OK (lc1, le1, la, lb, lc)
        |`Error er -> `Error er)
      else `Error ("typesWw: lb =\n" ^ PpFCat.pp_lc lb ^ 
                   "\nnot contained in le1 =\n" ^ PpFCat.pp_lc le1)
  end

module TypesWSign = TypesWSign' (IdIndex) (IList) (ConPCat) (ConPFunct)
    (Sign) (SrcFCore) (PpFCat) (EqFCat) (PartitionWSign) 


module type FootWSign =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Sign : T

    val footPp : Cat.t IList.t -> IdIndex.t -> Sign.t -> Funct.t
  end

module FootWSign' 
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : ConPCat
    with module IdIndex = IdIndex 
    with module IList = IList)
    (Funct : ConPFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Sign : Sign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    : (FootWSign
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct
    with module Sign = Sign) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat = Cat
    module Funct = Funct
    module Sign = Sign
    open IList
    open Cat
    open Funct

    let footPp lc i s = 
      (* [lc] is the type part of a product signature
         of which [s] is the component at label [i] 
       *)
      let le = Sign.type_part s in
      let la = unPP (find i lc) in (* labels of local types of [s] *)
      let lb = subtract le la in (* context types of [s] *)
      let la = diff le lb in (* local types of [s] --- unPP not unique! *)
      let f_PR_lc_i = f_PR lc i in
      let pia = imap (fun j -> 
        f_COMP f_PR_lc_i (f_PR la j)) la in
      let pib = imap (f_PR lc) lb in
      let c = c_PP lc in
      f_RECORD c (pia @@ pib)
  end

module FootWSign = 
  FootWSign' (IdIndex) (IList) (ConPCat) (ConPFunct) (Sign)


module type SemWSign =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Location : Location
    module ErrorRepLib : ErrorRepLib
    with module Location = Location
    module Cat : T
    module Funct : T
    module Trans : T
    module Sign : T
    module Dule : T

    val s_Pp : Sign.t IList.t -> [`OK of Sign.t|`Error of string]
    val s_Bb : Sign.t -> Cat.t IList.t -> Funct.t IList.t -> 
      [`OK of Sign.t|`Error of string]
    val sc_Bb : Sign.t -> Cat.t IList.t -> 
      (Cat.t -> [`OK of Funct.t|`Error of ErrorRepLib.error]) IList.t -> 
        Location.t ->
          [`OK of Sign.t|`Error of ErrorRepLib.error]
    val s_Ww : Dule.t -> Sign.t -> [`OK of Sign.t|`Error of string]
  end

module SemWSign'
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Location : Location)
    (ErrorRepLib : ErrorRepLib
    with module Location = Location)
    (Cat : ConPCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : ConPFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : T)
    (Sign : Sign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (Dule : Dule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with module Sign = Sign)
    (SrcFCore : SrcFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with type Funct.t = Funct.t)
    (TypesWSign : TypesWSign
    with module IdIndex = IdIndex
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Sign.t = Sign.t)
    (FootWSign : FootWSign
    with module IdIndex = IdIndex
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Sign.t = Sign.t)
    (EqFCat : EqFCat with type Cat.t = Cat.t)
    : (SemWSign
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with module Cat = Cat
    with module Funct = Funct
    with module Trans = Trans
    with module Sign = Sign
    with module Dule = Dule) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Location = Location
    module ErrorRepLib = ErrorRepLib
    module Cat = Cat
    module Funct = Funct
    module Trans = Trans
    module Sign = Sign
    module Dule = Dule
    open IList
    open Cat
    open Funct
    open ErrorRepLib

    let s_Pp ls = 
      (match TypesWSign.typesPp ls with
      |`OK lc -> 
          let legPp (i, s) = 
            let foot = FootWSign.footPp lc i s in
            f_COMP foot (Sign.s2f s)
          in
          let legs = bmap legPp ls in
          let c = c_PP lc in
          let body = f_pp c legs in
          `OK (Sign.f2s body)
      |`Error er -> `Error er)

    let s_Bb r la lf =
      (* [la] are local types of this signature,
         [lf] is the value part of this signature
       *)
      (match TypesWSign.typesBb r la with
      |`OK lc -> (* type part of the signature *)
          let c = c_PP lc in
          let ld = vmap SrcFCore.src lf in
          if vforall (EqFCat.eq c) ld then
            let le = vmap SrcFCore.trg lf in
            if vforall (EqFCat.eq c_BB) le then
              `OK (Sign.f2s (f_pp c lf))
            else `Error "types are of compound kinds in s_Bb"
          else `Error "types depend on wrong modules in s_Bb"
      |`Error er -> `Error er)          

    let sc_Bb r la l_c2f l = 
      (* [la] are local types of this signature,
         [l_c2f c] have source [c] and target [C_BB] 
       *)
      (match TypesWSign.typesBb r la with
      |`OK lc ->
          let c = c_PP lc in
          (match vmap1ok (fun c2f -> c2f c) l_c2f with
          |`OK lf ->
              `OK (Sign.f2s (f_pp c lf))
          |`Error er -> `Error er)          
      |`Error er ->  
          `Error 
            (modBackError#instance 
               [Loc l; 
                Msg er]))

    let s_Ww m1 s2 =
      let f1 = Dule.type_part m1 in
      (match TypesWSign.typesWw f1 s2 with
      |`OK (lc1, le1, la, lb, lc) -> 
          let c = c_PP lc in
          let pic1 = imap (f_PR lc) lc1 in 
          let f = f_COMP (f_RECORD c pic1) f1 in
          let pib = imap (fun i -> 
            f_COMP f (f_PR le1 i)) lb in
          let pia = imap (f_PR lc) la in 
          let re = f_RECORD c (pib @@ pia) in
          let h = f_COMP re (Sign.s2f s2) in
          `OK (Sign.f2s h)
      |`Error er -> `Error er)
  end

module SemWSign = 
  SemWSign' (IdIndex) (AtIndex) (IList) (Location) (ErrorRepLib)
    (ConPCat) (ConPFunct) (ATrans) (Sign) (Dule) (SrcFCore)
    (TypesWSign) (FootWSign) (EqFCat)


module type OkWDule =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Trans : T
    module Sign : T

    val typesRecord : Funct.t IList.t -> Sign.t IList.t -> 
      [`OK of Funct.t IList.t|`Error of string]
    val typesBase : Sign.t -> Sign.t -> Funct.t IList.t -> 
      [`OK of Funct.t|`Error of string]
    val typesInst : Funct.t -> Funct.t -> Sign.t -> 
      [`OK of Funct.t|`Error of string]
    val typesTrim : Cat.t -> Cat.t -> [`OK of Funct.t|`Error of string]
    val valuesTrim : Funct.t -> Funct.t -> [`OK of Trans.t|`Error of string]
    val typesSlash : Sign.t -> Sign.t -> Sign.t -> 
      [`OK of Cat.t IList.t * Cat.t IList.t|`Error of string]
  end

module OkWDule' 
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : ConPCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : ConPFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : ConPTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (Sign : Sign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (SrcFCore : SrcFCore
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with type Funct.t = Funct.t)
    (PpFCat : PpFCat
    with module Cat.IdIndex = IdIndex
    with module Cat.IList = IList
    with type Cat.t = Cat.t)
    (PpFFunct : PpFFunct
    with type Funct.t = Funct.t)
    (EqFCat : EqFCat with type Cat.t = Cat.t)
    (EqFFunct : EqFFunct with type Funct.t = Funct.t)
    (PartitionWSign : PartitionWSign
    with module IdIndex = IdIndex
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Sign.t = Sign.t)
    (TypesWSign : TypesWSign
    with module IdIndex = IdIndex
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Sign.t = Sign.t)
    : (OkWDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct
    with module Trans = Trans
    with module Sign = Sign) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat = Cat
    module Funct = Funct
    module Trans = Trans
    module Sign = Sign
    open IList
    open Cat
    open Funct
    open Trans

    let append_funct l k = append_eq EqFFunct.eq l k

    let typesRecord lf ls =
      let cut_at_i (i, f) r =
        let s = find i ls in
        let (la, lb) = PartitionWSign.partition s in
        let le = la @@ lb in
        let e = c_PP le in
        let lfib = imap (fun i -> f_COMP f (f_PR le i)) lb in
        let pia = imap (f_PR le) la in
        let fia = f_COMP f (f_RECORD e pia) in
        let l1 = cons (i, fia) nil in
        (match append_funct l1 lfib with
        |`OK lg ->
            append_funct lg r
        |`Error er -> `Error er)
      in
      bfold1ok nil cut_at_i lf

    let typesBase r s lg =
      let lc = Sign.type_part r in
      let c = c_PP lc in
      let ld = vmap SrcFCore.src lg in
      if vforall (EqFCat.eq c) ld then
        let (la, lb) = PartitionWSign.partition s in
        let le = vmap SrcFCore.trg lg in
        if eqset EqFCat.eq le la then
          if subset EqFCat.eq lb lc then
            let pilb = imap (f_PR lc) lb in 
            let lf = lg @@ pilb in
            let f = f_RECORD c lf in
            `OK f
          else `Error "typesBase: context types not provided"
        else `Error ("typesBase: type definitions do not agree with codomain"
                     ^ "\nle" ^ PpFCat.pp_lc le 
                     ^ ") <> \nla(" ^ PpFCat.pp_lc la ^ ")")
      else `Error "typesBase: type definitions depend on wrong modules"

    let typesInst f1 f2 s2 = 
      (match TypesWSign.typesWw f1 s2 with
      |`OK (lc1, le1, la, lb, _) -> 
          let le2 = la @@ lb in (* type part of [s2] *)
          let f12 = f_COMP f1 f2 in
          let pia = imap (fun i -> 
            f_COMP f12 (f_PR le2 i)) la in 
          let pic1 = imap (f_PR lc1) lc1 in 
          (match append_funct pia pic1 with
          |`OK lf ->
              let pib = imap (fun i -> 
                f_COMP f2 (f_PR le2 i)) lb in 
              let pie1 = imap (f_PR le1) lb in 
              if eqset EqFFunct.eq pib pie1 then
                `OK (f_RECORD (c_PP lc1) lf)
              else `Error "typesInst: context types changed in f2"
          |`Error er -> `Error er)
      |`Error er -> `Error er)

    let rec typesTrim e c = 
      if EqFCat.eq e c then 
        `OK (f_ID c) 
      else
      (match unPPok c with
      |`OK lc ->
          (match unPPok e with
          |`OK le ->
              let fsu (i, c) =
                (match find_ok i le with
                |`OK e ->
                    (match typesTrim e c with
                    |`OK sf ->
                        `OK (f_COMP (f_PR le i) sf)
                    |`Error er -> `Error er)
                |`Error er -> 
                    `Error ("typesTrim: " ^ IdIndex.t2string i ^ 
                            " not in le"))
              in
              (match bmap1ok fsu lc with
              |`OK lf ->
                  `OK (f_RECORD e lf)
              |`Error er -> `Error er)
          |`Error er -> `Error "typesTrim: e not C_PP")
      |`Error er -> `Error "typesTrim: e <> c")

    let rec valuesTrim h f = 
      if EqFFunct.eq h f then 
        `OK (t_id f) 
      else
      (match unpp_ok f with
      |`OK lf ->
          (match unpp_ok h with
          |`OK lh ->
              let fsu (i, f) =
                (match find_ok i lh with
                |`OK h ->
                    (match valuesTrim h f with
                    |`OK sf ->
                        `OK (t_comp (t_pr lh i) sf)
                    |`Error er -> `Error er)
                |`Error er ->
                    `Error ("valuesTrim: " ^ IdIndex.t2string i 
                            ^ " not in lh"))
              in
              (match bmap1ok fsu lf with
              |`OK lt ->
                  `OK (t_record h lt)
              |`Error er -> `Error er)
          |`Error er -> `Error "valuesTrim: h not f_pp")
      |`Error er -> `Error ("valuesTrim: \nh(" 
                            ^ PpFFunct.pp_f h ^ ") <> \nf("
                            ^ PpFFunct.pp_f f ^ ")"))

    let typesSlash s1 r2 s2 =
      let le1 = Sign.type_part s1 in
      let (la, lb) = PartitionWSign.partition r2 in
      if subset EqFCat.eq lb le1 then
        let (la2, lb2) = PartitionWSign.partition s2 in
        `OK (lb, lb2)
      else `Error "typesSlash: lb not contained in le1"
  end

module OkWDule = OkWDule' (IdIndex) (IList) (ConPCat) (ConPFunct) (ConPTrans)
    (Sign) (SrcFCore) (PpFCat) (PpFFunct) (EqFCat) (EqFFunct) (PartitionWSign)
    (TypesWSign)


module type SemWDule =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Location : Location
    module ErrorRepLib : ErrorRepLib
    with module Location = Location
    module Cat : T
    module Funct : T
    module Trans : T
    module Sign : T
    module Dule : T

    val m_Id : Sign.t -> Dule.t
    val m_Comp : Dule.t -> Dule.t -> Dule.t
    val m_Pr : Sign.t IList.t -> IdIndex.t -> 
                 [`OK of Dule.t|`Error of string]
    val m_Record : Sign.t -> Dule.t IList.t ->
                     [`OK of Dule.t|`Error of string]
    val m_Base : Sign.t -> Sign.t ->
                   Funct.t IList.t -> Trans.t IList.t ->
                     [`OK of Dule.t|`Error of string]
    val mc_Base : Sign.t -> Sign.t -> 
      (Cat.t -> [`OK of Funct.t|`Error of ErrorRepLib.error]) IList.t ->
        (Funct.t -> Funct.t -> 
          [`OK of Trans.t|`Error of ErrorRepLib.error]) IList.t ->
            Location.t ->
              [`OK of Dule.t|`Error of ErrorRepLib.error]
    val m_Inst : Dule.t -> Dule.t -> 
                   [`OK of Dule.t|`Error of string]
    val m_Trim : Dule.t -> Sign.t -> 
                   [`OK of Dule.t|`Error of string]
    val m_Slash : Dule.t -> Sign.t -> Sign.t -> 
                    [`OK of Dule.t|`Error of string]
  end

module SemWDule'
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Location : Location)
    (ErrorRepLib : ErrorRepLib
    with module Location = Location)
    (Cat : ConPCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : ConPFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : ConPTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (Sign : Sign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (Dule : Dule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with module Sign = Sign)
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
    (EqFCat : EqFCat with type Cat.t = Cat.t)
    (EqFFunct : EqFFunct with type Funct.t = Funct.t)
    (PpFFunct : PpFFunct 
    with type Funct.t = Funct.t
    with module Funct.IdIndex = IdIndex
    with module Funct.IList = IList)
    (FootWSign : FootWSign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Sign.t = Sign.t)
    (SemWSign : SemWSign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with type Sign.t = Sign.t
    with type Dule.t = Dule.t)
    (OkWDule : OkWDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with type Sign.t = Sign.t)
    : (SemWDule
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with module Cat = Cat
    with module Funct = Funct
    with module Trans = Trans
    with module Sign = Sign
    with module Dule = Dule) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Location = Location
    module ErrorRepLib = ErrorRepLib
    module Cat = Cat
    module Funct = Funct
    module Trans = Trans
    module Sign = Sign
    module Dule = Dule
    open IList
    open Cat
    open Funct
    open Trans
    open ErrorRepLib
(*
   Invariant: 
     If m : r -> s type-correct then [m] is a module with [r], [s].
*)
    let m_Id s = (* : s -> s *)
      let h = Sign.s2f s in
      let c = SrcFCore.src h in
      Dule.pack (f_ID c, t_id h, s)

    let m_Comp m1 m2 = (* : r1 -> s2 *)
      let f1 = Dule.type_part m1 (* : r1 -> s1 *) in
      let t1 = Dule.value_part m1 in
      let f2 = Dule.type_part m2 (* : r2 -> s2 *) in
      let t2 = Dule.value_part m2 in
      let f = f_COMP f1 f2 in (* s1 = r2 *)
      let it2 = t_FT f1 t2 in
      let t = t_comp t1 it2 in
      let s2 = Dule.codomain m2 in
      Dule.pack (f, t, s2)
(*
The following drawing shows the domains and codomains
of the transformations appearing in the definition of [m_Comp].

       t1    f1         t2    f2         t    f1
  r1 ------> --    r2 ------> --    r1 -----> -- 
             s1               s2              f2
                                              --
                                              s2

The drawing below illustrates the value part 
of the result of module composition.
Horizontal composition of transformations is here represented 
by placing the first transformation above the second.
Vertical composition is represented by sharing a common domain/codomain.

                 t_id
       t1    f1 ------> f1           
  r1 ------> --         --
                        f2
             s1 ------> --       s1 = r2
                  t2    s2

*)
    let m_Pr lr i = (* : S_Pp lr -> s *)
      (match find_ok i lr with
      |`OK s -> 
          (match SemWSign.s_Pp lr with
          |`OK r -> 
              let lc = Sign.type_part r in
              let foot_i = FootWSign.footPp lc i s in
              let legs = Sign.value_part r in
              let t = t_pr legs i in
              `OK (Dule.pack (foot_i, t, s))
          |`Error er -> `Error er)
      |`Error er -> `Error er)

    let m_Record r lm = (* : r -> S_Pp ls *)
      let lf = vmap Dule.type_part lm (* : r -> s_i *) in
      let lt = vmap Dule.value_part lm in
      let ls = vmap Dule.codomain lm in
      (match OkWDule.typesRecord lf ls with
      |`OK lf ->
          let g = Sign.s2f r in
          let c = SrcFCore.src g in
          let f = f_RECORD c lf in 
          let t = t_record g lt in
          (match SemWSign.s_Pp ls with
          |`OK s ->
              `OK (Dule.pack (f, t, s))
          |`Error er -> `Error er)
      |`Error er -> `Error er)

    let m_Base r s lg lt = (* : r -> s *)
      (match OkWDule.typesBase r s lg with
      |`OK f ->
          let g = Sign.s2f r in
          let lgt = vmap DomFCore.dom lt in
          if vforall (EqFFunct.eq g) lgt then
            let t = t_record g lt in
            let ts = DomFCore.cod t in
            let h = Sign.s2f s in
            let fs = f_COMP f h in
            if EqFFunct.eq ts fs then
              `OK (Dule.pack (f, t, s))
            else `Error 
                "value definitions do not agree with codomain in m_Base"
          else `Error "values depend on wrong modules in m_Base"
      |`Error er -> `Error er)

    (* f in l_c2g && f c = OK g => src g = c *)
    (* t in l_fh2t && t f h = OK u => dom u = f && cod u = h *)
    let mc_Base r s l_c2g l_fh2t l = (* : r -> s *)
      let g = Sign.s2f r in
      let c = SrcFCore.src g in
      (match vmap1ok (fun c2f -> c2f c) l_c2g with
      |`OK lg ->
          (match OkWDule.typesBase r s lg with
          |`OK f ->
              let h = Sign.s2f s in
              let lh = unpp (f_COMP f h) in
              let fih (i, fh2t) = 
                (match find_ok i lh with
                |`OK h -> fh2t g h
                |`Error er ->
                    `Error 
                      (modBackError#instance 
                         [Loc l; 
                          Msg (er ^ " not found in specification")]))
              in
              (match bmap1ok fih l_fh2t with
              |`OK lt -> 
                  let lht = vmap DomFCore.cod lt in
                  let not_defined = diff lh lht in
                  if is_nil not_defined then
                    let t = t_record g lt in
                    `OK (Dule.pack (f, t, s))
                  else
                    `Error 
                      (modBackError#instance 
                         [Loc l; 
                          Msg ("values specified but not defined:\n" 
                               ^ PpFFunct.pp_lf not_defined)])
              |`Error er -> `Error er)
          |`Error er ->
              `Error 
                (modBackError#instance 
                   [Loc l; 
                    Msg er]))
      |`Error er -> `Error er)

    let m_Inst m1 m2 = (* : r1 -> S_Ww (m1, s2) *)
      let f1 = Dule.type_part m1 (* : r1 -> s1=r2 *) in
      let t1 = Dule.value_part m1 in
      let f2 = Dule.type_part m2 (* : s1=r2 -> s2 *) in
      let t2 = Dule.value_part m2 in
      let s2 = Dule.codomain m2 in
      (match OkWDule.typesInst f1 f2 s2 with
      |`OK f -> 
          let it2 = t_FT f1 t2 in
          let t = t_comp t1 it2 in
          (match SemWSign.s_Ww m1 s2 with
          |`OK s ->
              `OK (Dule.pack (f, t, s))
          |`Error er -> `Error er)
      |`Error er -> `Error er)

    let m_Trim m1 r2 = (* : r1 -> r2 *)
      let f1 = Dule.type_part m1 (* : r1 -> s1 *) in
      let t1 = Dule.value_part m1 in
      let g2 = Sign.s2f r2 in
      let e1 = SrcFCore.trg f1 in (* [src s1] *)
      let c2 = SrcFCore.src g2 in
      (match OkWDule.typesTrim e1 c2 with
      |`OK scf ->
          let f = f_COMP f1 scf in
          let fcr2 = f_COMP f g2 in
          let f1s1 = DomFCore.cod t1 in (* = [f_COMP f1 s1] *)
          (match OkWDule.valuesTrim f1s1 fcr2 with (* weaker than s1 > r2 *)
          |`OK sct ->
              let t = t_comp t1 sct in
              `OK (Dule.pack (f, t, r2))
          |`Error er -> `Error er)
      |`Error er -> `Error er)

    let append_cat l k = append_eq EqFCat.eq l k

    let m_Slash m1 r2 s2 = (* : r -> s *) (* a _total_ hack *)
      (* we have [m1 | r2] and [r2] is [{i : s2; ...}] *)
      (* we want to get [m'] such that [m1 | r2 = {i : m' | s2; ...}] *)
      (* we have to trim [m1] to remove [i, ...] (changing only codomain) *)
      (* and extend to add dummy [i, ...] (changing also domain) *)
      let s1 = Dule.codomain m1 in
      (match OkWDule.typesSlash s1 r2 s2 with
      |`OK (lb, lb2) ->
          let la = diff lb2 lb in
          if is_nil la then
            `OK m1
          else
            let b = c_PP lb in
            let h = f_pp b nil in
            let s = Sign.f2s h in
            (match m_Trim m1 s with
            |`OK m ->
                let f = Dule.type_part m in
                let t = Dule.value_part m in
                let b2 = c_PP (lb @@ la) in
                let h2 = f_pp b2 nil in
                let s = Sign.f2s h2 in
                let r1 = Dule.domain m1 in
                let lc1 = Sign.type_part r1 in
                (match append_cat la lc1 with
                |`OK lc1a ->
                    let c1a = c_PP lc1a in
                    let lpc1 = imap (f_PR lc1a) lc1 in
                    let rpa = (f_RECORD c1a lpc1) in
                    let f = f_COMP rpa f in
                    let lf = imap (fun i -> f_COMP f (f_PR lb i)) lb in 
                    let lpa = imap (f_PR lc1a) la in
                    let lfa = lf @@ lpa in
                    let f = f_RECORD c1a lfa in
                    let t = t_FT rpa t in
                    `OK (Dule.pack (f, t, s))
                |`Error er -> `Error er) 
            |`Error er -> `Error er)
      |`Error er -> `Error er)
  end

module SemWDule = 
  SemWDule' (IdIndex) (AtIndex) (IList) (Location) (ErrorRepLib)
    (ConPCat) (ConPFunct) (ConPTrans) (Sign) (Dule) (SrcFCore) (DomFCore)
    (EqFCat) (EqFFunct) (PpFFunct) (FootWSign) (SemWSign) (OkWDule)


module type SemWDuleComplex =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Location : Location
    module ErrorRepLib : ErrorRepLib
    with module Location = Location
    module Cat : T
    module Funct : T
    module Trans : T
    module Sign : T
    module Dule : T

    val m_Type : Sign.t -> Funct.t ->
                     [`OK of Dule.t|`Error of string]
    val m_Value : Sign.t -> Trans.t ->
                     [`OK of Dule.t|`Error of string]
    val m_Base : Sign.t -> Sign.t -> 
                   Funct.t IList.t -> Trans.t IList.t ->
                     [`OK of Dule.t|`Error of string]
    val mc_Base : Sign.t -> Sign.t -> 
      (Cat.t -> [`OK of Funct.t|`Error of ErrorRepLib.error]) IList.t ->
        (Funct.t -> Funct.t -> 
          [`OK of Trans.t|`Error of ErrorRepLib.error]) IList.t ->
            Location.t ->
              [`OK of Dule.t|`Error of ErrorRepLib.error]
  end

module SemWDuleComplex'
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Location : Location)
    (ErrorRepLib : ErrorRepLib
    with module Location = Location)
    (Cat : ConPCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : ConPFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : ConPTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (Sign : Sign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (Dule : Dule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with module Sign = Sign)
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
    (EqFCat : EqFCat with type Cat.t = Cat.t)
    (EqFFunct : EqFFunct with type Funct.t = Funct.t)
    (PpFFunct : PpFFunct 
    with type Funct.t = Funct.t
    with module Funct.IdIndex = IdIndex
    with module Funct.IList = IList)
    (SemWSign : SemWSign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with type Sign.t = Sign.t
    with type Dule.t = Dule.t)
    (OkWDule : OkWDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with type Sign.t = Sign.t)
    (SemWDule : SemWDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with type Sign.t = Sign.t
    with type Dule.t = Dule.t)
    : (SemWDuleComplex
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with module Cat = Cat
    with module Funct = Funct
    with module Trans = Trans
    with module Sign = Sign
    with module Dule = Dule) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Location = Location
    module ErrorRepLib = ErrorRepLib
    module Cat = Cat
    module Funct = Funct
    module Trans = Trans
    module Sign = Sign
    module Dule = Dule
    open IList
    open Cat
    open Funct
    open Trans
    open ErrorRepLib

    let m_Type r g = (* : r -> s *)
      let lc = Sign.type_part r in
      let c = c_PP lc in
      let d = SrcFCore.src g in
      if EqFCat.eq c d then
        let e = SrcFCore.trg g in
        if isBB e then
          let pilc = imap (f_PR lc) lc in 
          let lf = cons (AtIndex.atc, g) pilc in
          let f = f_RECORD c lf in
          let h = Sign.s2f r in
          let t = t_record h nil in
	  let ld = cons (AtIndex.atc, c_BB) nil in
	  (match SemWSign.s_Bb r ld nil with
	  |`OK s ->
              `OK (Dule.pack (f, t, s))
          |`Error er -> `Error er)
        else `Error "mn_Type: local type is not C_BB"
      else `Error "mn_Type: type definition depends on wrong modules"

    let m_Value r t = (* : r -> s *)
      let h = Sign.s2f r in
      let g = DomFCore.dom t in
      if EqFFunct.eq h g then
        let f = f_ID (SrcFCore.src h) in
        let t = t_record h (cons (AtIndex.atc, t) nil) in
	let lh = unpp (DomFCore.cod t) in
	(match SemWSign.s_Bb r nil lh with
	|`OK s ->
            `OK (Dule.pack (f, t, s))
        |`Error er -> `Error er)
      else `Error "mn_Type: value definition depends on wrong modules"

    let iso m =
      let r = Dule.domain m in
      let f = Dule.type_part m in
      let t = Dule.value_part m in
      (* getting types out of [atc] wrappers *)
      let lc = Sign.type_part r in
      let c = c_PP lc in
      let ld = unPP (SrcFCore.trg f) in
      let (ld, f) = 
	if is_nil ld then 
          (* this is the only instance where [m_Record] differs from [m_Base] *)
	  (lc, f_ID c) 
	else (ld, f) 
      in
      assert (subset EqFCat.eq lc ld);
      let un_atc (i, e) =
	let le = unPP e in
        let g = f_COMP f (f_PR ld i) in
	if is_in AtIndex.atc le then
	  f_COMP g (f_PR le AtIndex.atc)
        else
          (* changed from [f_RECORD] so that [s_Bb] does not complain *)
          if not_in i lc then f_pp c nil else g
      in
      let lf = bmap un_atc ld in
      let f = f_RECORD c lf in
      let ld = unPP (SrcFCore.trg f) in
      (* getting values out of [atc] wrappers *)
      let g = DomFCore.dom t in
      let lh = unpp (DomFCore.cod t) in
      assert 
        (let le = vmap SrcFCore.trg lh in
        eqset (fun e c -> true) (lc @@ le) ld);
      let pr_atc (i, h) =
	let lhu = unpp h in
	let u = t_comp t (t_pr lh i) in
        if is_in AtIndex.atc lhu then
  	  t_comp u (t_pr lhu AtIndex.atc)
        else u
      in
      let lt = bmap pr_atc lh in
      let t = t_record g lt in
      (* creating the output signature *)
      let h = DomFCore.cod t in
      let lfc = imap (f_PR ld) lc in
      let g = f_RECORD (c_PP ld) lfc in
      let lh = unpp (f_COMP g h) in
      (match SemWSign.s_Bb r (diff ld lc) lh with
      |`OK s ->
          Dule.pack (f, t, s)
      |`Error er -> 
	  failwith ("iso: failed to create the output signature with\n" ^ er))
	
    let m_Base r s lg lt = (* : r -> s *)
      (match vmap1ok (m_Type r) lg with
      |`OK lgg ->
	  (match vmap1ok (m_Value r) lt with
	  |`OK ltt ->
	      (match SemWDule.m_Record r (lgg @@ ltt) with
	      |`OK m ->
		  SemWDule.m_Trim (iso m) s
	      |`Error er -> `Error er)
	  |`Error er -> `Error er)
      |`Error er -> `Error er)
	
    (* f in l_c2g && f c = OK g => src g = c *)
    (* t in l_fh2t && t f h = OK u => dom u = f && cod u = h *)
    let mc_Base r s l_c2g l_fh2t l = (* : r -> s *)
      let g = Sign.s2f r in
      let c = SrcFCore.src g in
      (match vmap1ok (fun c2f -> c2f c) l_c2g with
      |`OK lg ->
          (match OkWDule.typesBase r s lg with
          |`OK f ->
              let h = Sign.s2f s in
              let lh = unpp (f_COMP f h) in
              let fih (i, fh2t) = 
                (match find_ok i lh with
                |`OK h -> fh2t g h
                |`Error er ->
                    `Error 
                      (modBackError#instance 
                         [Loc l; 
                          Msg (er ^ " not found in specification")]))
              in
              (match bmap1ok fih l_fh2t with
              |`OK lt -> 
		  (match
                    let lht = vmap DomFCore.cod lt in
                    let not_defined = diff lh lht in
                    if is_nil not_defined then
		      (match vmap1ok (m_Type r) lg with
		      |`OK lgg ->
			  (match vmap1ok (m_Value r) lt with
			  |`OK ltt ->
			      (match SemWDule.m_Record r (lgg @@ ltt) with
			      |`OK m ->
				  SemWDule.m_Trim (iso m) s
			      |`Error er -> `Error er)
			  |`Error er -> `Error er)
		      |`Error er -> `Error er)
                    else
                      `Error ("values specified but not defined:\n" 
                              ^ PpFFunct.pp_lf not_defined)
		  with
		  |`OK m -> `OK m
		  |`Error er ->
		      `Error 
			(modBackError#instance 
			   [Loc l; 
			    Msg er]))
              |`Error er -> `Error er)
          |`Error er ->
              `Error 
                (modBackError#instance 
                   [Loc l; 
                    Msg er]))
      |`Error er -> `Error er)
  end

module SemWDuleComplex = 
  SemWDuleComplex' (IdIndex) (AtIndex) (IList) (Location) (ErrorRepLib)
    (ConPCat) (ConPFunct) (ConPTrans) (Sign) (Dule) (SrcFCore) (DomFCore)
    (EqFCat) (EqFFunct) (PpFFunct) (SemWSign) (OkWDule) (SemWDule)


module type SemLDule = (* L-Dule --- module system with linking *)
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Trans : T
    module Sign : T
    module Dule : T

    val m_Accord : Sign.t IList.t -> Dule.t IList.t ->
      [`OK of Dule.t|`Error of string]
    val m_Concord : Sign.t IList.t -> Dule.t IList.t ->
      [`OK of Dule.t|`Error of string]
    val m_Link : Sign.t IList.t -> Dule.t IList.t ->
      [`OK of Dule.t|`Error of string]
    val m_Link_ordered : Sign.t IList.t -> Dule.t IList.t ->
      [`OK of Dule.t|`Error of string]
  end

module SemLDule'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : T)
    (Funct : ConPFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : T)
    (Sign : Sign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (Dule : Dule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with module Sign = Sign)
    (SemWSign : SemWSign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with type Sign.t = Sign.t
    with type Dule.t = Dule.t)
    (SemWDule : SemWDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with type Sign.t = Sign.t
    with type Dule.t = Dule.t)
    : (SemLDule
    with module IdIndex = IdIndex
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct
    with module Trans = Trans
    with module Sign = Sign
    with module Dule = Dule) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat = Cat
    module Funct = Funct
    module Trans = Trans
    module Sign = Sign
    module Dule = Dule
    open IList

    let m_Accord lr lm = (* : S_Pp lr -> S_Pp ls *)
      (match SemWSign.s_Pp lr with
      |`OK r ->
          (match imap1ok (SemWDule.m_Pr lr) lr with
          |`OK lpr ->
              let prm m =
                let lf = Sign.value_part (Dule.domain m) in
                let lmf = imap (fun i -> find i lpr) lf in
                (match SemWDule.m_Record r lmf with
                |`OK re ->
                    `OK (SemWDule.m_Comp re m)
                |`Error er -> `Error er)
              in
              (match vmap1ok prm lm with
              |`OK lm ->
                  SemWDule.m_Record r lm
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      |`Error er -> `Error er)

    let m_Concord lr lm = (* : S_Pp lr -> S_Pp (ls @@ diff lr ls) *)
      (match SemWSign.s_Pp lr with
      |`OK r ->
          (match imap1ok (SemWDule.m_Pr lr) lr with
          |`OK lpr ->
              let prm m =
                let lf = Sign.value_part (Dule.domain m) in
                let lmf = imap (fun i -> find i lpr) lf in
                (match SemWDule.m_Record r lmf with
                |`OK re ->
                    `OK (SemWDule.m_Comp re m)
                |`Error er -> `Error er)
              in
              (match vmap1ok prm lm with
              |`OK lm ->
                  let lm = lm @@ subtract lpr lm in
                  SemWDule.m_Record r lm
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      |`Error er -> `Error er)

    (* here order of lm doesn't matter, 
       but no circularity allowed: *)
    let m_Link lr lm = (* : S_Pp lr -> Pp ls *)
      (match SemWSign.s_Pp lr with
      |`OK r ->
          (match imap1ok (SemWDule.m_Pr lr) lr with
          |`OK lpr ->
              let rec rlink i = 
                (match find_ok i lpr with
                |`OK pr -> `OK pr
                |`Error er ->
                    let m = find i lm in
                    let lf = Sign.value_part (Dule.domain m) in
                    (match imap1ok rlink lf with
                    |`OK lmf ->
                        (match SemWDule.m_Record r lmf with
                        |`OK re ->
                            `OK (SemWDule.m_Comp re m)
                        |`Error er -> `Error er)
                    |`Error er -> `Error er))
              in
              (match imap1ok rlink lm with
              |`OK lm ->
                  SemWDule.m_Record r lm
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      |`Error er -> `Error er)

    (* here we assume a module depends 
       only on the previous ones in lm: *)
    let m_Link_ordered lr lm = (* : S_Pp lr -> S_Pp ls *)
      (match SemWSign.s_Pp lr with
      |`OK r ->
          (match imap1ok (SemWDule.m_Pr lr) lr with
          |`OK lpr ->
              let pro = fun (i, m) lm ->
		if !Tools.debugging then 
		  (prerr_endline (" L:" ^ IdIndex.t2s i ^ " ");
		   flush stderr) else ();
                let lf = Sign.value_part (Dule.domain m) in
                (match imap1ok (fun i -> find_ok i lm) lf with
                |`OK lmf ->
                    (match SemWDule.m_Record r lmf with
                    |`OK re ->
                        let m = SemWDule.m_Comp re m in
                        `OK (cons (i, m) lm)
                    |`Error er -> `Error er)
                |`Error er -> 
                    `Error (IdIndex.t2string i 
                            ^ " depends on an unknown "
                            ^ er))
              in
              (match bfold1ok lpr pro lm  with
              |`OK lprlm ->
                  let lm = subtract lprlm lpr in
                  SemWDule.m_Record r lm
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      |`Error er -> `Error er)
  end

module SemLDule = SemLDule' (IdIndex) (IList) (ACat) (ConPFunct) (ATrans) 
    (Sign) (Dule) (SemWSign) (SemWDule)


module type XToolIDule = (* I-Dule --- module system with inductive modules *)
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Trans : T

    val repair : (Funct.t -> Funct.t) -> 
      (Funct.t -> Trans.t) -> (Funct.t -> Trans.t) -> 
        Trans.t IList.t -> Funct.t IList.t -> Funct.t -> Funct.t -> Cat.t -> 
          Trans.t
    val close_type : (Funct.t -> Funct.t) -> 
      (Cat.t -> Cat.t -> Cat.t IList.t) ->
        Funct.t -> Cat.t -> Cat.t IList.t -> 
          Funct.t
 end

module XToolIDule' 
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : ConPCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : ConPFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : ConPTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    : (XToolIDule
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
    open Cat
    open Funct
    open Trans

    let repair unii t_con t_de  lrta lbpr fi h a =
      let unf = unii fi in (* : c_PP (coi b a) -> b *)
      let fcon = t_con unf in
      let pifc = vmap (fun pr -> t_TF fcon pr) lbpr in
      let tc = t_RECORD a (lrta @@ pifc) in
      (* we do not use [t_TF tc h], because it is 
         not valid for [F_ee] and because we want 
         simple [dom] and [cod] of the result *)
      let fde = t_de unf in
      let pifd = vmap (fun pr -> t_TF fde pr) lbpr in
      let td = t_RECORD a (lrta @@ pifd) in
      t_TF_coco (tc, td) h

    let close_type f_ii coi  f b la = 
      (* f : cons (AtIndex.atr, b) la -> b *)
      let lapr = imap (f_PR la) la in
      let a = c_PP la in
      let lba = coi b a in
      let prba = f_PR lba AtIndex.atk in
      let lpr = vmap (fun pr -> f_COMP prba pr) lapr in
      let lcopr = cons (AtIndex.atr, f_PR lba AtIndex.atj) lpr in
      let rba = f_RECORD (c_PP lba) lcopr in
      let fca = f_COMP rba f in
      f_ii fca (* : a -> b *)
  end

module XToolIDule = XToolIDule' (IdIndex) (AtIndex) (IList) (ConPCat)
    (ConPFunct) (ConPTrans)


module type ToolIDule =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Trans : T

    val repair_ii : 
        Trans.t IList.t -> Funct.t IList.t -> Funct.t -> Funct.t -> Cat.t -> 
          Trans.t
    val repair_tt : 
        Trans.t IList.t -> Funct.t IList.t -> Funct.t -> Funct.t -> Cat.t -> 
          Trans.t 
    val close_type_ii :
        Funct.t -> Cat.t -> Cat.t IList.t -> Funct.t
    val close_type_tt :
        Funct.t -> Cat.t -> Cat.t IList.t -> Funct.t
    val fix_value : Trans.t -> Funct.t -> Funct.t IList.t -> Cat.t -> Trans.t
 end

module ToolIDule' 
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : SemFCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : SemFFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : SemFTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (XToolIDule : XToolIDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t)
    : (ToolIDule
    with module IdIndex = IdIndex
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct
    with module Trans = Trans) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat' = Cat
    module Funct' = Funct
    module Trans = Trans
    open IList
    open Cat
    open Funct
    open Trans
    module Cat = Cat'
    module Funct = Funct'

    let repair_ii = XToolIDule.repair unii t_con t_de

    let repair_tt = XToolIDule.repair untt t_uncon t_unde

    let close_type_ii = XToolIDule.close_type f_ii Cat.coi

    let close_type_tt = XToolIDule.close_type f_tt Cat.coi

    let fix_value t tb lta a = 
      (* t : cons (AtIndex.atr, tb) lta -> tb *)
      let ltapr = imap (t_pr lta) lta in
      let ta = f_pp a lta in
      let lba = Funct.cof tb ta in
      let prba = t_pr lba AtIndex.ate in
      let lpr = vmap (fun pr -> t_comp prba pr) ltapr in
      let lcopr = cons (AtIndex.atr, t_pr lba AtIndex.atd) lpr in
      let rba = t_record (f_pp a lba) lcopr in
      let fca = t_comp rba t in
      tl_fix fca (* : ta -> tb *)
  end

module ToolIDule = ToolIDule' (IdIndex) (AtIndex) (IList) (SemFCat) (SemFFunct)
    (SemFTrans) (XToolIDule)


module type OrdinaryIDule =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Sign : T
    module Dule : T

    val m_Ind_ordinary : Dule.t -> Dule.t
    val m_CoInd_ordinary : Dule.t -> Dule.t
  end

module OrdinaryIDule' 
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : ConPCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : ConPFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : ConPTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (Sign : Sign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (Dule : Dule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with module Sign = Sign)
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
    (ToolIDule : ToolIDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t)
    : (OrdinaryIDule
    with module IdIndex = IdIndex
    with module IList = IList
    with module Sign = Sign
    with module Dule = Dule) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Sign = Sign
    module Dule = Dule
    open IList
    open Cat
    open Funct
    open Trans

    let m_XInd_ordinary repair close_type fix_value  m = (* : S_Pp lr -> s' *)
      (* m : S_Pp lrr -> s, 
         lrr = cons (AtIndex.atr, s) lr,
         [AtIndex.atr] not in [lr] nor [lb] (local types of [s]),
         labels of [lr] and [lb] are disjoint,
         labels of context types of [r_i] are in [lr] 
         labels of context types of [s] are the labels of [lrr],
         labels of context types of [s'] are the labels of [lr],
         [s'] depends on its own local types 
         instead of on [AtIndex.atr]
       *)
      let s = Dule.codomain m in
      let h = Sign.s2f s in
      let f = Dule.type_part m in (* : c -> e *)
      let t = Dule.value_part m in (* : r -> f_COMP f h, r = S_Pp lrr *)
    (* analizing f:*)
      let c = SrcFCore.src f in (* = src (S_Pp lrr) *)
      let lc = unPP c in (* = cons (AtIndex.atr, b) la *)
      let e = SrcFCore.trg f in (* = src s *)
      let le = unPP e in (* = lb @@ lc *)
      let b = find AtIndex.atr lc in
      let lb = unPP b in (* normally, these have labels of [ls] *)
      let la = remove AtIndex.atr lc in (* [la] labels = [lr] labels *)
      let a = c_PP la in
    (* cutting f: *)
      let pib = imap (f_PR le) lb in
      let fc = f_COMP f (f_RECORD e pib) in (* : c -> b *)
    (* close_type f: *)
      let fi = close_type fc b la in (* : a -> b *)
    (* instantiating t: *)
      let lapr = imap (f_PR la) la in
      let lrfa = cons (AtIndex.atr, fi) lapr in
      let rf = f_RECORD a lrfa in (* : a -> c *)
      let rft = t_FT rf t in (* : f_COMP rf r -> f_COMP rf (f_COMP f h) *)
    (* repairing t: *)
      let lrta = vmap (fun f -> t_id f) lrfa in
      let lbpr = imap (f_PR lb) lb in
      let adc = repair lrta lbpr fi h a in
      let tad = t_comp rft adc in (* : g -> tb = f_COMP fif h *)
    (* analizing t: *)
      let g = DomFCore.dom tad in (* = f_COMP rf r *)
      let lg = unpp g in (* = cons (AtIndex.atr, tb) lta *)
      let tb = find AtIndex.atr lg in (* normally, the [ls] labels *)
      let lta = remove AtIndex.atr lg in (* [lta] labels = [lr] labels *)
    (* fix_value t:*)
      let t' = fix_value tad tb lta a in (* : ta -> tb *)
    (* changing s to s': *)
      let ld = remove AtIndex.atr le in
      let d = c_PP ld in
      let pid = imap (f_PR ld) ld in
      let pib = imap (fun i -> find i pid) lb in
      let reb = f_RECORD d pib in
      let pir = cons (AtIndex.atr, reb) pid in
      let red = f_RECORD d pir in (* d -> e *)
      let h' = f_COMP red h in
      let s' = Sign.f2s h' in
    (* extending f to s': *)
      let pifb = vmap (fun pr -> f_COMP fi pr) lbpr in
      let f' = f_RECORD a (pifb @@ lapr) in (* : a -> d *)
      Dule.pack (f', t', s')

(* split module here *)

    let m_Ind_ordinary = 
      m_XInd_ordinary 
	ToolIDule.repair_ii 
	ToolIDule.close_type_ii 
	ToolIDule.fix_value

    let m_CoInd_ordinary = 
      m_XInd_ordinary 
        ToolIDule.repair_tt 
        ToolIDule.close_type_tt 
        ToolIDule.fix_value
  end

module OrdinaryIDule = OrdinaryIDule' (IdIndex) (AtIndex) (IList) (ConPCat)
    (ConPFunct) (ConPTrans) (Sign) (Dule) (SrcFCore) (DomFCore) (ToolIDule)


module type SemIDule =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Sign : T
    module Dule : T

    val m_Ind : 
        Sign.t IList.t -> Dule.t IList.t ->
          [`OK of Dule.t|`Error of string]
    val m_CoInd : 
        Sign.t IList.t -> Dule.t IList.t ->
          [`OK of Dule.t|`Error of string]
  end

module SemIDule'
    (IdIndex : IdIndex)
    (AtIndex : AtIndex with module IdIndex = IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : T)
    (Funct : ConPFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : T)
    (Sign : Sign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (Dule : Dule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with module Sign = Sign)
    (SemWSign : SemWSign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with type Sign.t = Sign.t
    with type Dule.t = Dule.t)
    (SemWDule : SemWDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with type Sign.t = Sign.t
    with type Dule.t = Dule.t)
    (OrdinaryIDule : OrdinaryIDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Sign.t = Sign.t
    with type Dule.t = Dule.t)
    : (SemIDule
    with module IdIndex = IdIndex
    with module IList = IList
    with module Sign = Sign
    with module Dule = Dule) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Sign = Sign
    module Dule = Dule
    open IList

    let m_Ripcord lrr lm = (* : S_Pp lrr -> S_Pp ls' *)
      (* m_i : S_Pp lr_i -> s_i, 
         [lr_i} contained in [lr @@ ls],
         lrr = cons (AtIndex.atr, S_Pp ls) lr
         [s_i] has labels of context types inside [lr @@ ls],
         [AtIndex.atr] not in [ls], too,
         labels of context types of [r_i] are in [lr] 
         s'_i = S_Ww (re_i, s_i), 
         where domain of each [re_i] is [S_Pp lr],
         so the labels of context types of [S_Pp ls']
         are exactly the labels of [lrr]
       *)
      (match SemWSign.s_Pp lrr with
      |`OK rr ->
          (match imap1ok (SemWDule.m_Pr lrr) lrr with
          |`OK lprr ->
              let ls = vmap Dule.codomain lm in
              (match imap1ok (SemWDule.m_Pr ls) ls with
              |`OK lps ->
                  let prr = find AtIndex.atr lprr in
                  let lpc = vmap (fun pr -> 
		    SemWDule.m_Comp prr pr) lps in
                  let lpr = remove AtIndex.atr lprr in
                  let lprs = lpc @@ lpr in
                  let prm m =
                    let lf = Sign.value_part (Dule.domain m) in
                    let lmf = imap (fun i -> find i lprs) lf in
                    (match SemWDule.m_Record rr lmf with
                    |`OK re ->
                        SemWDule.m_Inst re m
                    |`Error er -> `Error er)
                  in
                  (match vmap1ok prm lm with
                  |`OK lm ->
                      SemWDule.m_Record rr lm
                  |`Error er -> `Error er)
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      |`Error er -> `Error er)

(* split module here *)

    let m_XInd m_Ind_ordinary lr lm = (* : S_Pp lr -> S_Pp ls *)
      (* m_i : S_Pp lr_i -> s_i, 
         [lr_i} contained in [lr @@ ls],
         [s_i] has labels of context types inside [lr @@ ls],
         [AtIndex.atr] not in [lr] nor [ls],
         labels of context types of [r_i] are in [lr] 
       *)
      let ls = vmap Dule.codomain lm in
      (match SemWSign.s_Pp ls with
      |`OK s ->
          let lrr = cons (AtIndex.atr, s) lr in
          (match m_Ripcord lrr lm with (* : S_ssr -> S_Pp ls' *)
          |`OK m -> 
              let mind = 
		m_Ind_ordinary m in (* : S_Pp lr -> S_Pp ls'' *)
              SemWDule.m_Trim mind s
          |`Error er -> `Error er)
      |`Error er -> `Error er)

(* split module here *)
        
    let m_Ind = m_XInd OrdinaryIDule.m_Ind_ordinary

    let m_CoInd = m_XInd OrdinaryIDule.m_CoInd_ordinary
  end

module SemIDule = SemIDule' (IdIndex) (AtIndex) (IList) (ACat) (ConPFunct)
    (ATrans) (Sign) (Dule) (SemWSign) (SemWDule) (OrdinaryIDule)


module type IDule = (* module system with inductive modules *)
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module VarStamp : Stamp
    module Cat : T
    module Funct : T
    module Trans : T
    module Sign : T
    module Dule : T
    module Location : Location
    module BCore : BCore 
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Location = Location

    type sign = Location.t * sign'
    and sign' =
      | S_Pp of sign IList.t
      | S_Bb of sign * Cat.t IList.t * Funct.t IList.t
      | SC_Bb of sign * BCore.kind IList.t * BCore.typ IList.t
      | S_Ww of dule * sign
      | S_Mm of string * sign
      | S_Var of VarStamp.t
    and dule = Location.t * dule'
    and dule' =
      | M_Id of sign
      | M_Comp of dule * dule
      | M_Pr of sign IList.t * IdIndex.t
      | M_Record of sign * dule IList.t
      | M_Base of sign * sign * Funct.t IList.t * Trans.t IList.t
      | MC_Base of sign * sign * BCore.typ IList.t * BCore.valu IList.t
      | M_Inst of dule * dule
      | M_Trim of dule * sign
      | M_Slash of dule * sign * sign
      | M_Accord of sign IList.t * dule IList.t
      | M_Concord of sign IList.t * dule IList.t
      | M_Link of sign IList.t * dule IList.t
      | M_Ind of sign IList.t * dule IList.t
      | M_CoInd of sign IList.t * dule IList.t
      | M_Memo of string * dule
      | M_Finish of Dule.t

    val term_sign : sign -> sign'
    val loc_sign : sign -> Location.t
    val term_dule : dule -> dule'
    val loc_dule : dule -> Location.t
  end

module IDule'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (VarStamp : Stamp)
    (Cat : T)
    (Funct : T)
    (Trans : T)
    (Sign : Sign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (Dule : Dule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with module Sign = Sign)
    (Location : Location)
    (BCore : BCore 
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Location = Location)
    : (IDule 
    with module IdIndex = IdIndex
    with module IList = IList
    with module VarStamp = VarStamp
    with module Cat = Cat
    with module Funct = Funct
    with module Trans = Trans
    with module Location = Location
    with module BCore = BCore
    with module Sign = Sign
    with module Dule = Dule) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module VarStamp = VarStamp
    module Cat = Cat
    module Funct = Funct
    module Trans = Trans
    module Sign = Sign
    module Dule = Dule
    module BCore = BCore
    module Location = Location

    type sign = Location.t * sign'
    and sign' =
      | S_Pp of sign IList.t
      | S_Bb of sign * Cat.t IList.t * Funct.t IList.t
      | SC_Bb of sign * BCore.kind IList.t * BCore.typ IList.t
      | S_Ww of dule * sign
      | S_Mm of string * sign
      | S_Var of VarStamp.t
    and dule = Location.t * dule'
    and dule' =
      | M_Id of sign
      | M_Comp of dule * dule
      | M_Pr of sign IList.t * IdIndex.t
      | M_Record of sign * dule IList.t
      | M_Base of sign * sign * Funct.t IList.t * Trans.t IList.t
      | MC_Base of sign * sign * BCore.typ IList.t * BCore.valu IList.t
      | M_Inst of dule * dule
      | M_Trim of dule * sign
      | M_Slash of dule * sign * sign
      | M_Accord of sign IList.t * dule IList.t
      | M_Concord of sign IList.t * dule IList.t
      | M_Link of sign IList.t * dule IList.t
      | M_Ind of sign IList.t * dule IList.t
      | M_CoInd of sign IList.t * dule IList.t
      | M_Memo of string * dule
      | M_Finish of Dule.t

    let term_sign (_, s) = s
    let loc_sign (l, _) = l
    let term_dule (_, m) = m
    let loc_dule (l, _) = l
  end

module IDule = IDule' (IdIndex) (IList) (Stamp) (ACat) (AFunct) (ATrans) 
    (Sign) (Dule) (Location) (BCore)


module type PpIDule =
  sig
    module IdIndex : IdIndex
    module IList : IList 
    with type Index.t = IdIndex.t
    module IDule : IDule
    with module IdIndex = IdIndex 
    with module IList = IList

    val pp_s : IDule.sign -> string
    val pp_ls : IDule.sign IList.t -> string
    val pp_m : IDule.dule -> string
    val pp_lm : IDule.dule IList.t -> string
  end

module PpIDule'
    (IdIndex : IdIndex)
    (IList : IList 
    with type Index.t = IdIndex.t)
    (IDule : IDule
    with module IdIndex = IdIndex 
    with module IList = IList) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module IDule = IDule
    open IList
    open IDule

    let pp_lfi lf =
      let s = bfold ""
          (fun (i, f) s -> 
            " " ^ IdIndex.t2s i ^ ";" ^ s) lf
      in
      (if s = "" then s else
      String.sub s 1 (String.length s - 2))

    let rec pp_s s =
      match term_sign s with
      | S_Pp ls -> "{" ^ pp_ls ls ^ "}"
      | S_Bb (r, lc, lf) -> "S_Bb(" ^ pp_s r ^ ", " ^ pp_lfi lf ^ ")"
      | SC_Bb (r, lc, lf) -> "SC_Bb(" ^ pp_s r ^ ", " ^ pp_lfi lf ^ ")"
      | S_Ww (m1, s2) -> "S_Ww(" ^ pp_m m1 ^ ", " ^ pp_s s2 ^ ")"
      | S_Mm (n, r) -> "S_Mm(" ^ n ^ ")"
      | S_Var var_num -> "S_Var(" ^ VarStamp.t2string var_num ^ ")"
    and pp_ls ls =
      let s = bfold ""
          (fun (i, f) s -> 
            " " ^ IdIndex.t2s i ^ " : " ^ pp_s f ^ ";" ^ s) ls
      in
      IListBasic.pp_stamp ls ^ " " ^ (if s = "" then s else
      String.sub s 1 (String.length s - 2))
    and pp_m m =
      match term_dule m with
      | IDule.M_Id s -> 
          "M_Id(" ^ pp_s s ^ ")"
      | IDule.M_Comp (m1, m2) -> 
          "M_Comp(" ^ pp_m m1 ^ ", " ^ pp_m m2 ^ ")" 
      | IDule.M_Pr (lr, i) -> 
          "M_Pr(" ^ pp_ls lr ^ ", " ^ IdIndex.t2string i ^ ")"
      | IDule.M_Record (r, lm) -> 
          "M_Record(" ^ pp_s r ^ ", " ^ pp_lm lm ^ ")"
      | IDule.M_Base (r, s, lg, lt) -> 
          "M_Base(" ^ pp_s r ^ ", " ^ pp_s s ^ ")"
      | IDule.MC_Base (r, s, lg, lt) ->
          "MC_Base(" ^ pp_s r ^ ", " ^ pp_s s ^ ")"
      | IDule.M_Inst (m1, m2) -> 
          "M_Inst(" ^ pp_m m1 ^ ", " ^ pp_m m2 ^ ")" 
      | IDule.M_Trim (m1, r2) -> 
          "M_Trim(" ^ pp_m m1 ^ ", " ^ pp_s r2 ^ ")" 
      | IDule.M_Slash (m1, r2, s2) -> 
          "M_Slash(" ^ pp_m m1 ^ ", " ^ pp_s r2 ^ ", " ^ pp_s s2 ^ ")" 
      | IDule.M_Accord (lr, lm) ->
          "M_Accord(" ^ pp_ls lr ^ ", " ^ pp_lm lm ^ ")"
      | IDule.M_Concord (lr, lm) ->
          "M_Concord(" ^ pp_ls lr ^ ", " ^ pp_lm lm ^ ")"
      | IDule.M_Link (lr, lm) -> 
          "M_Link(" ^ pp_ls lr ^ ", " ^ pp_lm lm ^ ")"
      | IDule.M_Ind (lr, lm) ->
          "M_Ind(" ^ pp_ls lr ^ ", " ^ pp_lm lm ^ ")"
      | IDule.M_CoInd (lr, lm) ->
          "M_CoInd(" ^ pp_ls lr ^ ", " ^ pp_lm lm ^ ")"
      | IDule.M_Memo (n, m) -> 
          "M_Memo(" ^ n ^ ")"
      | IDule.M_Finish t -> 
          "M_Finish()"

    and pp_lm lm =
      let s = bfold ""
          (fun (i, f) s -> 
            " " ^ IdIndex.t2s i ^ " = " ^ pp_m f ^ ";" ^ s) lm
      in
      (if s = "" then s else
      String.sub s 1 (String.length s - 2))
end

module PpIDule = PpIDule' (IdIndex) (IList) (IDule)


module type ElabIDule =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Location : Location
    module ErrorRepLib : ErrorRepLib
    with module Location = Location
    module Cat : T
    module Funct : T
    module Trans : T
    module Sign : T
    module Dule : T
    module IDule : IDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Cat = Cat
    with type Funct.t = Funct.t
    with module Trans = Trans
    with module Sign = Sign
    with module Dule = Dule
    with module Location = Location
    val complex_structures : bool ref

    val el_sign : IDule.sign -> 
      [`OK of Sign.t|`Error of ErrorRepLib.error]
    val el_dule : IDule.dule -> 
      [`OK of Dule.t
      |`Error of ErrorRepLib.error]
    val el_prelude : IDule.dule -> 
      [`OK of Dule.t
      |`Error of ErrorRepLib.error]
  end

module ElabIDule' 
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Location : Location)
    (ErrorRepLib : ErrorRepLib
    with module Location = Location)
    (Cat : T)
    (Funct : T)
    (Trans : T)
    (Sign : Sign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (Dule : Dule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with module Sign = Sign)
    (SemWSign : SemWSign
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with type Sign.t = Sign.t
    with type Dule.t = Dule.t)
    (SemWDule : SemWDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with type Sign.t = Sign.t
    with type Dule.t = Dule.t)
    (SemWDuleComplex : SemWDuleComplex
    with module IdIndex = IdIndex 
    with module IList = IList
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with type Sign.t = Sign.t
    with type Dule.t = Dule.t)
    (SemLDule : SemLDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with type Sign.t = Sign.t
    with type Dule.t = Dule.t)
    (SemIDule : SemIDule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Sign.t = Sign.t
    with type Dule.t = Dule.t)
    (BCore : BCore 
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location)
    (ElabBCore : ElabBCore
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with module BCore = BCore)
    (IDule : IDule 
    with module IdIndex = IdIndex
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with module Sign = Sign
    with module Dule = Dule
    with module Location = Location
    with module BCore = BCore)
    (CacheFFunct : Cache
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with type Value.t = Sign.t)
    (CacheFTrans : Cache
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with type Value.t = Dule.t)
    : (ElabIDule
    with module IdIndex = IdIndex
    with module IList = IList
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with module Sign = Sign
    with module Dule = Dule
    with module IDule = IDule) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Cat = Cat
    module Funct = Funct
    module Trans = Trans
    module Sign = Sign
    module Dule = Dule
    module IDule = IDule
    module Location = Location
    module ErrorRepLib = ErrorRepLib
    open IList
    open SemWSign
    open SemWDule
    open SemLDule
    open SemIDule
    open ErrorRepLib

    let complex_structures = ref false
    let mem_f = CacheFFunct.create () (* FIXME: memory leak! *)
    let mem_t = CacheFTrans.create ()

    let loc_error l ok_error =
      match ok_error with
      |`OK m ->
          `OK m
      |`Error s -> 
          `Error 
            (modBackError#instance 
               [Loc l; 
                Msg s])

    let rec el_sign s =
      let l = IDule.loc_sign s in
      match IDule.term_sign s with
      | IDule.S_Pp ls ->
          (match vmap1ok el_sign ls with
          |`OK ls -> 
              loc_error l (s_Pp ls)
          |`Error er -> `Error er)
      | IDule.S_Bb (r, lc, lf) -> 
          (match el_sign r with
          |`OK r ->
              loc_error l (s_Bb r lc lf)
          |`Error er -> `Error er)
      | IDule.SC_Bb (r, lc, lf) ->
          (match el_sign r with
          |`OK r -> 
              let lc = vmap ElabBCore.elab_kind lc in
              let l_c2f = vmap (fun f -> fun c -> ElabBCore.elab_typ c f) lf in
              sc_Bb r lc l_c2f l
          |`Error er -> `Error er)
      | IDule.S_Ww (m1, s2) ->
          (match el_dule m1 with
          |`OK m1 ->
              (match el_sign s2 with
              |`OK s2 ->
                  loc_error l (s_Ww m1 s2)
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      | IDule.S_Mm (n, s) -> 
	  if n = "" then el_sign s
	  else CacheFFunct.el_thunk mem_f (fun () -> el_sign s) n
      | IDule.S_Var _ -> loc_error l (s_Pp nil)

    and el_dule m =
      let l = IDule.loc_dule m in
      match IDule.term_dule m with
      | IDule.M_Id s ->
          (match el_sign s with
          |`OK s -> 
              `OK (m_Id s)
          |`Error er -> `Error er)
      | IDule.M_Comp (m1, m2) ->
          (match el_dule m1 with
          |`OK m1 ->
              (match el_dule m2 with
              |`OK m2 ->
                  `OK (m_Comp m1 m2)
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      | IDule.M_Pr (lr, i) ->
          (match vmap1ok el_sign lr with
          |`OK lr -> 
              loc_error l (m_Pr lr i)
          |`Error er -> `Error er)
      | IDule.M_Record (r, lm) ->
          (match el_sign r with
          |`OK r ->
              (match vmap1ok el_dule lm with
              |`OK lm -> 
                  loc_error l (m_Record r lm)
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      | IDule.M_Base (r, s, lg, lt) ->
          (match el_sign r with
          |`OK r ->
              (match el_sign s with
              |`OK s ->
		  let m_Base = 
		    if !complex_structures then SemWDuleComplex.m_Base
		    else m_Base
		  in
                  loc_error l (m_Base r s lg lt)
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      | IDule.MC_Base (r, s, lg, lt) ->
          (match el_sign r with
          |`OK r ->
              (match el_sign s with
              |`OK s ->
                  let l_c2g = vmap (fun g -> fun c -> 
                    ElabBCore.elab_typ c g) lg in
                  let l_fh2t = vmap (fun t -> fun f -> fun h ->
                    ElabBCore.elab_valu f h t) lt in
 		  let mc_Base = 
		    if !complex_structures then SemWDuleComplex.mc_Base
		    else mc_Base
		  in
                  mc_Base r s l_c2g l_fh2t l
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      | IDule.M_Inst (m1, m2) ->
          (match el_dule m1 with
          |`OK m1 ->
              (match el_dule m2 with
              |`OK m2 ->
                  loc_error l (m_Inst m1 m2)
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      | IDule.M_Trim (m1, r2) ->
          (match el_dule m1 with
          |`OK m1 ->
              (match el_sign r2 with
              |`OK r2 ->
                  loc_error l (m_Trim m1 r2)
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      | IDule.M_Slash (m1, r2, s2) ->
          (match el_dule m1 with
          |`OK m1 ->
              (match el_sign r2 with
              |`OK r2 ->
                  (match el_sign s2 with
                  |`OK s2 ->
                      loc_error l (m_Slash m1 r2 s2)
                  |`Error er -> `Error er)
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      | IDule.M_Accord (lr, lm) ->
          (match vmap1ok el_sign lr with
          |`OK lr -> 
              (match vmap1ok el_dule lm with
              |`OK lm ->                      
                  loc_error l (m_Accord lr lm)
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      | IDule.M_Concord (lr, lm) ->
          (match vmap1ok el_sign lr with
          |`OK lr -> 
              (match vmap1ok el_dule lm with
              |`OK lm ->                      
                  loc_error l (m_Concord lr lm)
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      | IDule.M_Link (lr, lm) -> 
        (* assumption : there is no cyclic dependency in lm (with m_Link) or
           every module depends only on previous ones (with m_Link_ordered) *)
          (match vmap1ok el_sign lr with
          |`OK lr ->
              (match bmap1ok (fun (i, m) ->
		if !Tools.debugging then 
		  (prerr_endline (" S:" ^ IdIndex.t2s i ^ " ");
		   flush stderr) else ();
                el_dule m) lm with
              |`OK lm ->
                  let lm = bfold nil (fun iv r -> 
                    cons iv r) lm in (* this is just rev! *)
                  loc_error l (m_Link_ordered lr lm) (* or m_Link *)
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      | IDule.M_Ind (lr, lm) -> 
        (* here circularity in lm permitted! *)
          (match vmap1ok el_sign lr with
          |`OK lr -> 
              (match vmap1ok el_dule lm with
              |`OK lm -> 
                  loc_error l (m_Ind lr lm)
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      | IDule.M_CoInd (lr, lm) -> 
        (* here circularity in lm permitted! *)
          (match vmap1ok el_sign lr with
          |`OK lr -> 
              (match vmap1ok el_dule lm with
              |`OK lm -> 
                  loc_error l (m_CoInd lr lm)
              |`Error er -> `Error er)
          |`Error er -> `Error er)
      | IDule.M_Memo (n, m) ->
          CacheFTrans.el_thunk mem_t (fun () -> el_dule m) n
      | IDule.M_Finish t -> `OK t

    let el_dule m =
      let m = el_dule m in
      let _ = !reset_memos () in
      m

    let el_prelude = el_dule
  end

module CacheFFunct = Cache' (Location) (ErrorRepLib)
    (struct type t = Sign.t end)

module CacheFTrans = Cache' (Location) (ErrorRepLib)
    (struct type t = Dule.t end)

module ElabIDule = ElabIDule' (IdIndex) (IList) (Location) (ErrorRepLib)
    (ACat) (AFunct) (ATrans) (Sign) (Dule)
    (SemWSign) (SemWDule) (SemWDuleComplex) (SemLDule) (SemIDule)
    (BCore) (ElabBCore) (IDule) (CacheFFunct) (CacheFTrans)


(* Ordinary product operations (with no name-driven sharing). *)
(* Just for the theory. *)
module type SemPDule =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T
    module Trans : T
    module Sign : T
    module Dule : T

    (* ordinary product *)
    val s_Pp_ordinary : Sign.t IList.t -> Sign.t
    val m_Pr_ordinary : Sign.t IList.t -> IdIndex.t -> Dule.t
    val m_Record_ordinary : Sign.t -> Dule.t IList.t -> Dule.t

    (* general equalizer (* *)
    val s_Qq : Dule.t IList.t ->
      [`OK of Sign.t|`Error of string]
    val m_Equate : Dule.t IList.t ->
      [`OK of Dule.t|`Error of string]
    val m_Verify : Dule.t IList.t -> Dule.t -> 
      [`OK of Dule.t|`Error of string]
     *)
  end

module SemPDule'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    (Cat : ConPCat with module IdIndex = IdIndex and module IList = IList)
    (Funct : ConPFunct
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t)
    (Trans : ConPTrans
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (Sign : Sign
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t)
    (Dule : Dule
    with module IdIndex = IdIndex 
    with module IList = IList
    with type Cat.t = Cat.t
    with type Funct.t = Funct.t
    with type Trans.t = Trans.t
    with module Sign = Sign)
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
    (EqFFunct : EqFFunct with type Funct.t = Funct.t)
    (PpFFunct : PpFFunct 
    with type Funct.t = Funct.t
    with module Funct.IdIndex = IdIndex
    with module Funct.IList = IList)
    : (SemPDule
    with module IdIndex = IdIndex
    with module IList = IList
    with module Cat = Cat
    with module Funct = Funct
    with module Trans = Trans
    with module Sign = Sign
    with module Dule = Dule) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    module Location = Location
    module ErrorRepLib = ErrorRepLib
    module Cat = Cat
    module Funct = Funct
    module Trans = Trans
    module Sign = Sign
    module Dule = Dule
    open IList
    open Cat
    open Funct
    open Trans
    open ErrorRepLib

    let typesPp ls = 
      let capture_type_part s =
        let la = Sign.type_part s in
        c_PP la
      in
      vmap capture_type_part ls

    let footPp lc i s =
      f_PR lc i

    let s_Pp_ordinary ls = 
      let lc = typesPp ls in
      let legPp (i, s) = 
        let foot = footPp lc i s in
        f_COMP foot (Sign.s2f s)
      in
      let legs = bmap legPp ls in
      let c = c_PP lc in
      let body = f_pp c legs in
      Sign.f2s body

    let m_Pr_ordinary lr i = (* : S_Pp_ordinary lr -> s *)
      let s = find i lr in
      let r = s_Pp_ordinary lr in
      let lc = Sign.type_part r in
      let foot_i = footPp lc i s in
      let legs = Sign.value_part r in
      let t = t_pr legs i in
      Dule.pack (foot_i, t, s)

    let m_Record_ordinary r lm = (* : r -> S_Pp_ordinary ls *)
      let lf = vmap Dule.type_part lm (* : r -> s_i *) in
      let lt = vmap Dule.value_part lm in
      let ls = vmap Dule.codomain lm in
      let g = Sign.s2f r in
      let c = SrcFCore.src g in
      let f = f_RECORD c lf in 
      let t = t_record g lt in
      let s = s_Pp_ordinary ls in
      Dule.pack (f, t, s)
  end

module SemPDule = SemPDule' (IdIndex) (IList)
    (ConPCat) (ConPFunct) (ConPTrans) (Sign) (Dule)
    (SrcFCore) (DomFCore) (EqFFunct) (PpFFunct)
