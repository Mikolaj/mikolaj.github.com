(* Copyright (C) 2003 Mikolaj Konarski
 *
 * This file is part of the Dule compiler.
 * The Dule compiler is released under the GNU General Public License (GPL).
 * Please see the file Dule-LICENSE for license information.
 *
 * $Id: middle_middle.ml,v 1.18 2006-03-29 15:47:08 mikon Exp $
 *) 

open Core_back open Tools open Error_rep

module type ConPCat = (* categories of p-Core *)
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t

    type t

    val c_PP : t IList.t -> t
    val c_BB : t

    val unPP : t -> t IList.t
    val unPPok : t -> [`OK of t IList.t|`Error of string]
    val isBB : t -> bool
    val isPP : t -> bool
  end

module ConPCat = SemFCat


module type ConPFunct = (* functors of p-Core --- only products *)
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T

    type t

    val f_ID : Cat.t -> t
    val f_COMP : t -> t -> t
    val f_PR : Cat.t IList.t -> IdIndex.t -> t
    val f_RECORD : Cat.t -> t IList.t -> t
    val f_pp : Cat.t -> t IList.t -> t

    val unpp : t -> t IList.t
    val unpp_ok : t -> [`OK of t IList.t|`Error of string]
  end

module ConPFunct = SemFFunct


module type ConPTrans = (* transformations of p-Core --- only product ops *)
  sig	
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    module Cat : T
    module Funct : T

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
    val t_TF_coco : t * t ->  Funct.t -> t
  end

module ConPTrans = ConFTrans


module type Cache =
  sig
    module Location : Location
    module ErrorRepLib : ErrorRepLib
    with module Location = Location
    module Value : sig type t end

    type mem

    val create : unit -> mem
    val clear : mem -> unit
    val find_mem_ok : mem -> string -> [`OK of Value.t|`Error of unit]
    val add_mem : mem -> string -> Value.t -> unit
    val el_thunk : mem -> 
	(unit -> [`OK of Value.t
                 |`Error of ErrorRepLib.error]) ->
	  string -> [`OK of Value.t
                    |`Error of ErrorRepLib.error]
  end

module Cache'
    (Location : Location)
    (ErrorRepLib : ErrorRepLib
    with module Location = Location)
    (Value : sig type t end)
    : (Cache
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib
    with module Value = Value) =
  struct
    module Location = Location
    module ErrorRepLib = ErrorRepLib
    module Value = Value

    type mem = (string, Value.t) Hashtbl.t

    let create () = Hashtbl.create good_hash_size

    let clear mem = Hashtbl.clear mem

    let find_mem_ok mem n = 
      try `OK (Hashtbl.find mem n)
      with Not_found -> `Error ()

    let add_mem mem n v = 
      assert 
	(try Hashtbl.find mem n; false
	with Not_found -> true);
      Hashtbl.add mem n v

    let el_thunk mem thunk n =
      match find_mem_ok mem n with
      |`OK f -> `OK f
      |`Error er ->
	  (match thunk () with
	  |`OK f ->
	      let _ = add_mem mem n f in
	      `OK f
	  |`Error er -> `Error er)
  end

