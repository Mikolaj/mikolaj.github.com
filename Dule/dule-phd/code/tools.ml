(* Copyright (C) 2003--2006 Mikolaj Konarski
 * Copyright (C) 2003 Andrzej Gasienica-Samek
 * Copyright (C) 2005 Piotr Roszkowski
 * Copyright (C) 2006 Michal Wesolowski
 *
 * This file is part of the Dule compiler.
 * The Dule compiler is released under the GNU General Public License (GPL).
 * Please see the file Dule-LICENSE for license information.
 *
 * $Id: tools.ml,v 1.92 2007-08-25 13:21:29 mikon Exp $
 *) 

let debugging = ref false

let pri s = (* only short constant strings, please! *)
  if !debugging then prerr_string s else ()

let good_hash_size = 3001

let combine m n = m * 65599 + n


module type T =
  sig	
    type t
  end


module type Stamp =
  sig
    type t

    val first : t
    val eq : t -> t -> bool
    val less : t -> t -> bool
    val inc : t -> t
    val incr : t ref -> unit
    val dummy : t
    val t2int : t -> int
    val t2string : t -> string
  end

module Stamp : Stamp =
  struct
    type t = int

    let first = 5
    let eq n m = (n = m)
    let less n m = (n < m)
    let inc n = n + 1
    let incr = incr
    let dummy = -1
    let t2int n = n
    let t2string n = string_of_int n
  end


module type MemoizeType =
  sig
    type t
    type result
    val equal : t -> t -> bool
    val hash : t -> int
  end

module type Memoize =
  sig
    module MemoizeType : MemoizeType
    type t
    val create : int -> t
    val clear : t -> unit
    val find : t -> MemoizeType.t -> MemoizeType.result
    val add : t -> MemoizeType.t -> MemoizeType.result -> unit
    val memoize : t ->
      (MemoizeType.t -> MemoizeType.result) -> 
	MemoizeType.t -> MemoizeType.result
    val memorec : t ->
      ((MemoizeType.t -> MemoizeType.result) -> 
	MemoizeType.t -> MemoizeType.result) -> 
	  MemoizeType.t -> MemoizeType.result
  end

(* Memoize.clear functions will be appended to this one *)
let reset_memos = ref (fun () -> ())

module Memoize'
    (MemoizeType : MemoizeType) 
    : (Memoize
    with module MemoizeType = MemoizeType) =
  struct
    module MemoizeType = MemoizeType
	
    module Hash = Hashtbl.Make (MemoizeType)

    type t = MemoizeType.result Hash.t
	
    let create size =
      let tbl = Hash.create size in
      let resets = !reset_memos in 
      let _ = reset_memos := fun () -> 
	begin
	  begin
	    if Hash.length tbl = 0 then () 
	    else Hash.clear tbl
	  end;
	  resets ()
	end;
      in 
      tbl
	
    let clear = Hash.clear

    let find = Hash.find

    let add = Hash.add
 
    let memoize tbl f a =
      try 
	find tbl a
      with Not_found ->
	let b = f a in
	let _ = add tbl a b in
	b

    let memorec tbl f =
      let rec f' a = 
	try 
	  find tbl a
	with Not_found ->
	  let b = f f' a in
	  let _ = add tbl a b in
	  b
      in
      f'
  end


module type StrictOrder =
  sig
    type t
    val less : t -> t -> bool
  end

module type AVL =
  sig
    module Key : StrictOrder
    module Valu : T
    type t
    val empty :  t
    val add : t -> Key.t -> Valu.t -> t
    val find : t -> Key.t -> Valu.t
  end

module AVL'
    (Key : StrictOrder)
    (Valu : T)
    : (AVL
    with module Key = Key
    with module Valu = Valu) =
  struct
    module Key = Key
    module Valu = Valu

    type bal = MINUS| ZERO| PLUS

    type t =
	Null
      |
	Node of t * Key.t * Valu.t * t * bal

    let empty = Null

    let rec height tree = 
      match tree with
	Null -> 0
      |
	Node (l,k,v,r,b) ->
	  (match b with
	  | MINUS -> 1 + height r
	  | ZERO -> 1 + height r
	  | PLUS -> 1 + height l)
	    
    let get_left t =
      match t with
	Null -> failwith "get_left: empty"
      |
	Node (l,_,_,_,_)-> l

    let get_right t =
      match t with
	Null -> failwith "get_right: empty"
      |
	Node (_,_,_,r,_) -> r

    let get_key t =
      match t with
	Null -> failwith "get_key: empty"
      |
	Node (_,k,_,_,_) -> k
	    
    let get_valu t =
      match t with
	Null -> failwith "get_valu: empty"
      |
	Node (_,_,v,_,_) -> v
	    
    let get_bal t =
      match t with
	Null -> failwith "get_bal: empty"
      |
	Node (_,_,_,_,b) -> b
	    
    let shiftRightRight tree =
      match tree with
	Null -> failwith "shiftRightRight: tree is empty"
      |
	Node (l,k,v,r,_) ->
	  match r with 
	    Null -> failwith "shiftRightRight: right is empty"
	  |
	    Node (rleft,rkey,rvalu,rright,rbal) ->
	      Node (Node (l,k,v,rleft,
			  if rbal = MINUS then ZERO else MINUS),
		    rkey,rvalu,rright,
		    if rbal = ZERO then PLUS else ZERO)

    let shiftLeftLeft tree =
      match tree with
	Null -> failwith "shiftLeftLeft: tree is empty"
      |
	Node (l,k,v,r,_) ->
	  match l with 
	    Null -> failwith "LeftLeft: left is empty"
	  |
	    Node (lleft,lkey,lvalu,lright,lbal) ->
	      Node (lleft,lkey,lvalu,Node (lright,k,v,r,
					   if lbal = PLUS then ZERO else PLUS),
		    if lbal = ZERO then MINUS else ZERO)

    let shiftLeftRight tree =
      match tree with
	Null -> failwith "shiftLeftRight: tree is empty"
      |
	Node (left,key,valu,right,_) ->
	  match left with
	    Null -> failwith "shiftLeftRight: left is empty"
	  |
	    Node (lleft,lkey,lvalu,lright,_) ->
	      match lright with
		Null -> failwith "shiftLeftRight: lright is empty"
	      |
		Node (lRleft,lRkey,lRvalu,lRright,lRbal) ->
		  Node (Node (lleft,lkey,lvalu,lRleft,
			      if lRbal = MINUS then PLUS else ZERO),
			lRkey,lRvalu,
			Node (lRright,key,valu,right,
			      if lRbal = PLUS then MINUS else ZERO),ZERO)

    let shiftRightLeft tree =
      match tree with
	Null -> failwith "shiftRightLeft: tree is empty"
      |
	Node (left,key,valu,right,_) ->   
	  match right with
	    Null -> failwith "shiftRightLeft: right is empty"
	  |
	    Node (rleft,rkey,rvalu,rright,_) ->
	      match rleft with
		Null -> failwith "shiftRightLeft: rleft is empty"
	      |
		Node (rLleft,rLkey,rLvalu,rLright,rLbal) ->
		  Node (Node (left,key,valu,rLleft,
			      if rLbal = MINUS then PLUS else ZERO),
			rLkey,rLvalu,
			Node (rLright,rkey,rvalu,rright,
			      if rLbal = PLUS then MINUS else ZERO),ZERO)

   let rec insert tree ikey ivalu =
      let (new_tree, same_height) =
	match tree with 
	  Null -> (* height changed (from 0 to 1) *)
	    (Node (Null,ikey,ivalu,Null,ZERO), false)
	|
	  Node (left,key,valu,right,bal) ->
	    if Key.less ikey key then (* left gets it *)
	      insLRside ikey ivalu true  (left,key,valu,right,bal)
	    else if Key.less key ikey then (* right gets it *)
	      insLRside ikey ivalu false (left,key,valu,right,bal)
	    else (* already there *)
	      (tree, true)
      in
     assert (same_height = (height tree = height new_tree));
      (new_tree, same_height)
	
    and insLRside ikey ivalu sideLeft (left,key,valu,right,bal) = 
      let (b, same_height, newLeft, newRight) =
	if sideLeft then 
	  let toChange = left in
	  let (t, same_height) = insert toChange ikey ivalu in
	  (get_bal t, same_height, t, right)
	else
	  let toChange = right in
	  let (t, same_height) = insert toChange ikey ivalu in
	  (get_bal t, same_height, left, t)
      in
      if
	same_height
      then (* change is local, there was still some free space *)
	(Node (newLeft,key,valu,newRight,bal), true)
      else (* balancing needed *)
	let mirror_balance =
	  let reverse bal =
	    match bal with
	    | MINUS -> PLUS
	    | ZERO -> ZERO
	    | PLUS -> MINUS
	  in
	  if sideLeft then bal
	  else reverse bal
	in
	match mirror_balance with 
	  PLUS -> (* inserting on the higher side --- problems! *)
	    let shift = 
	      if 
		bal = b
	      then (* balancing on the higher side suffices *)
		if sideLeft then shiftLeftLeft else shiftRightRight
	      else (* first balance the higher side, then whole tree *)
		if sideLeft then shiftLeftRight else shiftRightLeft
	    in
	    (shift (Node (newLeft,key,valu,newRight,bal)),
	     true)
	| 
	  ZERO -> (* tree leans on the side where we insert, height changed *)
	    (Node (newLeft,key,valu,newRight,
		   if sideLeft then PLUS else MINUS),
	     false)
	| 
	  MINUS -> (* inserted on the lower side *)
	    (Node (newLeft,key,valu,newRight,ZERO),
	     true)
	      
    let add tree key valu =
      let (result, _) = insert tree key valu in
      assert
	(let rec testAvl t =
	  match t with
	    Null -> true
	  |
	    Node (l,k,v,r,b)->
	      let diff = height l - height r in
	      let bal = 
		if diff < 0 then MINUS
		else if diff > 0 then PLUS
		else ZERO
	      in
              testAvl l && testAvl r && abs diff <= 1 && bal = b
	in testAvl result);
      result
		    
    let rec find tree ikey = 
      match tree with
	Null -> raise Not_found
      |
	Node (left,key,valu,right,_) ->
	  if Key.less ikey key then
	    find left ikey
	  else if Key.less key ikey then 
	    find right ikey
	  else 
	    valu
  end


module type UpdateIndex =
  sig
    module IndexType : T

    type t 

    val de_t : t -> IndexType.t
    val stamp_t : t -> int 
    val find_update : IndexType.t -> t
    val eq : t -> t -> bool
  end

module UpdateIndexAVL' (* Stamp and AVL' implicitly *)
    (IndexType : T)
    : (UpdateIndex 
    with module IndexType = IndexType) =
  struct
    module IndexType = IndexType

    type t = Stamp.t
 
    module I =
      struct 
	type t = IndexType.t
	let less i j = (compare i j < 0)
      end
    module T = Stamp
    module AVLIndex = AVL' (T) (I) (* IndexType.t are values *)
    module AVLT = AVL' (I) (T) (* t are values, IndexType.t are keys *)

    let num = ref Stamp.first
    let index_tree = ref AVLIndex.empty
    let t_tree = ref AVLT.empty
	
    let find_update index =
      try AVLT.find !t_tree index
      with Not_found -> 
	let _ = Stamp.incr num in
	let index_tree' = AVLIndex.add !index_tree !num index in
	let t_tree' = AVLT.add !t_tree index !num in
        let _ = index_tree := index_tree' in
	let _ = t_tree := t_tree' in
        !num
      
    let de_t t = AVLIndex.find !index_tree t
(*  let de_t = AVLIndex.find !index_tree --- not equivalent to the above!!! *)
    let stamp_t = Stamp.t2int 
    let eq = Stamp.eq
  end


module type Index =
  sig	
    type t
    val t2string : t -> string (* only for error messages *)
    val eq : t -> t -> bool
  end

module type IdIndex =
  sig	
    type t
    (* constructors: *)
    val s2type : string -> t   (* type identifier *)
    val s2value : string -> t  (* value identifier *)
    val s2case : string -> t   (* case identifier *)
    val s2dule : string -> t   (* module identifier *)
    val s2sp : string -> t     (* specification identifier *)
    val loc2wild : string -> t (* wild card identifier *)
    val loc2patt : string -> t (* pattern identifier *)
    (* conversions: *)
    val t2string : t -> string (* longer *)
    val t2s : t -> string
    (* tests: *)
    val is_dule : t -> bool
    val is_sp : t -> bool
    val is_patt : t -> bool
    val eq : t -> t -> bool
    (* for hashing: *)
    val stamp_t : t -> int
  end


module type IndexType =
  sig
    type t =
      |	TYPE of string
      |	VALUE of string
      |	CASE of string
      |	DULE of string 
      |	SP of string
      | WILD of string
      | PATT of string
  end

module IndexType : IndexType =
  struct
    type t =
      |	TYPE of string
      |	VALUE of string
      |	CASE of string
      |	DULE of string 
      |	SP of string
      | WILD of string
      | PATT of string
  end


module IdIndex' 
    (IndexType : IndexType)
    (UpdateIndex : UpdateIndex
    with type IndexType.t = IndexType.t)
    : IdIndex =
  struct
    open IndexType

    type t = UpdateIndex.t
 
    let s2type s = UpdateIndex.find_update (TYPE s)
    let s2value s = UpdateIndex.find_update (VALUE s)
    let s2case s = UpdateIndex.find_update (CASE s)
    let s2dule s = UpdateIndex.find_update (DULE s)
    let s2sp s = UpdateIndex.find_update (SP s)
    let loc2wild s = UpdateIndex.find_update (WILD s)
    let loc2patt s = UpdateIndex.find_update (PATT s)

    let t2string i =
      match UpdateIndex.de_t i with
      |	TYPE s -> "type id " ^ s
      |	VALUE s -> "value id " ^ s
      |	CASE s -> "constructor id " ^ s
      |	DULE s -> "module id " ^ s
      |	SP s -> "specification id " ^ s
      |	WILD s -> "wild card at position " ^ s
      |	PATT s -> "pattern at position " ^ s
    let t2s i =
      match UpdateIndex.de_t i with
      |	TYPE s -> s
      |	VALUE s -> s
      |	CASE s -> s
      |	DULE s -> s
      |	SP s -> s
      |	WILD s -> "wild card at " ^ s
      |	PATT s -> "pattern at " ^ s

    let is_dule i =
      match UpdateIndex.de_t i with
      | DULE s -> true
      | _ -> false
    let is_sp i =
      match UpdateIndex.de_t i with
      | SP s -> true
      | _ -> false
    let is_patt i =
      match UpdateIndex.de_t i with
      | PATT _ -> true
      | _ -> false

    let eq = UpdateIndex.eq

    let stamp_t = UpdateIndex.stamp_t
  end

module UpdateIndexAVL = UpdateIndexAVL' (IndexType)
module IdIndex = IdIndex' (IndexType) (UpdateIndexAVL)


module type AtIndex =
  sig	
    module IdIndex : IdIndex
    (* these are reserved --- not accesible through lexer: *)
    val atu : IdIndex.t (* standard function space *)
    val atj : IdIndex.t (* argument part of a types's source *)
    val atk : IdIndex.t (* context part of a types's source *)
    val atd : IdIndex.t (* argument part of a value's domain *)
    val ate : IdIndex.t (* context part of a value's domain *)
    val atr : IdIndex.t (* main parameter for inductive modules *)
    val atc : IdIndex.t (* default case of case expression *)
    (* these are accesible: *)
    val it : IdIndex.t  (* default argument name *)
    val result : IdIndex.t (* name of the module holding the results *)
    val tt : IdIndex.t (* name of constructor of truth *)
    val ff : IdIndex.t (* name of constructor of falsity *)
    val mNat : IdIndex.t (* name of the Nat module *)
    val zero : IdIndex.t (* name of Nat.zero operation *)
    val one : IdIndex.t
    val two : IdIndex.t
    val three : IdIndex.t
    val four : IdIndex.t
    val five : IdIndex.t
    val six : IdIndex.t
    val seven : IdIndex.t
    val eight : IdIndex.t
    val nine : IdIndex.t
    val succ : IdIndex.t (* name of Nat.succ operation *)
    val arg_of_succ : IdIndex.t (* name of argument of Nat.succ *)
    val nil : IdIndex.t (* names of list constructors *)
    val cons : IdIndex.t
    val head : IdIndex.t (* names of record fields for lists *)
    val tail : IdIndex.t
    (* conversions: *)
    val sp2dule : IdIndex.t -> IdIndex.t
    val dule2sp : IdIndex.t -> IdIndex.t
  end

module AtIndex' 
    (IdIndex : IdIndex) 
    : (AtIndex 
       with module IdIndex = IdIndex) =
  struct
    module IdIndex = IdIndex
    open IdIndex

    let atu = s2value "@u"
    let atj = s2type "@j"
    let atk = s2type "@k"
    let atd = s2value "@d"
    let ate = s2value "@e"
    let atr = s2dule "@r"
    let atc = s2case "@c"

    let it = s2value "it"
    let result = s2dule "Result"
    let tt = s2case "True"
    let ff = s2case "False"
    let mNat = s2dule "Nat"
    let zero = s2value "zero"
    let one = s2value "one"
    let two = s2value "two"
    let three = s2value "three"
    let four = s2value "four"
    let five = s2value "five"
    let six = s2value "six"
    let seven = s2value "seven"
    let eight = s2value "eight"
    let nine = s2value "nine"
    let succ = s2value "succ"
    let arg_of_succ = s2value "n"
    let nil = s2case "Nil"
    let cons = s2case "Cons"
    let head = s2value "head"
    let tail = s2value "tail"

    let sp2dule i =
      assert (is_sp i);
      s2dule (t2s i)
    let dule2sp i =
      assert (is_dule i);
      s2sp (t2s i)
  end

module AtIndex = AtIndex' (IdIndex)


module type IListBasicForIList =
  sig
    module Index : Index

    type 'a t
    (* for list operations: *)
    val ind_ilist : 'a t -> (Index.t * 'a) list
    val nil : 'a t
    val cons : (Index.t * 'a) -> 'a t -> 'a t 
    exception EconsDuplication of string (* for parser only *)
    val econs : (Index.t * 'a) -> 'a t -> 'a t 
    val bmap : (Index.t * 'a -> 'b) -> 'a t -> 'b t
    val set_rigid : 'a t -> 'a t 
    val pp_stamp : 'a t -> string
  end

module type IListBasic =
  sig
    include IListBasicForIList

    module IListStamp : Stamp

    (* for stamp operations: *)
    val is_rigid : 'a t -> bool
    val is_flexible : 'a t -> bool
    val get_stamp : 'a t -> IListStamp.t
    val get_elem : 'a t -> 'a
    val rigid2flexible : IListStamp.t -> 'a -> 'a t -> 'a t 
    val move_flexible : 'a t -> 'a t -> 'a t 
  end

module IListBasic' 
    (Index : Index)
    (IListStamp : Stamp)
    : (IListBasic 
       with module Index = Index
       with module IListStamp = IListStamp) =
  struct
    module Index = Index
    module IListStamp = IListStamp

    type 'a t = 
      | Flexible of IListStamp.t * 'a * (Index.t * 'a) list
      | Rigid of (Index.t * 'a) list

    let ind_ilist l =
      match l with
      | Flexible (n, e, l) -> l
      | Rigid l -> l

    let nil = Rigid []

    let rec is_in i l =
      match l with
      | [] -> false
      | (j, v) :: r -> Index.eq j i || is_in i r

    let cons ((i, v) as iv) l =
      let l = ind_ilist l in
      assert 
	(if is_in i l then 
	  failwith ("IListBasic.cons: duplicated " ^ Index.t2string i) 
	else true);
      Rigid (iv :: l)

    exception EconsDuplication of string

    let econs ((i, v) as iv) l =
      let l = ind_ilist l in
      if is_in i l then 
	raise (EconsDuplication (Index.t2string i))
      else
	Rigid (iv :: l)

    let rec bmap' f l = (* for speed and, partly, for order *)
    (* bfold' nil (fun (i, v) r -> cons (i, f (i, v)) r) *)
      match l with
	| [] -> []
	| ((i, v) as iv) :: r -> (i, f iv) :: bmap' f r

    let bmap f l = Rigid (bmap' f (ind_ilist l))

    let set_rigid l = Rigid (ind_ilist l)

    let is_rigid l =
      match l with
      | Flexible (n, e, l) -> false
      | Rigid l -> true

    let is_flexible l =
      match l with
      | Flexible (n, e, l) -> true
      | Rigid l -> false

    let get_stamp l =
      match l with
      | Flexible (n, e, l) -> n
      | Rigid l -> assert false

    let get_elem l =
      match l with
      | Flexible (n, e, l) -> e
      | Rigid l -> assert false

    let rigid2flexible n e l =
      match l with
      | Flexible (n, e, l) -> assert false
      | Rigid l -> Flexible (n, e, l)

    let move_flexible l l' =
      assert (is_rigid l');
      match l with
      | Flexible (n, e, _) -> rigid2flexible n e l'
      | Rigid l -> assert false

    let pp_stamp l =
      match l with
      | Flexible (n, e, l) -> IListStamp.t2string n
      | Rigid l -> "R"
  end

module IListBasic = IListBasic' (IdIndex) (Stamp)


module type IList =
  sig
    module Index : Index
    module IListBasic : IListBasicForIList
    with module Index = Index

    type 'a t = 'a IListBasic.t

    val nil : 'a t
    val cons : (Index.t * 'a) -> 'a t -> 'a t 
    exception EconsDuplication of string
    val econs : (Index.t * 'a) -> 'a t -> 'a t 

    val is_nil : 'a t -> bool
    val is_in : Index.t -> 'a t -> bool
    val not_in : Index.t -> 'a t -> bool
    val bforall : (Index.t * 'a -> bool) -> 'a t -> bool
    val vforall : ('a -> bool) -> 'a t -> bool
    val iforall : (Index.t -> bool) -> 'a t -> bool
    val vexists : ('a -> bool) -> 'a t -> bool
    val subset : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val eqset : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    
    val bfold : 'a -> (Index.t * 'b -> 'a -> 'a) -> 'b t -> 'a
    val vfold : 'a -> ('b -> 'a -> 'a) -> 'b t -> 'a
    val ifold : 'a -> (Index.t -> 'a -> 'a) -> 'b t -> 'a
    val bmap : (Index.t * 'a -> 'b) -> 'a t -> 'b t
    val vmap : ('a -> 'b) -> 'a t -> 'b t
    val imap : (Index.t -> 'b) -> 'a t -> 'b t
    val rmap : (Index.t -> Index.t) -> 'a t -> 'a t
    val bfilter : (Index.t * 'a -> bool) -> 'a t -> 'a t
    val vfilter : ('a -> bool) -> 'a t -> 'a t
    val ifilter : (Index.t -> bool) -> 'a t -> 'a t
    val remove : Index.t -> 'a t -> 'a t
    val subtract : 'a t -> 'b t -> 'a t
    val diff : 'a t -> 'a t -> 'a t (* common values [==] *)
    val inter : 'a t -> 'a t -> 'a t (* common values [==] *)
    val (@@) : 'a t -> 'a t -> 'a t (* labels must be unique *)
    val find : Index.t -> 'a t -> 'a
    val find_ok : Index.t -> 'a t -> [`OK of 'a|`Error of string]
    val bchoose : 'a t -> [`OK of Index.t * 'a|`Error of string]
    val vchoose : 'a t -> [`OK of 'a|`Error of string]
    val ichoose : 'a t -> [`OK of Index.t|`Error of string]
    val append_eq : ('a -> 'a -> bool) ->
      'a t -> 'a t -> [`OK of 'a t|`Error of string]
    val bfold1ok : 'a -> (Index.t * 'b -> 'a -> [`OK of 'a|`Error of 's]) ->
      'b t -> [`OK of 'a|`Error of 's] 
    val vfold1ok : 'a -> ('b -> 'a -> [`OK of 'a|`Error of 's]) ->
      'b t -> [`OK of 'a|`Error of 's] 
    val ifold1ok : 'a -> (Index.t -> 'a -> [`OK of 'a|`Error of 's]) ->
      'b t -> [`OK of 'a|`Error of 's] 
    val bmap1ok : (Index.t * 'a -> [`OK of 'b|`Error of 's]) ->
      'a t -> [`OK of 'b t|`Error of 's]
    val vmap1ok : ('a -> [`OK of 'b|`Error of 's]) ->
      'a t -> [`OK of 'b t|`Error of 's]
    val imap1ok : (Index.t -> [`OK of 'b|`Error of 's]) ->
      'a t -> [`OK of 'b t|`Error of 's]
  end

module IList' 
    (Index : Index)
    (IListBasic : IListBasicForIList
     with module Index = Index)
    : (IList 
       with module Index = Index
       with module IListBasic = IListBasic) =
  struct
    module Index = Index
    module IListBasic = IListBasic
    open IListBasic

    type 'a t = 'a IListBasic.t

    let nil = nil
    let cons = cons
    exception EconsDuplication = EconsDuplication
    let econs = econs
    let bmap = bmap

    let is_nil l = (ind_ilist l = [])

    let rec is_in' i l =
      match l with
      | [] -> false
      | (j, v) :: r -> Index.eq j i || is_in' i r

    let is_in i l = is_in' i (ind_ilist l)

    let not_in i l = not (is_in i l)

    let rec bfold' init f l =
      match l with
      | [] -> init
      | iv :: r -> bfold' (f iv init) f r

    let bfold init f l = bfold' init f (ind_ilist l)

    let rec find_ok' i l =
      match l with
      | [] -> `Error (Index.t2string i)
      | (j, v) :: r -> if Index.eq i j then `OK v else find_ok' i r

    let find_ok i l = find_ok' i (ind_ilist l)

    let bforall p = 
      bfold true 
	(fun iv b -> 
	  (match b with
	  | true -> p iv
	  | false -> false))

    let vforall p = bforall (fun (i, v) -> p v)

    let iforall p = bforall (fun (i, v) -> p i)

    let vexists p l = not (vforall (fun v -> not (p v)) l)

    let subset eq l1 l2 =
      bforall (fun (i, v) -> 
        (match find_ok i l2 with
	|`OK x -> eq v x
	|`Error er -> false)) l1

    let length l = bfold 0 (fun _ r -> r + 1) l

    let eqset eq l1 l2 =
      l1 == l2 || (* eq is reflective *)   
      (length l1 = length l2 (* much quicker than with [subset eq l1 l2] *)
	 && subset eq l2 l1)

    let vfold init f = bfold init (fun (i, v) -> f v)

    let ifold init f = bfold init (fun (i, v) -> f i)

    let vmap f = bmap (fun (i, v) -> f v)
					 
    let imap f = bmap (fun (i, v) -> f i)

    let rmap f = bfold nil (fun (i, v) r -> cons (f i, v) r)

    let bfilter p = bfold nil (fun iv r -> 
      if p iv then cons iv r else r)

    let vfilter p = bfilter (fun (i, v) -> p v)

    let ifilter p = bfilter (fun (i, v) -> p i)

    let remove i = ifilter (fun j -> not (Index.eq i j))

    let subtract l1 l2 = 
      ifilter (fun i -> not_in i l2) l1

    let diff l1 l2 = 
      bfilter (fun (i, v1) -> 
	match find_ok i l2 with
	|`OK v2 -> 
	    assert (v1 == v2);
	    false 
	|`Error er -> 
	    true) l1

    let inter l1 l2 =
      bfilter (fun (i, v1) -> 
	match find_ok i l2 with
	|`OK v2 -> 
	    assert (v1 == v2);
	    true 
	|`Error er -> 
	    false) l1
	
    let (@@) l1 l2 = set_rigid (
      bfold l2 (fun iv r -> cons iv r) l1)

    let rec find' i l =
      match l with
	| [] -> failwith ("IList.find: " ^ Index.t2string i)
	| (j, v) :: r -> if Index.eq i j then v else find' i r

    let find i l = find' i (ind_ilist l)

    let bchoose l =
      match ind_ilist l with
      |	[] -> `Error "IList.choose: choice from empty ilist"
      |	(i, v) :: r -> `OK (i, v)

    let vchoose l = 
      match bchoose l with
      |`OK (i, v) -> `OK v
      |`Error er -> `Error er

    let ichoose l = 
      match bchoose l with
      |`OK (i, v) -> `OK i
      |`Error er -> `Error er

    let bfold1ok init f =
      bfold (`OK init)
	(fun iv r -> 
	  (match r with
	  |`OK r -> f iv r
	  |`Error s -> `Error s))

    let vfold1ok init f = bfold1ok init (fun (i, v) r -> f v r)

    let ifold1ok init f = bfold1ok init (fun (i, v) r -> f i r)

    let bmap1ok f =
      bfold1ok nil
	(fun ((i, v) as iv) b1 -> 
	  match f iv with
	  |`OK a1 -> 
	      `OK (cons (i, a1) b1)
	  |`Error s -> `Error s)

    let vmap1ok f = bmap1ok (fun (i, v) -> f v)

    let imap1ok f = bmap1ok (fun (i, v) -> f i)

    let append_eq eq l1 l2 = 
      bfold1ok l2
	(fun ((i, v) as iv) r -> 
	  if not_in i l2 then `OK (cons iv r) 
	  else if eq (find i l2) v then `OK r
	  else `Error ("append_eq: find " ^ Index.t2string i 
		       ^ " l2 <> v in ")) l1
  end

module IList = IList' (IdIndex) (IListBasic)


module type OkError =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t
    val bmap2ok : (IdIndex.t * 'a -> [`OK of 'b * 'c|`Error of 's]) ->
      'a IList.t -> [`OK of 'b IList.t * 'c IList.t|`Error of 's]
    val vmap2ok : ('a -> [`OK of 'b * 'c|`Error of 's]) ->
      'a IList.t -> [`OK of 'b IList.t * 'c IList.t|`Error of 's]
    val bmap3ok : (IdIndex.t * 'a -> [`OK of 'b * 'c * 'd|`Error of 's]) ->
      'a IList.t -> 
	[`OK of 'b IList.t * 'c IList.t * 'd IList.t|`Error of 's]
    val vmap3ok : ('a -> [`OK of 'b * 'c * 'd|`Error of 's]) ->
      'a IList.t -> 
	[`OK of 'b IList.t * 'c IList.t * 'd IList.t|`Error of 's]
    val bmap4ok : (IdIndex.t * 'a -> 
      [`OK of 'b * 'c * 'd * 'e|`Error of 's]) ->
      'a IList.t -> 
	[`OK of 'b IList.t * 'c IList.t * 'd IList.t * 'e IList.t
        |`Error of 's]
    val vmap4ok : ('a -> [`OK of 'b * 'c * 'd * 'e|`Error of 's]) ->
      'a IList.t -> 
	[`OK of 'b IList.t * 'c IList.t * 'd IList.t * 'e IList.t
        |`Error of 's]
    val bmap5ok : (IdIndex.t * 'a -> 
      [`OK of 'b * 'c * 'd * 'e * 'f|`Error of 's]) ->
      'a IList.t -> 
	[`OK of 'b IList.t * 'c IList.t * 'd IList.t * 'e IList.t * 'f IList.t
        |`Error of 's]
    val vmap5ok : ('a -> [`OK of 'b * 'c * 'd * 'e * 'f|`Error of 's]) ->
      'a IList.t -> 
	[`OK of 'b IList.t * 'c IList.t * 'd IList.t * 'e IList.t * 'f IList.t
        |`Error of 's]
    val for_all3 : ('a -> 'a -> bool) -> 'a -> 'a ->
      [`OK of 'a IList.t * 'a IList.t * 'd IList.t|`Error of string] ->
	[`OK of 'a * 'a * 'd IList.t |`Error of string]
    val for_all5 : ('a -> 'a -> bool) -> 'a -> 'a ->
      [`OK of 'a IList.t * 'a IList.t * 'd IList.t * 'e IList.t * 'f IList.t
      |`Error of string] ->
	  [`OK of 'a * 'a * 'd IList.t * 'e IList.t * 'f IList.t
          |`Error of string]
  end

module OkError'
    (IdIndex : IdIndex)
    (IList : IList with type Index.t = IdIndex.t)
    : (OkError
    with module IdIndex = IdIndex
    with module IList = IList) =
  struct
    module IdIndex = IdIndex
    module IList = IList
    open IList

    let bmap2ok f =
      bfold1ok (nil, nil)
	(fun ((i, v) as iv) (b1, b2) -> 
	  match f iv with
	  |`OK (a1, a2) -> 
	      `OK (cons (i, a1) b1, cons (i, a2) b2)
	  |`Error s -> `Error s)

    let vmap2ok f = bmap2ok (fun (i, v) -> f v)

    let bmap3ok f =
      bfold1ok (nil, nil, nil)
	(fun ((i, v) as iv) (b1, b2, b3) -> 
	  match f iv with
	  |`OK (a1, a2, a3) -> 
	      `OK (cons (i, a1) b1, cons (i, a2) b2, cons (i, a3) b3)
	  |`Error s -> `Error s)

    let vmap3ok f = bmap3ok (fun (i, v) -> f v)

    let bmap4ok f =
      bfold1ok (nil, nil, nil, nil)
	(fun ((i, v) as iv) (b1, b2, b3, b4) -> 
	  match f iv with
	  |`OK (a1, a2, a3, a4) -> 
	      `OK (cons (i, a1) b1, cons (i, a2) b2, cons (i, a3) b3,
		  cons (i, a4) b4)
	  |`Error s -> `Error s)

    let vmap4ok f = bmap4ok (fun (i, v) -> f v)

    let bmap5ok f =
      bfold1ok (nil, nil, nil, nil, nil)
	(fun ((i, v) as iv) (b1, b2, b3, b4, b5) -> 
	  match f iv with
	  |`OK (a1, a2, a3, a4, a5) -> 
	      `OK (cons (i, a1) b1, cons (i, a2) b2, cons (i, a3) b3,
		  cons (i, a4) b4, cons (i, a5) b5)
	  |`Error s -> `Error s)

    let vmap5ok f = bmap5ok (fun (i, v) -> f v)

    let for_all3 eq e c lclelf =
     match lclelf with
	`OK (lc, le, lf) -> 
	  if vforall (eq c) lc then
	    if vforall (eq e) le then
	      `OK (c, e, lf)
	    else `Error "not vforall le in "
	  else `Error "not vforall lc in "
      |`Error er -> `Error (er ^ ", in ")

    let for_all5 eq e c lclelflhlt =
      match lclelflhlt with
	`OK (lc, le, lf, lh, lt) -> 
	  if vforall (eq c) lc then
	    if vforall (eq e) le then
	      `OK (c, e, lf, lh, lt)
	    else `Error "not vforall le in "
	  else `Error "not vforall lc in "
      |`Error er -> `Error (er ^ ", in ")
end

module OkError = OkError' (IdIndex) (IList)


module type PpTools =
  sig
    module IdIndex : IdIndex
    module IList : IList with type Index.t = IdIndex.t

    type outt
	  
    val mute : bool ref
    val depth : int ref
    val printstring : outt -> string -> unit
    val printlist : 
	string -> string -> string ->
          (outt -> 'a -> unit) -> outt -> 'a IList.t -> unit
    val printlistbackquote : 
	string -> string ->
          (outt -> 'a -> unit) -> outt -> 'a IList.t -> unit
    val printraw : string -> (outt -> 'a -> unit) -> outt -> 'a -> unit
    val printraw2 : 
	string -> (outt -> 'a -> unit) -> (outt -> 'b -> unit) -> 
          outt -> 'a -> 'b -> unit
    val printbinop : 
	string -> (outt -> 'a -> unit) -> (outt -> 'b -> unit) -> 
          outt -> 'a -> 'b -> unit
    val printindex : outt -> IdIndex.t -> unit
    val printbackquote : outt -> IdIndex.t -> unit
    val printlabel : string -> (outt -> 'a -> unit) -> outt -> 'a -> unit
    val pp2str : (outt -> 'a -> unit) -> 'a -> string
  end

module PpTools'
    (IdIndex : IdIndex)
    (IList : IList
    with type Index.t = IdIndex.t)
    : (PpTools
    with module IdIndex = IdIndex
    with module IList = IList) =
  struct
    module IdIndex = IdIndex
    module IList = IList

    type outt = Format.formatter
	  
    let mute = ref false
    let depth = ref 5
	
    let index2str (i : IdIndex.t) : string = IdIndex.t2s i
    let backquote (i : IdIndex.t) : string = "`" ^ index2str i
						     
    let printstring (out : outt) (s : string) : unit = 
      Format.fprintf out "%s" s
    let printindex (out : outt) (i : IdIndex.t) : unit = 
      printstring out (index2str i)
    let printbackquote (out : outt) (i : IdIndex.t) : unit = 
      printstring out (backquote i)
	
    let printraw 
	(label:string)
        (p : outt -> 'a -> unit)
        (out : outt) (v : 'a) : unit =
      if (Format.pp_over_max_boxes out ()) then
        ()
      else
        (Format.fprintf out "@[<hov 2>%s(@," label;
         p out v;
         Format.fprintf out ")@]")
	
    let printraw2 
	(label:string)
        (p1 : outt -> 'a -> unit) (p2 : outt -> 'b -> unit) 
        (out : outt) (v1 : 'a) (v2 : 'b) =
      if (Format.pp_over_max_boxes out ()) then
        ()
      else
        (Format.fprintf out "@[<hov 2>%s(@," label;
         p1 out v1;
         Format.fprintf out ",@ ";
         p2 out v2;
         Format.fprintf out ")@]")
	
    let printlabel 
	(label:string)
        (p : outt -> 'a -> unit) (out : outt) (v : 'a) =
      if (Format.pp_over_max_boxes out ()) then
        ()
      else
        (Format.fprintf out "@[<hv 2>%s@ " label;
         p out v;
         Format.fprintf out "@]")
	
    let printlist2 
	(left:string) (right:string) 
	(pair : outt -> IdIndex.t -> 'a -> unit)
        (field_sep : outt -> unit) (out : outt) (l : 'a IList.t) =
      if (Format.pp_over_max_boxes out ()) then
        ()
      else
        (Format.fprintf out "@[<hv>%s" left;
         let (empty, _) = 
	   IList.bfold (true, out)
             (fun (i, c) (first, out) ->
               (if first then ()
               else field_sep out);
               pair out i c;
               (false, out)) l
         in
         Format.fprintf out "%s@]" right)
	
    let printlist 
	(left:string) (right:string) (sep:string)
        (p : outt -> 'a -> unit) (out : outt) (l : 'a IList.t) =
      printlist2 left right 
	(fun out i v -> printlabel (index2str i ^ " " ^ sep) p out v)
        (fun out -> 
	  Format.fprintf out ";@, ") out l
	
    let printlistbackquote 
	(left:string) (right:string)
        (p : outt -> 'a -> unit) (out : outt) (l : 'a IList.t) =
      printlist2 left right 
	(fun out i v -> printlabel (backquote i) p out v)
        (fun out -> 
	  Format.fprintf out "@,|") out l
	
    let printbinop 
	(sep:string)
        (p1 : outt -> 'a -> unit) (p2 : outt -> 'b -> unit) 
        (out : outt) (v1 : 'a) (v2 : 'b) =
      if (Format.pp_over_max_boxes out ()) then
        ()
      else
        (Format.fprintf out "@[<hv>";
         p1 out v1;
         Format.fprintf out "@ %s " sep;
         p2 out v2;
         Format.fprintf out "@]")
	
    let pp2str (pp: outt -> 'a -> unit) (v: 'a) : string =
      if !mute then "Pretty-printing muted by the --no-pp option!"
      else
	let
            b = Buffer.create 1
	in
	let     
            f = Format.formatter_of_buffer b
	in
	Format.pp_set_margin f 78 ;
	Format.pp_set_max_indent f 48;
	Format.pp_set_max_boxes f !depth;
	Format.pp_set_ellipsis_text f "...";
	pp f v ;
	Format.pp_print_flush f () ;
	Buffer.contents b
  end

module PpTools = PpTools' (IdIndex) (IList)


module type Type =
  sig
    module IdIndex : IdIndex
    module IList : IList 
    with type Index.t = IdIndex.t
    module VarStamp : Stamp

    type t
    val type_sub_app_f : (* memoized, so with side-effects! *)
	bool ->
	  (t -> t) lazy_t -> 
	    (t IList.t -> t IList.t) ->
	      (VarStamp.t -> t -> t) -> 
		t -> t 
    val f_COMP : t -> t -> t
    val f_pp_nil : t -> t
  end

module type Substitution =
  sig
    module IdIndex : IdIndex
    module IListStamp : Stamp
    module IListBasic : IListBasic 
    with type Index.t = IdIndex.t
    with type IListStamp.t = IListStamp.t
    module IList : IList 
    with type Index.t = IdIndex.t
    module VarStamp : Stamp
    module Type : Type
    with module IdIndex = IdIndex 
    with module IList = IList
    with module VarStamp = VarStamp

    type subst

    val sub_id : subst
    val sub_app_sub : subst -> subst -> subst
    val sub_comp : subst -> subst -> subst
    val sub_var : VarStamp.t -> Type.t -> subst
    val sub_ilist : IListStamp.t -> Type.t IList.t -> subst
    val sub_translate : 
	(VarStamp.t -> Type.t) -> 
	  (IListStamp.t -> Type.t IList.t) -> 
	    VarStamp.t -> IListStamp.t ->
	      VarStamp.t -> IListStamp.t ->
		VarStamp.t -> IListStamp.t -> 
		  subst * VarStamp.t * IListStamp.t

    val sub_app_f : subst -> Type.t -> Type.t
    val sub_app_lf : subst -> Type.t IList.t ->  Type.t IList.t
    val sub_finish_f : subst -> Type.t -> Type.t
    val sub_finish_lf : subst -> Type.t IList.t ->  Type.t IList.t

    val finish_sub : VarStamp.t -> IListStamp.t -> subst -> subst
    val finish_f : Type.t ->  Type.t
    val finish_lf : Type.t IList.t -> Type.t IList.t
  end

module Substitution''
    (IdIndex : IdIndex)
    (IListStamp : Stamp)
    (IListBasic : IListBasic 
    with type Index.t = IdIndex.t
    with type IListStamp.t = IListStamp.t)
    (IList : IList 
    with type Index.t = IdIndex.t
    with type 'a IListBasic.t = 'a IListBasic.t)
    (VarStamp : Stamp)
    (VLStamp : IList 
    with type Index.t = VarStamp.t)
    (ILStamp : IList 
    with type Index.t = IListStamp.t)
    (Type : Type
    with module IdIndex = IdIndex 
    with module IList = IList
    with module VarStamp = VarStamp)
    : (Substitution
    with module IdIndex = IdIndex
    with module IListStamp = IListStamp
    with module IListBasic = IListBasic
    with module IList = IList
    with module VarStamp = VarStamp
    with module Type = Type) =
  struct
    module IdIndex = IdIndex
    module IListStamp = IListStamp
    module IListBasic = IListBasic
    module IList = IList
    module VarStamp = VarStamp
    module Type = Type
    open IListBasic
    open IList

    type subst = Type.t VLStamp.t * Type.t IList.t ILStamp.t
	  
    let sub_id = (VLStamp.nil, ILStamp.nil)

    let sub_var var_num new_f = 
      (VLStamp.cons (var_num, new_f) VLStamp.nil, 
       ILStamp.nil)

    let sub_ilist l_stamp new_lf = 
      (VLStamp.nil, 
       ILStamp.cons (l_stamp, new_lf) ILStamp.nil)

    let sub_translate 
	f_var f_ilist
	var_num_start l_stamp_start
	var_num_end l_stamp_end
	var_num_current l_stamp_current =
      assert 
	(not 
	   (VarStamp.less var_num_end var_num_start ||
	   IListStamp.less l_stamp_end l_stamp_start));
      (* the folling is possible, because in [guess_comp] [var_num] is recycled
           VarStamp.less var_num_current var_num_end 
         we translate those high variable numbers just as well *)
      let rec inc_var_num var_num_start var_num_current lf =
	if VarStamp.eq var_num_start var_num_end then 
	  (lf, var_num_current)
	else 
	  let fv = f_var var_num_current in
	  let pair = (var_num_start, fv) in
	  inc_var_num 
	    (VarStamp.inc var_num_start) 
	    (VarStamp.inc var_num_current)
	    (VLStamp.cons pair lf)
      in 
      let rec inc_l_stamp l_stamp_start l_stamp_current llf =
	if IListStamp.eq l_stamp_start l_stamp_end then 
	  (llf, l_stamp_current)
	else 
	  let lv = f_ilist l_stamp_current in
	  let pair = (l_stamp_start, lv) in
	  inc_l_stamp 
	    (IListStamp.inc l_stamp_start) 
	    (IListStamp.inc l_stamp_current)
	    (ILStamp.cons pair llf)
      in 
      let (lf, var_num_new) = 
	inc_var_num var_num_start var_num_current VLStamp.nil in
      let (llf, l_stamp_new) = 
	inc_l_stamp l_stamp_start l_stamp_current ILStamp.nil in
      ((lf, llf), var_num_new, l_stamp_new)

    let sub_app_var s var_num old_f =
      let (lf, llf) = s in
      (match VLStamp.find_ok var_num lf with
      |`OK f -> f
      |`Error er -> old_f)

    let sub_app_lf sub_app_f_s s old_lf =
      let sub_app_f_s = Lazy.force_val sub_app_f_s in
      let ext_lf = vmap sub_app_f_s old_lf in
      if is_rigid old_lf then ext_lf
      else
	let (_, llf) = s in
	let l_stamp = get_stamp old_lf in
	let gs = get_elem old_lf in
	let gs = sub_app_f_s gs in
	(match ILStamp.find_ok l_stamp llf with
	|`OK lf ->
	    let lfd = subtract lf ext_lf in
	    let lgf = vmap (Type.f_COMP gs) lfd in
	    if is_rigid lf then ext_lf @@ lgf
	    else
	      let l_stamp = get_stamp lf in
	      let fs = get_elem lf in
	      let gfs = Type.f_COMP gs fs in
	      rigid2flexible l_stamp gfs (ext_lf @@ lgf)
	|`Error er -> 
	    rigid2flexible l_stamp gs ext_lf)

    let sub_app_f s =
      let rec sub_app_f_s = 
	lazy
	  (let sub_app_lf_s = sub_app_lf sub_app_f_s s in
	  let sub_app_var_s = sub_app_var s in
	  Type.type_sub_app_f (* memoized, so with side-effects! *)
	    false sub_app_f_s sub_app_lf_s sub_app_var_s)
      in Lazy.force_val sub_app_f_s

    let sub_app_lf s = sub_app_lf (lazy (sub_app_f s)) s

    let sub_app_sub s (lf2, llf2) =
      let lf = VLStamp.vmap (sub_app_f s) lf2 in
      let llf = ILStamp.vmap (sub_app_lf s) llf2 in
      (lf, llf)

    let sub_comp (lf1, llf1) (lf2, llf2) =
      let (lf1, llf1) = sub_app_sub (lf2, llf2) (lf1, llf1) in
      (VLStamp.(@@) lf1 lf2, ILStamp.(@@) llf1 llf2)

    let sub_finish_var s var_num old_f =
      let (lf, llf) = s in
      (match VLStamp.find_ok var_num lf with
      |`OK f -> f
      |`Error er -> Type.f_pp_nil old_f (* instead of variable! *))

    let sub_finish_lf sub_finish_f_s s old_lf =
      let sub_finish_f_s = Lazy.force_val sub_finish_f_s in
      let ext_lf = vmap sub_finish_f_s old_lf in
      if is_rigid old_lf then ext_lf
      else
	let (_, llf) = s in
	let l_stamp = get_stamp old_lf in
	(match ILStamp.find_ok l_stamp llf with
	|`OK lf ->
	    let lfd = subtract lf ext_lf in
	    let gs = get_elem old_lf in
	    let gs = sub_finish_f_s gs in
	    let lgf = vmap (Type.f_COMP gs) lfd in
	    lgf @@ ext_lf
	|`Error er -> ext_lf)

    let sub_finish_f s =
      let rec sub_finish_f_s =
	lazy
	  (let sub_finish_lf_s = sub_finish_lf sub_finish_f_s s in
	  let sub_finish_var_s = sub_finish_var s in
	  Type.type_sub_app_f (* memoized, so with side-effects! *)
	    true sub_finish_f_s sub_finish_lf_s sub_finish_var_s)
      in Lazy.force_val sub_finish_f_s

    let sub_finish_lf s = sub_finish_lf (lazy (sub_finish_f s)) s

    let finish_f = sub_finish_f sub_id
    let finish_lf = sub_finish_lf sub_id

    let finish_sub var_num l_stamp (lf1, llf1) = 
      let lf1 = VLStamp.bfold VLStamp.nil (fun (i, f) r ->
	if VarStamp.less var_num i then r
	else VLStamp.cons (i, finish_f f) r) lf1 
      in
      let llf1 = ILStamp.bfold ILStamp.nil (fun (i, lf) r ->
	if IListStamp.less l_stamp i then r
	else ILStamp.cons (i, finish_lf lf) r) llf1 
      in
      (lf1, llf1)
  end

module Substitution' = 
  Substitution'' (IdIndex) (IListBasic.IListStamp) (IListBasic) (IList) (Stamp)
    (IList' (Stamp) (IListBasic' (Stamp) (Stamp)))
    (IList' (Stamp) (IListBasic' (Stamp) (Stamp)))
