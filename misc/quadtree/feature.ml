(* Copyright (C) 2006 Mikolaj Konarski
 *
 * This file is part of the IAngband rouge-like game engine.
 * IAngband is released under the GNU General Public License (GPL).
 * Please see the file LICENSE for license information.
 *)

open Position
open Item

open Position

module type Command =
sig

  type command = 
    [`Direction of Direction.t
    |`Activate
    |`Help]

  val parse_command : char -> command
end

module Command : Command =
struct

  type command = 
    [`Direction of Direction.t
    |`Activate
    |`Help]

  let parse_command c =
    if Direction.is_direction c then
      `Direction(Direction.chr2dir c)
    else
      match c with
      | 'A' -> `Activate
      | 'h' -> `Help
      | _ -> failwith "Illegal command"
end

module type Monster =
sig

  type kind = [`Canine|`Dragon]

  type t

  val name : t -> string
  val chr : t -> char
  val define : name:string -> kind:kind -> t
end

module Monster : Monster =
struct

  type kind = [`Canine|`Dragon]

  type t =
      {name : string;
       kind : kind}

  let name monster = monster.name

  let chr monster =
    match monster.kind with
    |`Canine -> 'C'
    |`Dragon -> 'd'

  let define ~name ~kind =
    {name = name;
     kind = kind}
end

module type PC =
sig

  type kind = [`Warrior|`Mage]

  type t

  val name : t -> string
  val chr : t -> char
  val define : name:string -> kind:kind -> t
end

module PC : PC =
struct

  type kind = [`Warrior|`Mage]

  type t =
      {name : string;
       kind : kind}

  let name character = character.name

  let chr character =
    match character.kind with
    |`Warrior -> '@' (* TODO: distinguish by color *)
    |`Mage -> '@'

  let define ~name ~kind =
    {name = name;
     kind = kind}
end

module type Movable =
sig

  type kind = [`Item of Item.t|`Monster of Monster.t|`PC of PC.t]

  type t

  val id : t -> int
  val pos : t -> pos
  val kind : t -> kind
  val chr : t -> char
  val define : id:int -> pos:pos -> kind:kind -> t
  val update_pos : t -> pos -> t
end

module Movable : Movable =
struct

  type kind = [`Item of Item.t|`Monster of Monster.t|`PC of PC.t]

  type t =
      {id : int;
       pos : pos;
       kind : kind}

  let id movable = movable.id
  let pos movable = movable.pos
  let kind movable = movable.kind
  let chr movable =
    match movable.kind with
    |`Item m -> Item.chr m
    |`Monster m -> Monster.chr m
    |`PC m -> PC.chr m
  let define ~id ~pos ~kind =
    {id = id;
     pos = pos;
     kind = kind}
  let update_pos movable pos =
    {id = movable.id;
     pos = pos;
     kind = movable.kind}
end

module type Feature =
sig

(*  type t uncommented for tests *)
  type t = Void| Air| Soil| Stairs| Voidgate of pos| Granite

  val enterable : t -> Movable.t -> bool
  val exitable : t -> Direction.t -> Movable.t -> bool
  val seethrough : t -> bool
  val invisible : t -> bool
  val is_voidgate : t -> [`No|`Yes of pos]
  val chr : t -> char
  val default : t
end

module Feature : Feature =
struct

  type t = Void| Air| Soil| Stairs| Voidgate of pos| Granite

  let enterable feature actor =
    match feature with
    | Void -> false
    | Air -> true
    | Stairs -> true
    | Voidgate _ -> true
    | Granite| Soil -> false

  let exitable feature dir actor =
    match feature with
    | Void -> false
    | Air -> if Direction.must_fly dir then false (* can_fly actor *) else true
    | Stairs -> true 
    | Voidgate _ -> 
	if Direction.must_fly dir then false (* can_fly actor *) else true
    | Granite| Soil -> false (* unless wraithform *)

  let seethrough feature =
    match feature with
    | Void -> false
    | Air -> true
    | Stairs -> false
    | Voidgate _ -> true
    | Granite| Soil -> false

  let invisible feature =
    match feature with
    | Void -> false
    | Air -> true
    | Stairs -> false
    | Voidgate _ -> false
    | Granite| Soil -> false

  let is_voidgate feature =
    match feature with
    | Voidgate p -> `Yes(p)
    | _ -> `No

  let chr feature =
    match feature with
    | Void -> 'x'
    | Air -> ' '
    | Stairs -> '>'
    | Voidgate _ -> '+'
    | Granite -> '#'
    | Soil -> '#'

  let default = Air
end

module type Field =
sig

  type t = t' Hashcons.hash_consed
  and t' =
      {feature : Feature.t;
       seen : bool;
       items : int list;
       actor : [`None|`One of int]}

  val construct : t' -> t 

  val void : t

  val equal : t -> t -> bool
end

module Field : Field =
struct

  type t = t' Hashcons.hash_consed
  and t' =
      {feature : Feature.t;
       seen : bool;
       items : int list;
       actor : [`None|`One of int]}

  module FieldHashedType : (Hashcons.HashedType with type t = t') =
    struct
      type t = t'
      let equal = (=)
      let hash = Hashtbl.hash
    end

  module HashedField = Hashcons.Make (FieldHashedType) 

  let tbl = HashedField.create 31
      
  let construct f = HashedField.hashcons tbl f

  let void = 
    construct
      {feature = Feature.Void;
       seen = false;
       items = [];
       actor = `None}

  let equal f1 f2 = (f1 == f2)
end


  
