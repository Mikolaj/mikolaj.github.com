(* Copyright (C) 2006 Mikolaj Konarski
 *
 * This file is part of the IAngband rouge-like game engine.
 * IAngband is released under the GNU General Public License (GPL).
 * Please see the file LICENSE for license information.
 *)

open Position
open Item
open Feature
open Octree

open Position
open Field

module type Dungeon =
sig

  type t

  val chr : t -> (int -> Movable.t) -> pos -> char
  val find_feature : t -> pos -> t * Feature.t
  val update_actor : t -> pos -> pos -> t
  val define : (pos * Field.t) list -> t
end

module Dungeon : Dungeon =
struct

  type t = (pos -> Field.t) * Octree.t

  let find pos (_, d) = Octree.find pos d

  let add pos f (x, d) = (x, Octree.add pos f d)

  let find_feature dungeon pos =
    let f = find pos dungeon in
    if not (Field.equal f Field.void) then
      (dungeon, f.Hashcons.node.feature)
    else
      let new_field = (fst dungeon) pos in
      let new_dungeon = add pos new_field dungeon in
      (new_dungeon, new_field.Hashcons.node.feature)

  let chr dungeon fim pos = 
    let rec chrec dungeon fim pos stop =
      let (dungeon (*FIXME*), _) = find_feature dungeon pos in
      let f = find pos dungeon in
      (match f.Hashcons.node.actor with
      |`None ->
	  (match f.Hashcons.node.items with
	  | [] ->
	      if not (Feature.invisible f.Hashcons.node.feature) then
		Feature.chr f.Hashcons.node.feature
	      else
		let pos_down = {x = pos.x; y = pos.y; z = pos.z - 1} in
		if stop then ' ' else
		(match chrec dungeon fim pos_down true with
		| '#' -> '.'
		| _ -> ' ')
	  | h::t -> 
	      Movable.chr (fim h))
      |`One a -> Movable.chr (fim a))
    in chrec dungeon fim pos false

  let update_actor dungeon pos new_pos =
    let f = find pos dungeon in
    (match f.Hashcons.node.actor with
    |`None -> failwith "Dungeon.update_actor: no beings"
    |`One a ->
	let new_f = find new_pos dungeon in
        (* TODO: what if there is a being? ;> *)
	add pos
	  (construct
	     {feature = f.Hashcons.node.feature;
	      seen = f.Hashcons.node.seen;
	      items = f.Hashcons.node.items;
	      actor = `None})
	     (add new_pos
		(construct 
		   {feature = new_f.Hashcons.node.feature;
		    seen = f.Hashcons.node.seen;
		    items = new_f.Hashcons.node.items;
		    actor = `One(a)})
		dungeon))

  let test_desc = fun p -> 
  if p = {x = 2; y = 1; z = 0} then
    construct
	    {feature = Feature.Air;
	     seen = true;
	     items = [];
	     actor = `One(2)}
  else if p = {x = 2; y = 5; z = 0} then
    construct
	    {feature = Feature.Air;
	     seen = true;
	     items = [3];
	     actor = `None}
  else if p.x = 1 && p.y = 2 then
    construct
	    {feature = Feature.Stairs;
	     seen = true;
	     items = [];
	     actor = `None}
  else if p.x = 3 && p.y = 1 && p.z mod 2 = -1 then
    construct 
	    {feature = Feature.Stairs;
	     seen = true;
	     items = [];
	     actor = `None}
  else if p.z mod 2 = 1 || p.z mod 2 = -1 then
    construct 
	    {feature = Feature.Granite;
	     seen = true;
	     items = [];
	     actor = `None}
  else if p.y > 5 then 
    construct
	    {feature = Feature.Granite;
	     seen = true;
	     items = [];
	     actor = `None}
  else if p.x < -2 then
    construct
	    {feature = Feature.Voidgate {x = 0; y = 0; z = 0};
	     seen = true;
	     items = [];
	     actor = `None}
  else
    construct
	    {feature = Feature.Air;
	     seen = true;
	     items = [];
	     actor = `None}

  let rec define l =
     match l with
     | [] -> (test_desc, Octree.empty)
     | (pos, field)::t -> 
	 let d = define t in
	 add pos field d
end

module type Interpret =
sig

  val interpret :
      Dungeon.t -> Movable.t -> Command.command -> 
	Dungeon.t * Movable.t * string    
      
end

module Interpret : Interpret =
struct

  let interpret dungeon actor command =
    let pos = Movable.pos actor in
    match command with
    |`Direction(dir) ->
	let (dungeon, old_feature) = Dungeon.find_feature dungeon pos in
	let new_pos = Direction.trans_pos pos dir in
	let (dungeon, feature) = Dungeon.find_feature dungeon new_pos in
	if not (Feature.exitable old_feature dir actor
		  && Feature.enterable feature actor) then
	  (dungeon, actor, Direction.failure_message dir)
	else
	  (* FIXME: create all fields visible from new_pos *)
	  (Dungeon.update_actor dungeon pos new_pos, 
	   Movable.update_pos actor new_pos, 
	   Direction.success_message dir)
    |`Activate -> 
	let (dungeon, feature) = Dungeon.find_feature dungeon pos in
	(match Feature.is_voidgate feature with
	|`Yes p -> 
	    (Dungeon.update_actor dungeon pos p, 
	     Movable.update_pos actor p, 
	     "Your body aches as you slowly pass through the void gate.")
	|`No -> (dungeon, actor, 
		 "You see nothing to activate!"))
    |`Help -> (dungeon, actor, "DON'T PANIC!")

end

module type DrawDungeon =
sig

  val draw : Buffer.t -> string -> Dungeon.t -> (int -> Movable.t) 
    -> pos -> int -> int -> Buffer.t

end

module DrawDungeon: DrawDungeon =
struct

  let draw_message buf message width =
    let i = String.length message in
    let padding = String.make (width - i) ' ' in
    let buf = Buffer.add_string buf (message ^ padding); buf in
    buf

  let rec draw_line buf dungeon fim pos width =
    if width = 0 then buf
    else 
      let chr = Dungeon.chr dungeon fim pos in
      let next_pos = {x = pos.x + 1; y = pos.y; z = pos.z} in
      let buf = Buffer.add_char buf chr; buf in
      draw_line buf dungeon fim next_pos (width - 1)

  let rec draw_rectangle buf dungeon fim pos width height =
    if height = 0 then buf
    else
      let buf = draw_line buf dungeon fim pos width in
      let next_pos = {x = pos.x; y = pos.y - 1; z = pos.z} in
      draw_rectangle buf dungeon fim next_pos width (height - 1)

  let draw buf message dungeon fim center width height =
    let start_pos = 
      {x = center.x - width / 2;
       y = center.y + height / 2 - 1;
       z = center.z}
    in
    let buf = draw_message buf message width in
    let buf = draw_rectangle buf dungeon fim start_pos width (height - 1) in
    buf
end
