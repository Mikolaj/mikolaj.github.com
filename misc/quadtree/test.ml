(* Copyright (C) 2006 Mikolaj Konarski
 *
 * This file is part of the IAngband rouge-like game engine.
 * IAngband is released under the GNU General Public License (GPL).
 * Please see the file LICENSE for license information.
 *)

open Position
open Item
open Feature
open Dungeon

open Position

(* 
do monster genes and natural selection, reward for damaging player
proto_item with
paint '.' only when the lower level square is '#'
paint ' ' if lower level is '>' or ' ', '<' if upper level is ' '
*)

module type IAngbandItems =
sig

  val items : Item.t list

end

module IAngbandItems : IAngbandItems =
struct

  let items = 
    [Item.define ~name:"Short Sword";
     Item.define ~name:"Long Bow";
     Item.define ~name:"Chain Mail";
     Item.define ~name:"Gold"]

end

module type IAngbandMonsters =
sig

  val monsters : Monster.t list

end

module IAngbandMonsters : IAngbandMonsters =
struct

  let monsters = 
    [Monster.define ~name:"Doggy" ~kind:`Canine]

end

module type IAngbandPCs =
sig

  val pcs : PC.t list

end

module IAngbandPCs : IAngbandPCs =
struct

  let pcs = 
    [PC.define ~name:"Grumbly" ~kind:`Warrior]

end

module type IAngbandMovables =
sig

  val movables : int -> Movable.t

end

module IAngbandMovables : IAngbandMovables =
struct

  let movables = fun i ->
    if i = 1 then
      Movable.define ~id:1 ~pos:{x = 0; y = 0; z = 0} 
	~kind:(`PC(List.hd IAngbandPCs.pcs))
    else if i = 2 then
      Movable.define ~id:2 ~pos:{x = 2; y = 1; z = 0} 
	~kind:(`Monster(List.hd IAngbandMonsters.monsters))
    else if i = 3 then
      Movable.define ~id:3 ~pos:{x = 4; y = 6; z = 0} 
	~kind:(`Item(List.hd IAngbandItems.items))
    else failwith "unknown movable"

end

(* terminal black magic *)

let original_tcio =
  (try Unix.tcgetattr Unix.stdin with
  | Unix.Unix_error _ -> 
      prerr_endline "This stdin is not a terminal!";
      exit 1)

let cols = ref 0
let rows = ref 0

let cols_rows =
  (* FIXME: autodetection too difficult for me, now *)
  let _ = cols := 80 in
  let _ = rows := 24 in
  ()

let init_term () =
  let tcio = Unix.tcgetattr Unix.stdin in
  tcio.Unix.c_echo <- false;
  tcio.Unix.c_icanon <- false;
  tcio.Unix.c_vmin <- 1;
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN tcio

let shutdown_term () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN original_tcio

let buf = Buffer.create (!cols * !rows)

let rec loop dungeon actor message =
  Buffer.clear buf;
  let buf = DrawDungeon.draw 
      buf message dungeon (IAngbandMovables.movables) 
      (Movable.pos actor) !cols !rows in
  Buffer.output_buffer stdout buf;
  flush stdout;
  let c = input_char stdin in
  let (new_dungeon, new_actor, message) = 
    Interpret.interpret dungeon actor (Command.parse_command c) in
  loop new_dungeon new_actor message
    
;;

init_term ()
;;
print_endline ""
;;
try loop (Dungeon.define 
	    [({x = 0; y = 0; z = 0},
	      Field.construct 
		{Field.feature = Feature.Air;
		 Field.seen = true;
		 Field.items = [];
		 Field.actor = `One(1)})])
    (IAngbandMovables.movables 1) "Use numbers to move (Num Lock helps); > and < to go up and down" with
| e -> shutdown_term ()
;;
shutdown_term ()
;;
