(* Copyright (C) 2006 Mikolaj Konarski
 *
 * This file is part of the IAngband rouge-like game engine.
 * IAngband is released under the GNU General Public License (GPL).
 * Please see the file LICENSE for license information.
 *)

module type Position =
sig

  type pos = {x : int; y : int; z : int}

end

module Position : Position =
struct

  type pos = {x : int; y : int; z : int}

end

open Position

module type Direction = 
sig

  type t

  val is_direction : char -> bool
  val chr2dir : char -> t
  val trans_pos : pos -> t -> pos
  val failure_message : t -> string
  val success_message : t -> string
  val must_fly : t -> bool
end

module Direction : Direction = 
struct 

  type t = 
    | DirN| DirNE| DirE| DirSE| DirS| DirSW| DirW| DirNW
    | DirHERE| DirDOWN| DirUP

  let is_direction c =
    c = '>' ||
    c = '<' ||
    (Char.code c >= Char.code '0' && 
     Char.code c <= Char.code '9')
      
  let chr2dir c =
    match c with
    | '8' -> DirN
    | '9' -> DirNE
    | '6' -> DirE
    | '3' -> DirSE
    | '2' -> DirS
    | '1' -> DirSW
    | '4' -> DirW
    | '7' -> DirNW
    | '5' -> DirHERE
    | '>' -> DirDOWN
    | '<' -> DirUP
    | _ -> failwith "Direction.chr2dir: illegal direction"

  let trans_pos pos dir =
    match dir with
    | DirN -> {x = pos.x; y = pos.y + 1; z = pos.z}
    | DirNE -> {x = pos.x + 1; y = pos.y + 1; z = pos.z}
    | DirE -> {x = pos.x + 1; y = pos.y; z = pos.z}
    | DirSE -> {x = pos.x + 1; y = pos.y - 1; z = pos.z}
    | DirS -> {x = pos.x; y = pos.y - 1; z = pos.z}
    | DirSW -> {x = pos.x - 1; y = pos.y - 1; z = pos.z}
    | DirW -> {x = pos.x - 1; y = pos.y; z = pos.z}
    | DirNW -> {x = pos.x - 1; y = pos.y + 1; z = pos.z}
    | DirHERE -> {x = pos.x; y = pos.y; z = pos.z}
    | DirDOWN -> {x = pos.x; y = pos.y; z = pos.z - 1}
    | DirUP -> {x = pos.x; y = pos.y; z = pos.z + 1}

  let failure_message dir =
    match dir with
    | DirHERE -> "You cannot stay in the same place!"
    | DirDOWN -> "You try to dig under your feet but to no avail!"
    | DirUP -> "You try to jump upwards but to no avail!"
    | _ -> "You bump into a wall!"

  let success_message dir =
    match dir with
    | DirDOWN -> "You enter a lower level."
    | DirUP -> "You enter a higher level."
    | _ -> ""

  let must_fly dir = 
    match dir with
    | DirUP -> true
    | _ -> false
end
