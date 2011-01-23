(* Copyright (C) 2006 Mikolaj Konarski
 *
 * This file is part of the IAngband rouge-like game engine.
 * IAngband is released under the GNU General Public License (GPL).
 * Please see the file LICENSE for license information.
 *)

open Position

module type Item =
sig

  type t

  val name : t -> string
  val chr : t -> char
  val define : name:string -> t
end

module Item : Item =
struct

  type t = 
      {name : string;
       level : int;
       weight : int;
       price : int;
       chr : char}

  let name item = item.name

  let chr item = item.chr

  let define ~name =
    {name = name;
     level = 1;
     weight = 0;
     price = 0;
     chr = '$'}
end
