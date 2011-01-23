(* Copyright (C) 2006 Mikolaj Konarski
 *
 * This file is part of the IAngband rouge-like game engine.
 * IAngband is released under the GNU General Public License (GPL).
 * Please see the file LICENSE for license information.
 *)

open Position
open Position
open Feature
open Quadtree

module type Octree =
sig
  type t

  val empty : t

  val add : pos -> Field.t -> t -> t

  val find : pos -> t -> Field.t
end

module Octree : Octree =
struct

  type t = Field.t Quadtree.t array 

  let size = 1048576
  let cornerSW = -524288
(* 
one step is 5 meters; 
1000000 steps = 5000 km;
enough for x and y of a continent larger than Australia;
5000 steps = 25 km;
4000--5000 steps up --- way too little oxygen to breathe; 
dramatic HP loss; no structures possible, only air and ice clouds
4000--5000 steps down --- 700 degrees Celsius, 
too hot to breathe, HP loss even with fire resistance, 
the levels are all magma lakes; hot and extra hot
*)

  let empty = Array.make 10000 (Quadtree.empty size cornerSW (Field.void))

  let find pos d = Quadtree.find pos.x pos.y d.(pos.z + 5000)

  let add pos f d = d.(pos.z + 5000) 
    <- Quadtree.add pos.x pos.y f d.(pos.z + 5000); d
end

