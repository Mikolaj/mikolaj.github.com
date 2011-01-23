(* Copyright (C) 2006 Sebastian Kaczanowski
 *
 * This file is part of the IAngband rouge-like game engine.
 * IAngband is released under the GNU General Public License (GPL).
 * Please see the file LICENSE for license information.
 *)

open Quadtree

module type Quadutils =
sig
  (*type 'a t*)

  val string_of_rectangle : ('a -> string)-> 'a Quadtree.t -> int -> int -> int -> int -> string

  val string_of_quadtree : ('a -> string)-> 'a Quadtree.t -> string
end

module Quadutils : Quadutils =
struct
  (*type 'a t = Quadtree.t*)

  let rec string_of_quadtree_row toString tree y xEnd actX  =
    if actX > xEnd then ""
    else
      (toString (Quadtree.find actX y tree)) 
      ^ (string_of_quadtree_row toString tree y xEnd (actX + 1))

  let string_of_rectangle toString tree xStart yStart xEnd yEnd =
    let rec pqr actY =
      if actY < yStart then ""
      else
        (string_of_quadtree_row toString tree actY xEnd xStart) 
        ^ "\n" ^ (pqr (actY - 1))
      in
        pqr yEnd

  let string_of_quadtree toString tree =
    let xStart = Quadtree.beginX tree in
    let yStart = Quadtree.beginY tree in
    let size = Quadtree.getSize tree in
    let ends start = start + size - 1 in
    let endX = ends xStart in
    let endY = ends yStart
    in
      string_of_rectangle toString tree xStart yStart endX endY
  
end
