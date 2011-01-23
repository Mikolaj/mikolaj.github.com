(* Copyright (C) 2006 Sebastian Kaczanowski and Mikolaj Konarski
 *
 * This file is part of the IAngband rouge-like game engine.
 * IAngband is released under the GNU General Public License (GPL).
 * Please see the file LICENSE for license information.
 *)
 
(*
  Dictionary:
    RR - rectangle representation int int int int - x1 y1 x2 y2 and x1 <= x2 and y1 <= y2
*)
module type Quadtree =
sig
  type 'a t
  
  (*
    params:
      size
      cornerSW
      default
    return quadtree started from cornerSW with fields amount equal size * size
      and all values set to default (tree with one leaf = Leaf default)
  *)
  val empty : int -> int -> 'a -> 'a t
  
  (*
    params:
      x
      y
      newValue
      tree
    return quadtree which may differs from tree only at position x, y
      and got at this position newValue
  *)
  val add : int -> int -> 'a -> 'a t -> 'a t

  (*
    params:
      x
      y
      tree
    return value set in tree at position x, y
  *)
  val find : int -> int -> 'a t -> 'a

  (*
    params:
      tree
      value
    return true if tree contain value else false
  *)
  val contain : 'a t -> 'a -> bool

  (* (!!)Jak to zrobic, zeby moc jednak uzyc typu 'a q, o ktory tu tak naprawde chodzi
  wydaje mi sie, ze kazdy wezel powinien byc typu 'a t *)
  (* 4 params:
       1 - function to fold nodes (may be use to get foldLeaf in desired order) 
           (gets 4 subnodes 
           function to operate on one node with given function to operate on this subnode
             (in most cases this function is same as param 2
             actual result and return actual result, 
           returns result of operate on node)
       2 - function to operate on leaf
       3 - start result
       4 - tree
   *)
(*  val fullFold : ('c -> 'c -> 'c -> 'c -> ( ('a -> 'b -> 'b) -> 'c -> 'b -> 'b) -> ('a -> 'b -> 'b) -> 'b -> 'b  ) -> ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
*)

  (*
    params:
      f
      b initial accumulator value
      tree
    return f counted on evry leaf in tree
  *)
  val foldLeavs : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b

  (*
    counts number of leaves in whole tree
  *)
  val countLeavs : 'a t -> int

  (*
    counts number of nodes without leaves in whole tree
  *)
  val countNodes : 'a t -> int

  (*
    params:
      tree
      color
      RR
    return quadtree differs from tree only in fields included in RR
      these fields are filled by color
  *)
  val fillRectangle : 'a t -> 'a -> int -> int -> int -> int -> 'a t 

  (*
    params:
      tree
      f function which takes number of Leaf filds which are included in RR
        actual Leaf value, 
        accumulator value (counted for now from previous leavs)
        and return desired value
      b initial acumulator value
      RR
      return desired value counted on evry leaf once
        but based one fields number really included in RR
  *)
  val checkRectangleWithCount : 'a t -> ('a -> int -> 'b -> 'b ) -> 'b -> int -> int -> int -> int -> 'b

  (*
    params:
      tree
      f function which takes actual Leaf value, 
        accumulator value (counted for now from previous leavs)
        and return desired value
      b initial acumulator value
      RR
      return desired value counted on evry leaf once
        (remember that leaf can represent planty of fields)
  *)
  val checkRectangle : 'a t -> ('a -> 'b -> 'b) -> 'b -> int -> int -> int -> int -> 'b

  (*
    params:
      tree
      f function from tree value to bool
      RR
    return true if f return true evrywhere else false
  *)
  val forAllInRectangle : 'a t -> ('a -> bool) -> int -> int -> int -> int -> bool

  (*
    params:
      tree
      f function from tree value to bool
      RR
    return true if f returns true in any field else return false
  *)
  val existInRectangle : 'a t -> ('a -> bool) -> int -> int -> int -> int -> bool

  (* params:
       tree
       color
       RR
     return number of fields in rectangle with color set to color *)
  val countColor : 'a t -> 'a -> int -> int -> int -> int -> int

  (* 
    executs function on evry leaf in tree 
    and return tree with leavs affected by this function 
   *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (*
    params:
      t1simplefy - checks if this is a case in which value in returned tree can be count without information contained in tree2
      t2simplefy - checks if this is a case in which value in returned tree can be count without information contained in tree1
      t1example - example value of type from tree1
      t2example - example value of type from tree2
      joinBy - counts value of returned tree from tree1 and tree2 values
      newCornerSW - start point of returned tree
      tree1 - any proper tree
      tree2 - any proper tree
    return tree affected by joinBy with size of larger from tree1.size and tree2.size
      (joining process goes through structures not position)
  *)
  val map2Trees : ('a -> bool) -> ('b -> bool) -> 'a -> 'b -> ('a -> 'b -> 'c) -> int -> 'a t -> 'b t -> 'c t 

  (*
    params:
      tree1
      tree2
      newCornerSW
    return a tree which is an intersection of tree1 and tree2 and with a cornerSw in newCornerSW.
      Operation is size independent and new size of a tree will be max(tree1.size, tree2.size)
  *)
  val intersect : bool t -> bool t -> int -> bool t

  (*
    params:
      tree1
      tree2
      newCornerSW
    return a tree which is an sum of tree1 and tree2 and with a cornerSw in newCornerSW.
      Operation is size independent and new size of a tree will be max(tree1.size, tree2.size)
  *)
  val sum : bool t -> bool t -> int -> bool t

  (*
    params:
      tree
      permute - 
  *)
  (* patrz (!!)
  val effect : 'a t -> ('a -> 'a -> 'a -> 'a -> 'b q) -> 'b t
  *)

  (*
    return tree horizontaly reflected
    (works properly with trees with size equal to power of 2)
  *)
  val flipHorizontal : 'a t -> 'a t

  (*
    return tree verticaly reflected
    (works properly with trees with size equal to power of 2)
  *)
  val flipVertical : 'a t -> 'a t

  (*
    return tree reflected along \ (back slash) axis
    (works properly with trees with size equal to power of 2)
  *)
  val flipBackSlash : 'a t -> 'a t

  (*
    return tree reflected along / (slash) axis
    (works properly with trees with size equal to power of 2)
  *)
  val flipSlash : 'a t -> 'a t

  (*
    return tree rotated 90 degrees
    (works properly with trees with size equal to power of 2)
  *)  
  val rotate90 : 'a t -> 'a t

  (*
    return tree rotated 180 degrees
    (works properly with trees with size equal to power of 2)
  *)  
  val rotate180 : 'a t -> 'a t

  (*
    return tree rotated 270 degrees
    (works properly with trees with size equal to power of 2)
  *)  
  val rotate270 : 'a t -> 'a t

  (*
    params:
      resizePatern
      tree
    return tree two times smaller then param tree
      In a situation when 4 leaf becomes one leaf resizePattern is used to get leaf value.
  *)
  val zoomOut : ('a -> 'a -> 'a -> 'a -> 'a) -> 'a t -> 'a t

  (*
    return two times larger tree
  *)
  val zoomIn : 'a t -> 'a t

  val getSize : 'a t -> int

  val beginX : 'a t -> int
  
  val beginY : 'a t -> int
end

module Quadtree : Quadtree =
struct

  type rectInRect = 
    | Fully 
    | Partial 
    | No 

  type 'a q =
    | Leaf of 'a
    | Node of 'a q * 'a q * 'a q * 'a q

  type 'a t =
      {size : int;
       cornerSW : int;
       tree : 'a q}

  let firstUpperExp2 x =
    let rec fUE2 x lastChecked =
      if lastChecked >= x then lastChecked
      else fUE2 x (lastChecked * 2)
    in
      fUE2 x 1

  let getSize tree = tree.size

  let beginX tree = tree.cornerSW

  let beginY tree = beginX tree
    

  let empty size cornerSW default = 
    {size = size;
     cornerSW = cornerSW;
     tree = Leaf(default)}

  let find posx posy t =
    let rec div x y k t =
      assert (k > 0);
      match t with
      | Leaf v -> v
      | Node (t1, t2, t3, t4) ->
          (* t2 t4 *)
          (* t1 t3 *)
	  let nk = k / 2 in
	  let nx = x + nk in
	  let ny = y + nk in
	  if posx < nx then
	    if posy < ny then
	      div x y nk t1
	    else div x ny nk t2
	  else
	    if posy < ny then
	      div nx y nk t3
	    else div nx ny nk t4
    in
    div t.cornerSW t.cornerSW (firstUpperExp2 t.size) t.tree

  (* create an optimal tree from optimal subtrees *)
  let glue (t1, t2, t3, t4) =
    match (t1, t2, t3, t4) with
    | (Leaf v1, Leaf v2, Leaf v3, Leaf v4)
      when v1 = v2 && v2 = v3 && v3 = v4
      -> t1
    | _ -> Node (t1, t2, t3, t4)


  (* add value [v] in [t] at coordinates [(posx, posy)] *)
  let add posx posy v t =
    let rec div x y size t =
      (* [t[ represents the square of [size] with SW corner at [(x, y)] *)
      assert (size > 0);
      match t with
      | Leaf old_v ->
	  if old_v = v then t
	  else
	    if size = 1 then Leaf v
	    else
	      let node = Node (t, t, t, t) in 
	      div x y size node
      | Node (t1, t2, t3, t4) ->
	  assert (size > 1);
	  let nsize = size / 2 in
	  let nx = x + nsize in
	  let ny = y + nsize in
	  let t = 
	    if posx < nx then
	      if posy < ny then
		(div x y nsize t1, t2, t3, t4)
	      else (t1, div x ny nsize t2, t3, t4)
	    else
              
	      if posy < ny then
		(t1, t2, div nx y nsize t3, t4)
	      else (t1, t2, t3, div nx ny nsize t4)
	  in
	  glue t
    in
    {size = t.size;
     cornerSW = t.cornerSW;
     tree = div t.cornerSW t.cornerSW (firstUpperExp2 t.size) t.tree}

  let contain t x =
    let rec treeContain t =
        match t with 
        | Leaf actX -> x = actX
        | Node( t1, t2, t3, t4) -> treeContain(t1) || treeContain(t2) || treeContain(t3) || treeContain(t4)
    in
      treeContain t.tree

  let fullFold fNode fLeaf b tree =
    let rec fF actFLeaf t actB =
      match t with
      | Leaf actA -> actFLeaf actA actB
      | Node( t1, t2, t3, t4) -> fNode t1 t2 t3 t4 fF actFLeaf actB
    in
      fF fLeaf tree.tree b 

  let standardNodeFunction t1 t2 t3 t4 fF f actB =
     fF f t4 (fF f t3 (fF f t2 (fF f t1 actB)))

  let foldLeavs f b tree =
    fullFold standardNodeFunction f b tree
(*    let fNode = (fun t1 t2 t3 t4 fF f actB -> fF f t4 (fF f t3 (fF f t2 (fF f t1 actB))))
    in
      fullFold fNode f b tree
*)

(*
  let foldLeavs f b tree =
    let rec fL t actB =
      match t with
      | Leaf actA -> f actA actB
      | Node( t1, t2, t3, t4 ) -> fL t4 (fL t3 (fL t2 (fL t1 actB) ) )
    in
      fL tree.tree b
*)

  let countNodes t =
    let fLeaf = (fun _ x -> x) in
    let fNode = (fun t1 t2 t3 t4 fF fLeaf b -> 1 + ( standardNodeFunction t1 t2 t3 t4 fF fLeaf b )) 
    in
      fullFold fNode fLeaf 0 t

  let countLeavs t =
    foldLeavs (fun _ x -> x + 1) 0 t

  let isSquareInRect size x y x1 y1 x2 y2 =
    let xx = x + size - 1  in
    let yy = y + size - 1
    in
      if x > x2 || xx < x1 || y > y2 || yy < y1 then No
      else if x >= x1 && xx <= x2 && y >= y1 && yy <= y2 then Fully
      else Partial 

  let fillRectangle tree color x1 y1 x2 y2 =
    let rec fillRect s actX actY t =
      assert(s>0);
      let isIn = isSquareInRect s actX actY x1 y1 x2 y2
      in
        match isIn with
        | No -> t
        | Fully -> Leaf color
        | Partial -> 
          (assert(s>=2);
          let newS = s/2 in
          let actXX = actX + newS in
          let actYY = actY + newS in
          let fillRectSize = fillRect newS in
          let fillRect1 = fillRectSize actX actY in
          let fillRect2 = fillRectSize actX actYY in
          let fillRect3 = fillRectSize actXX actY in
          let fillRect4 = fillRectSize actXX actYY in
          let tTmp = match t with
            | Leaf v -> ( fillRect1 (Leaf v), fillRect2 (Leaf v), fillRect3 (Leaf v), fillRect4 (Leaf v)  )
            | Node(t1, t2, t3, t4) -> ( fillRect1 t1, fillRect2 t2, fillRect3 t3, fillRect4 t4)
          in
            glue tTmp)
    in
     assert(x1 <= x2 && y1 <= y2);
     {size = tree.size;
     cornerSW = tree.cornerSW;
     tree = fillRect (firstUpperExp2 tree.size) 0 0 tree.tree}

  let countIncludedFields isIn s x y x1 y1 x2 y2 =
    let countFields a1 b1 a2 b2 = (a2 - a1 + 1) * (b2 - b1 + 1)
    in
    match isIn with
    | No -> 0
    | Fully -> countFields x1 x2 y1 y2
    | Partial ->
      let xx = x + s - 1 in
      let yy = y + s - 1 in
      let xx1 = if x1 > x then x1 else x in
      let xx2 = if x2 < xx then x2 else xx in
      let yy1 = if y1 > y then y1 else y in
      let yy2 = if y2 < yy then y2 else yy
      in
        countFields xx1 xx2 yy1 yy2

  let checkRectangleWithCount tree f b x1 y1 x2 y2 =
    let rec checkRect s actX actY t actB =
      assert( s > 0);
      let isIn = isSquareInRect s actX actY x1 y1 x2 y2
      in
        if isIn = No then actB
        else
          match t with
          | Leaf v -> f v (countIncludedFields isIn s actX actY x1 y1 x2 y2) actB
          | Node( t1, t2, t3, t4) -> 
            (assert(s>=2);
            let newS = s/2 in
            let actXX = actX + newS in
            let actYY = actY + newS in
            let checkRectSize = checkRect newS in
            let checkRect1 = checkRectSize actX actY in
            let checkRect2 = checkRectSize actX actYY in
            let checkRect3 = checkRectSize actXX actY in
            let checkRect4 = checkRectSize actXX actYY
            in
              checkRect1 t1 (checkRect2 t2 (checkRect3 t3 (checkRect4 t4 actB)))
            )
   in
     checkRect (firstUpperExp2 tree.size) 0 0 tree.tree b

  let countColor tree color x1 y1 x2 y2 =
    let f = fun actColor fields b -> if actColor = color then b + fields else b
    in
      checkRectangleWithCount tree f 0 x1 y1 x2 y2

  let checkRectangle tree f b x1 y1 x2 y2 =
    let newF = fun a _ bb -> f a bb
    in
      checkRectangleWithCount tree newF b x1 y1 x2 y2 

  let forAllInRectangle tree boolF x1 y1 x2 y2 =
    let f = fun a b -> b && (boolF a)
    in
    	checkRectangle tree f true x1 y1 x2 y2

  let existInRectangle tree boolF x1 y1 x2 y2 =
    let f = fun a b -> b || (boolF a)
    in
      checkRectangle tree f false x1 y1 x2 y2

  let map f tree =
    let rec iterTmp t =
      match t with
      | Leaf v -> Leaf (f v)
      | Node( t1, t2, t3, t4 ) -> Node( iterTmp t1, iterTmp t2, iterTmp t3, iterTmp t4 )
   in
     {size = tree.size;
     cornerSW = tree.cornerSW;
     tree = iterTmp tree.tree}
  
  let map2Trees t1simplefy t2simplefy t1example t2example joinBy newCornerSW tree1 tree2 =
    let rec m2t t1 t2 =
      match t1 with
      | Leaf x -> (
        match t2 with
        | Leaf y -> Leaf (joinBy x y)
        | Node( n1, n2, n3, n4 ) ->
          if t1simplefy x then Leaf (joinBy x t2example)
          else
            let m2t1 = m2t t1 in
            let newNode = ( m2t1 n1, m2t1 n2, m2t1 n3, m2t1 n4 )
            in 
              glue newNode
        )
      | Node( n1, n2, n3, n4 ) -> (
        match t2 with
        | Leaf y ->
          if t2simplefy y then Leaf (joinBy t1example y)
          else
            let m2t2 xt = m2t xt t2 in 
            let newNode = ( m2t2 n1, m2t2 n2, m2t2 n3, m2t2 n4)
            in
              glue newNode
        | Node( n1_2, n2_2, n3_2, n4_2 ) ->
          let newNode = ( m2t n1 n1_2, m2t n2 n2_2, m2t n3 n3_2, m2t n4 n4_2)
          in
            glue newNode
        )
    in
      {size = if tree1.size > tree2.size then tree1.size else tree2.size;
      cornerSW = newCornerSW;
      tree = m2t tree1.tree tree2.tree}

  let intersect tree1 tree2 newCornerSW =
    let simplefy x = x = false in
    let joinBy x y = x && y
    in
      map2Trees simplefy simplefy false false joinBy newCornerSW tree1 tree2
  
  let sum tree1 tree2 newCornerSW =
    let simplefy x = x = true in
    let joinBy x y = x || y
    in
      map2Trees simplefy simplefy false false joinBy newCornerSW tree1 tree2

  let effect tree permute =
    let rec applyEffect node =
      match node with
      | Node( t1, t2, t3, t4) -> permute applyEffect t1 t2 t3 t4
      | l -> l
    in
      {size = tree.size;
      cornerSW = tree.cornerSW;
      tree = applyEffect tree.tree}

  let flipHorizontal tree =
    effect tree (fun af t1 t2 t3 t4 -> Node(af t2, af t1, af t4, af t3 ))

  let flipVertical tree =
    effect tree (fun af t1 t2 t3 t4 -> Node(af t3, af t4, af t1, af t2)) 

  let flipBackSlash tree =
    effect tree (fun af t1 t2 t3 t4 -> Node(af t4, af t2, af t3, af t1))

  let flipSlash tree =
    effect tree (fun af t1 t2 t3 t4 -> Node(af t1, af t3, af t2, af t4))

  let rotate90 tree =
    effect tree (fun af t1 t2 t3 t4 -> Node(af t3, af t1, af t4, af t2))

  let rotate180 tree =
    effect tree (fun af t1 t2 t3 t4 -> Node(af t4, af t3, af t2, af t1))

  let rotate270 tree =
    effect tree (fun af t1 t2 t3 t4 -> Node(af t2, af t4, af t1, af t3))

  let zoomOut resizePatern tree =
    let rec zoom size node =
      (*assert(size > 1);*)
      if size = 2 then 
        match node with
        | Node( Leaf v1, Leaf v2, Leaf v3, Leaf v4 ) -> Leaf (resizePatern v1 v2 v3 v4)
        | x -> x (* mam pewnosc, ze tu jest Leaf ale moznaby to na wszelki wypadek sprawdzic*)
      else
        match node with
        | Node( t1, t2, t3, t4) ->
          let ns = size / 2 in
          let zs node2 = zoom ns node2 in
          let newNode = ( zs t1, zs t2, zs t3, zs t4 )
          in
            glue newNode
        | Leaf v -> Leaf v
    in
      {size = tree.size / 2;
      cornerSW = tree.cornerSW;
      tree = zoom (firstUpperExp2 tree.size) tree.tree}

  let zoomIn tree =
    {size = tree.size * 2;
    cornerSW = tree.cornerSW;
    tree = tree.tree}
end

