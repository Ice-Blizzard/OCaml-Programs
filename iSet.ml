(*
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

This code is based on ISet (Interval sets) and PSet (Polymorphic sets)
by Xavier Leroy, Nicolas Cannase, Marcus Mottl, Jacek Chrzaszcz (only ISet)
I do not earn money with this code, I wrote this for improving my abilities
and for project for University of Warsaw, which also provided me with this code
*)
(* GNU Lesser General Public License *)
(* code by Rafał Szulc Computer Science student at University of Warsaw *)
(* colliding intervals - have common number or numbers with diffrence 1 *)
(* not colliding intervals - are not colliding *)

(* interval [a,b] such that a <= b and includes only integers *)
type interval = int * int

(* 
type t represents AVL tree, where Leaf is empty tree, otherwise in every Node 
we have his left son, then interval he (Node) represents, his right son, 
height of tree and number of integers in our tree
*)
type t =
  | Leaf
  | Node of t * interval * t * int * int

(* the empty set *)
(* val empty : t *)
let empty = Leaf

(* returns true if the set is empty, otherwise returns false *)
(* val is_empty : t -> bool *)
let is_empty t =
  match t with
  | Leaf -> true
  | Node (_, _, _, _, _) -> false

(* 
just like in PSet I create helping function that will give me height of Node or
0 if there is no Node
*)
(* val height : t -> int *)
let height t =
  match t with
  | Leaf -> 0
  | Node (_, _, _, h, _) -> h

(* 
analogical fuction to height but this function will give me number 
of integers in interval of this Node or 0 if there is no Node *)
(* val quantity : t -> int *)
let quantity t =
  match t with
  | Leaf -> 0
  | Node (_, _, _, _, p) -> p

(* 
function that for a,b returns a+b but we are in ring that includes
integers in interval [min_int,max_int]
*)
(* val plus : int -> int -> int *)
let plus a b =
  if a >= 0 && b >= 0
  then
    if max_int - b <= a
    then max_int
    else a + b
  else a + b

(* function that will give me number of integers in interval *)
(* two cases because of problems with -min_int *)
(* val interval_quantity : interval -> int *)
let interval_quantity i =
  if fst i = min_int
  then plus (plus (snd i) (-(fst i + 1))) 2
  else plus (plus (snd i) (-fst i)) 1

(* 
function that will create our avl 
tree with correct numbers in height and quantity similar to make in PSet 
*)
(* val make : t -> interval -> t -> t *)
let make l i r =
  Node (l, i, r, max (height l) (height r) + 1, 
  plus (plus (quantity l) (quantity r)) (interval_quantity i))

(* 
function bal similar to bal in PSet that is needed in order to have 
our tree as avl tree
*)
(* val bal : t -> interval -> t -> t *)
let bal l i r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, li, lr, _, _) ->
        if height ll >= height lr then make ll li (make lr i r)
        else
          (match lr with
          | Node (lrl, lri, lrr, _, _) ->
              make (make ll li lrl) lri (make lrr i r)
          | Leaf -> assert false)
    | Leaf -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, ri, rr, _, _) ->
        if height rr >= height rl then make (make l i rl) ri rr
        else
          (match rl with
          | Node (rll, rli, rlr, _, _) ->
              make (make l i rll) rli (make rlr ri rr)
          | Leaf -> assert false)
    | Leaf -> assert false
  else make l i r

(* 
same as in PSet, Node is slightly diffrent because type t is slightly diffrent
and i represents interval instead of k for easier reading
minimal element 
*)
let rec min_elt = function
  | Node (Leaf, i, _, _, _) -> i
  | Node (l, _, _, _, _) -> min_elt l
  | Leaf -> raise Not_found

(* 
same as in PSet, Node is slightly diffrent because type t is slightly diffrent
and i represents interval instead of k for easier reading
remove minimal element 
*)
let rec remove_min_elt = function
  | Node (Leaf, _, r, _, _) -> r
  | Node (l, i, r, _, _) -> bal (remove_min_elt l) i r
  | Leaf -> invalid_arg "PSet.remove_min_elt"

(*
adding one not colliding element to our tree
val add_one : interval -> t -> t
slightly diffrent version of add_one from PSet
*)
let rec add_one x = function
  | Leaf -> Node (Leaf, x, Leaf, 1, interval_quantity x)
  | Node (l, i, r, h, p) ->
      let c = compare x i in
      if c = 0 then Node (l, x, r, h, p)
      else if c < 0 then
        let nl = add_one x l in
        bal nl i r
      else
        let nr = add_one x r in
        bal l i nr

(*
making avl tree from two avl trees and interval
val join : t-> interval -> t -> t
*)
let rec join l v r =
  match (l, r) with
  | Leaf, _ -> add_one v r
  | _, Leaf -> add_one v l
  | Node (ll, lv, lr, lh, _), Node (rl, rv, rr, rh, _) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

(* 
function that deletes all disjoint intervals from tree or colliding with
(q, max_int)
val discover_min : int -> t -> t * int
*)
let rec discover_min q = function
  | Leaf -> (Leaf, q)
  | Node (l, i, r, _, _) ->
    if q > snd i + 1 && snd i <> max_int then
      let (rr, qq) = discover_min q r
      in (bal l i rr, qq)
    else if q >= fst i then (l, fst i)
    else discover_min q l

(*
analogical function to discover_min but collading with (min_int, q)
val discover_max : int -> t -> t * int
*)
let rec discover_max q = function
  | Leaf -> (Leaf, q)
  | Node (l, i, r, _, _) ->
    if q < fst i - 1 then
      let (ll, qq) = discover_max q l
      in (bal ll i r, qq)
    else if q <= snd i then (r, snd i)
    else discover_max q r

(*
val add : interval -> t -> t
[add (x, y) s] returns a set containing the same elements as [s],
plus all elements of the interval [[x,y]] including [x] and [y].
Assumes [x <= y].
if s is avl tree it returns avl tree
*)
let rec add i = function
  | Leaf -> add_one i Leaf
  | Node (l, ii, r, _, _) ->
    if fst ii <> min_int && snd i < fst ii - 1 
    then bal (add i l) ii r
    else
      if snd ii <> max_int && fst i > snd ii + 1
      then bal l ii (add i r)
      else
        let (ll, q1) = discover_min (min (fst i) (fst ii)) l
        and (rr, q2) = discover_max (max (snd i) (snd ii)) r in
        join ll (q1, q2) rr

(*
functions remove and split will be implementationed together
val remove : interval -> t -> t
    [remove (x, y) s] returns a set containing the same elements as [s],
    except for all those which are included between [x] and [y].
    Assumes [x <= y].
val split : int -> t -> t * bool * t
    [split x s] returns a triple [(l, present, r)], where
    [l] is the set of elements of [s] that are strictly lesser than [x];
    [r] is the set of elements of [s] that are strictly greater than [x];
    [present] is [false] if [s] contains no element equal to [x],
    or [true] if [s] contains an element equal to [x].
I also use loop from PSet
if t is avl tree it returns avl tree
*)
let rec remove i t =
  match t with
  | Leaf -> Leaf
  | _ ->
    let (l, _, r) = split (fst i) t in
    let (_, _, rr) = split (snd i) r in
    match rr with
    | Leaf -> l
    | Node (_, _, _, _, _) ->      
      join l (min_elt rr) (remove_min_elt rr)

and split i t =
  let rec loop i = function
    | Leaf ->
        (Leaf, false, Leaf)
    | Node (l, ii, r, _, _) ->
        if i < fst ii 
        then let (ll, pres, rr) = loop i l in (ll, pres, join rr ii r)
        else
          if i > snd ii
          then let (lr, pres, rr) = loop i r in (join l ii lr, pres, rr)
          else
            let w1 =
              if fst ii = i 
              then l
              else add (fst ii, i - 1) l
            and w2 =
              if snd ii = i 
              then r
              else add (i + 1, snd ii) r
            in (w1, true, w2)
  in let l, pres, r = loop i t in l, pres, r

(*
val mem : int -> t -> bool
[mem x s] returns [true] if [s] contains [x], and [false] otherwise.
*)
let mem q t =
  let (l, pres, r) = split q t in pres

(*
val elements : t -> interval list
    Return the list of all continuous intervals of the given set.
    The returned list is sorted in increasing order.
*)
let elements set = 
  let rec loop acc = function
    | Leaf -> acc
    | Node (l, i, r, _, _) -> loop (i :: loop acc r) l in
  loop [] set

(*
val below : int -> t -> int
    [below n s] returns the number of elements of [s] that are lesser
    or equal to [n]. If there are more than max_int such elements, 
    the result should be max_int.
*)
let below q t =
  let rec below_helper q t acc =
    match t with
    | Leaf -> acc
    | Node (l, i, r, _, _) ->
      if q > snd i 
      then below_helper q r (plus (plus acc (quantity l)) (interval_quantity i))
      else
        if q >= fst i
        then plus (plus acc (quantity l)) (interval_quantity (fst i, q))
        else below_helper q l acc
      in below_helper q t 0

(*
val fold : (int * int -> 'a -> 'a) -> t -> 'a -> 'a
    [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)], where x1
    ... xN are all continuous intervals of s, in increasing order.
*)
let fold f t acc =
  let rec loop acc = function
    | Leaf -> acc
    | Node (l, i, r, _, _) -> loop (f i (loop acc l)) r
  in loop acc t

(*
val iter : (interval -> unit) -> t -> unit
    [iter f s] applies [f] to all continuous intervals in the set [s].
    The intervals are passed to [f] in increasing order.
Similar to iter from PSet
*)
let iter f =
  let rec loop = function
    | Leaf -> ()
    | Node (l, i, r, _, _) -> loop l; f i; loop r 
  in loop;;


