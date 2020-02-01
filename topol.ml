(*
 * PMap - Polymorphic maps
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
 *)

(* Polymorphic Map.

	This is a polymorphic map, similar to standard library [Map] module
	but in a defunctorized style.
*)
(* I use PMat which I got from University of Warsaw and use it for project *)
(* for University of Warsaw. I do not earn money from this program. *)
(* Code by Rafał Szulc University of Warsaw *)

(* Exception for cases where our graph has at least one cycle because *)
(* for such graphs you cannot do topological sorting. *)
exception Cykliczne

(* Some useful helping functions. *)
(* Function returns graph with all vertices, but without edges. *)
(* Creates PMap map with vertices. *)
(* val graph_vertices : 'a list -> ('a, 'b list) PMap.t = <fun> *)
let graph_vertices vertices =
  List.fold_left
    (fun graph vertex -> PMap.add vertex [] graph)
    PMap.empty
    vertices

(* Adds neighbors to graph with vertices. *)
(* val add_neighbors :
('a * 'b list) list -> ('a, 'b list) PMap.t -> ('a, 'b list) PMap.t = <fun> *)
let add_neighbors neighbors graph =
  List.fold_left
    (fun graph (vertex, edges) ->
      let other_edges = try PMap.find vertex graph with Not_found -> [] in
      PMap.add vertex (edges @ other_edges) graph)
    graph
    neighbors

(* Creates list of vertices. *)
(* val get_vertices : ('a * 'b) list -> 'a list = <fun> *)
let get_vertices neighbors =
  List.map (fun (vertex, _) -> vertex) neighbors

(* Creates pmat map from vertices and neighbors where: *)
(* neighbors - list of (vertex * list of his neighbors) *)
(* val create_graph : ('a * 'b list) list -> ('a, 'b list) PMap.t = <fun> *)
let create_graph neighbors =
  let vertices = get_vertices neighbors in
  let empty_graph = graph_vertices vertices in
  let graph = add_neighbors neighbors empty_graph in
  graph

(* (topological) dfs algorithm *)
(* exception Cykliczne is raised if graph has cycle(s) *)
(* graph is a pmap adjacency list *)
(* post_order is topological sorted list *)
(* finished is pmap of elements from post_order *)
(* visited is pmap of visited vertices *)
(* vertex is starting point of dfs *)
(* val topol_dfs :
('a, 'a list) PMap.t ->
'a list * ('a, int) PMap.t * ('a, int) PMap.t ->
'a -> 'a list * ('a, int) PMap.t * ('a, int) PMap.t = <fun> *)
let rec topol_dfs graph (post_order, finished, visited) vertex =
  if PMap.mem vertex visited
  then
    if PMap.mem vertex finished
    then (post_order, finished, visited)
    else raise Cykliczne
  else
    let (post_order, finished, visited) =
      List.fold_left
        (topol_dfs graph) 
        (post_order, finished, (PMap.add vertex 0 visited))
        (try PMap.find vertex graph with Not_found -> []) in
    (vertex::post_order, PMap.add vertex 0 finished, visited)

(*
For given list [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])]
returns a list, where every element a_i and a_ij appears only once,
ordered such that every element a_i is before every element from set
{a_i1, ... , a_il}
*)
(* val topol : ('a * 'a list) list -> 'a list = <fun> *)
let topol neighbors =
  let graph = create_graph neighbors in
  let (post_order, _, _) =
    List.fold_left 
      (fun acc (vertex, _) -> topol_dfs graph acc vertex) 
      ([], PMap.empty, PMap.empty)
      neighbors in
  post_order

;;

