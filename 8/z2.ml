
let swap f = fun a b -> f b a

module type VERTEX =
sig
	type t
	type label
	
	val equal: t -> t -> bool
	val create: label -> t
	val label: t -> label
end;;

(* directed edge *)
module type EDGE =
sig
	type t
	type label
	type v
	
	val equal: t -> t -> bool
	val create: v -> v -> t
	val label: t -> label
	val vstart: t -> v
	val vend: t -> v
end;;

module Vertex : VERTEX with type label = string =
struct
	type label = string
	type t = Vert of label
	
	let equal (Vert a) (Vert b) = a = b
	let create l = Vert l
	let label (Vert l) = l
end;;

(*module EdgeFunctor (Vert:VERTEX) : EDGE with type label = (Vert.label * Vert.label) and type v = Vert.t =
struct
	type v = Vert.t
	type label = Vert.label * Vert.label
	type t = Edg of v * v * label
	
	let equal (Edg(_, _, l1)) (Edg(_, _, l2)) = l1 = l2
	let create v1 v2 = Edg(v1, v2, (Vert.label v1, Vert.label v2))
	let label (Edg(_, _, l)) = l
	let vstart (Edg(v, _, _)) = v
	let vend (Edg(_, w, _)) = w
end;;

module Edge = EdgeFunctor (Vertex);;*)
module Edge : EDGE with type label = (Vertex.label * Vertex.label)
	and type v = Vertex.t =
struct
	type v = Vertex.t
	type label = Vertex.label * Vertex.label
	type t = Edg of v * v * label
	
	let equal (Edg(_, _, l1)) (Edg(_, _, l2)) = l1 = l2
	let create v1 v2 = Edg(v1, v2, (Vertex.label v1, Vertex.label v2))
	let label (Edg(_, _, l)) = l
	let vstart (Edg(v, _, _)) = v
	let vend (Edg(_, w, _)) = w
end;;

module type GRAPH =
sig
	type t
	module V : VERTEX with type label = string
	type vertex = V.t
	
	module E : EDGE with type v = vertex and type label = (string * string)
	type edge = E.t
	
	(* funkcje wyszukiwania *)
	val mem_v : t -> vertex -> bool
	val mem_e : t -> edge -> bool
	val mem_e_v : t -> vertex -> vertex -> bool
	val find_e : t -> vertex -> vertex -> edge
	val succ : t -> vertex -> vertex list
	val pred : t -> vertex -> vertex list
	val succ_e : t -> vertex -> edge list
	val pred_e : t -> vertex -> edge list

	(* funkcje modyfikacji *) 
	val empty : t
	val add_e : t -> edge -> t
	val add_v : t -> vertex -> t
	val rem_e : t -> edge -> t
	val rem_v : t -> vertex -> t

	(* iteratory *)
	val fold_v : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
	val fold_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
	
	(* info o grafie *)
	val all_vertices : t -> vertex list
	val all_edges : t -> edge list
end;;

(* conventions: it's a directed graph without multiple edges
 (i.e. not a multigraph) that can have loops (i.e. edges from v to v)
*)
module GraphFunctor (Vert:VERTEX with type label = string) (Edg:EDGE with type v = Vert.t and type label = (string * string)) : GRAPH =
struct
	module V = Vert
	type vertex = V.t
	module E = Edg
	type edge = E.t
	(* edge list corresponds to succesors *)
	type t = G of (vertex * edge list) list
	
	(* don't know how to name it properly; an intuition is that
	"mapping" will usually be List.find or List.exists *)
	let aim_for_v (G ves) v mapping = mapping (fun (w, _) -> V.equal v w) ves

	(* raises Not_found if v is not contained in a graph *)
	let succ_e g v = snd (aim_for_v g v List.find)

	let succ g v = List.map E.vend (succ_e g v)
	
	let mem_v g v = aim_for_v g v List.exists

	let mem_e g e = try let es = succ_e g (E.vstart e)
		in List.exists (fun e1 -> E.equal e e1) es
		with Not_found -> false

	let mem_e_v g v w = mem_e g (E.create v w)

	let find_e g v w = let es = succ_e g v
		in List.find (fun e1 -> V.equal w (E.vend e1)) es
	
	let pred_e (G ves) v = List.filter (fun e -> V.equal (E.vend e) v)
		(List.flatten (List.map snd ves))

	let pred g v = List.map E.vstart (pred_e g v)
	
	let empty = G []

	(* won't change a graph if v is already contained in it *)
	let add_v g v = if aim_for_v g v List.exists then g
		else let G ves = g in G ((v, [])::ves)
	
	let find_and_split p xs =
		let rec aux leftr right = match right with
			| [] -> raise Not_found
			| y::ys -> if p y then (List.rev leftr, y, ys)
				else aux (y::leftr) ys
		in aux [] xs
	(* - endpoints of e must exist already 
	   - won't change a graph if e is already contained in it
		 (and won't raise an exception, but it can be changed -
		 just a matter of convention),
	     i.e. our graph is not a multigraph
	*)
	let add_e (G ves) e =
		let inse e es = 
			if List.exists (fun f -> E.equal f e) es then es
			else e::es
		in try 
		let (l, (v, es), r) = find_and_split
			(fun (w, _) -> V.equal (E.vstart e) w) ves
		in G ((v, inse e es)::(l @ r))
		with Not_found -> G ((E.vstart e, [e])::ves)
	
	let rem_e (G ves) e =
		let (l, (v, es), r) = find_and_split
			(fun (w, _) -> V.equal (E.vstart e) w) ves
		in G ((v, List.filter (fun e1 -> not (E.equal e e1)) es)::(l @ r))
	
	(* fold_v: (vertex -> 'a -> 'a) -> t -> 'a -> 'a *)
	let fold_v f (G ves) acc = List.fold_left (swap f) acc (List.map fst ves)

	(* fold_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a *)
	let fold_e f (G ves) acc = 
		List.fold_left (swap f) acc (List.flatten (List.map snd ves))
	
	let rem_v ((G ves) as g) v = 
		let remd_pred = List.fold_left
		(fun acc (w, es) -> 
			(w, 
			List.filter (fun e -> 
				not (V.equal v (E.vend e))) es
			)::acc
		) [] ves
		in G (List.filter (fun (w, _) -> not (V.equal w v)) remd_pred)
	
	let all_vertices (G ves) = List.map fst ves
	let all_edges (G ves) = List.flatten (List.map snd ves)
end;;

module Graph = GraphFunctor (Vertex) (Edge);;

module type CONTAINER =
sig
	type 'a t
	exception EmptyContainer
	
	val empty: 'a t
	val push: 'a -> 'a t -> 'a t
	(* multipush [x1; x2; ...; xn] c should be equivalent to
	List.fold_left (swap push) c [x1; x2; ...; xn] *)
	val multipush: 'a list -> 'a t -> 'a t
	val pop: 'a t -> 'a t
	val peek: 'a t -> 'a
	val is_empty: 'a t -> bool
end;;

module Stack : CONTAINER =
struct
	type 'a t = 'a list
	exception EmptyStack
	exception EmptyContainer = EmptyStack
	
	let empty = []
	let push a st = a::st
	let multipush xs s = List.fold_left (swap push) s xs
	let pop st = match st with
		| [] -> raise EmptyStack
		| _::rest -> rest
	let peek st = match st with
		| [] -> raise EmptyStack
		| a::_ -> a
	let is_empty st = st = []
end;;

module Queue : CONTAINER =
struct
	type 'a t = 'a list * 'a list
	exception EmptyQueue
	exception EmptyContainer = EmptyQueue
	
	let empty = ([], [])
	let is_empty (ls, rs) = ls = [] && rs = []
	let push a (ls, rs) = (a::ls, rs)
	let multipush xs s = List.fold_left (swap push) s xs
	let rec peekopop (ls, rs) = match rs with
		| [] -> if ls = [] then raise EmptyQueue
			else peekopop ([], List.rev ls)
		| r::rest -> (r, (ls, rest))
	let pop q = snd (peekopop q)
	let peek q = fst (peekopop q)
end;;

(* start must be a vertex belonging to graph *)
let dfs graph start =
	let rec dfsaux stack acc =
		try 
			let v = Stack.peek stack
			in let nstack = Stack.pop stack
			in let chldrn = Graph.succ graph v
			in let nonvischldrn = List.filter 
				(fun x -> not (List.mem x acc)) chldrn
			in let nacc = if List.mem v acc then acc else v::acc
			in dfsaux (Stack.multipush nonvischldrn nstack) nacc
		with Stack.EmptyContainer -> List.rev_map (fun v -> Graph.V.label v) acc
	in dfsaux Stack.(push start empty) [];;

(* start must be a vertex belonging to graph *)
let bfs graph start =
	let rec bfsaux queue acc =
		try 
			let v = Queue.peek queue
			in let nqueue = Queue.pop queue
			in let chldrn = Graph.succ graph v
			in let nonvischldrn = List.filter 
				(fun x -> not (List.mem x acc)) chldrn
			in let nacc = if List.mem v acc then acc else v::acc
			in bfsaux (Queue.multipush nonvischldrn nqueue) nacc
		with Queue.EmptyContainer -> List.rev_map (fun v -> Graph.V.label v) acc
	in bfsaux Queue.(push start empty) [];;


(** testing... **)

let printv g = List.map Graph.V.label (Graph.all_vertices g);;
let printe g = List.map Graph.E.label (Graph.all_edges g);;
let is_perm xs ys = List.for_all ((swap List.mem) ys) xs && 
	List.length xs = List.length ys &&
	List.for_all ((swap List.mem) xs) ys
open Graph;;

let emp = empty;;
let v1 = V.create "1";;
let v2 = V.create "2";;
let v3 = V.create "3";;
let e12 = E.create v1 v2;;
(* 2 cycle i.e. (1,2), (2,1) *)
let cycle2_g =
	let a = ref (add_v emp v1)
	in a := add_v !a v2;
	add_e !a e12;;
assert (is_perm (printv cycle2_g) ["1"; "2"]);;
assert (is_perm (printe cycle2_g) [("1", "2")]);;
let rm1 = rem_v cycle2_g v1;;
assert (printv rm1 = ["2"]);;
assert (printe rm1 = []);;

(* now path with a loop, i.e. with edges (3,1), (1,1), (1,2) *)
let e11 = E.create v1 v1;;
let e31 = E.create v3 v1;;
let loop_g =
	let a = ref (add_v emp v1)
	in a := add_v !a v2;
	a := add_v !a v3;
	a := add_e !a e11;
	a := add_e !a e12;
	add_e !a e31;;
assert (is_perm (printv loop_g) ["1"; "2"; "3"]);;
assert (is_perm (printe loop_g) [("3", "1"); ("1", "2"); ("1", "1")]);;

let rmloop = rem_e loop_g e11;;
assert (is_perm (printv rmloop) ["1"; "2"; "3"]);;
assert (is_perm (printe rmloop) [("3", "1"); ("1", "2")]);;

let rmprede = rem_e loop_g e31;;
assert (is_perm (printv rmprede) ["1"; "2"; "3"]);;
assert (is_perm (printe rmprede) [("1", "1"); ("1", "2")]);;

let rmsucce = rem_e loop_g e12;;
assert (is_perm (printv rmsucce) ["1"; "2"; "3"]);;
assert (is_perm (printe rmsucce) [("1", "1"); ("3", "1")]);;

let rmpredv = rem_v loop_g v3;;
assert (is_perm (printv rmpredv) ["1"; "2"]);;
assert (is_perm (printe rmpredv) [("1", "1"); ("1", "2")]);;

let rmloopv = rem_v loop_g v1;;
assert (is_perm (printv rmloopv) ["3"; "2"]);;
assert (is_perm (printe rmloopv) []);;

let v4 = V.create "4";;
let v5 = V.create "5";;
let e13 = E.create v1 v3;; let e24 = E.create v2 v4;; let e25 = E.create v2 v5;; let e35 = E.create v3 v5;; let e41 = E.create v4 v1;; let e55 = E.create v5 v5
let gr =
	let a = ref (add_v emp v1)
	in a := add_v !a v2;
	a := add_v !a v3; a := add_v !a v4; a:= add_v !a v5;
	a := add_e !a e12;
	a := add_e !a e13; a := add_e !a e24; a := add_e !a e25;
	a := add_e !a e35; a:= add_e !a e55; add_e !a e41;;

let bfsgr = bfs gr v1;;
let dfsgr = dfs gr v1;;
