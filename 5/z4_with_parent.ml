
(* first argument denotes parent *)
type 'a huffTree = HNil | HLeaf of 'a huffTree * 'a * float | 
	HNode of 'a huffTree * 'a huffTree * float * 'a huffTree;;

let symbol2freq = [('A', 0.1); ('B', 0.2); ('C', 0.3); ('D', 0.4)];;

let getFreq = function
	| HNil -> failwith "Nil tree doesn't have frequency in it"
	| HLeaf(_, _, f) -> f
	| HNode(_, _, f, _) -> f;;

let float2signum f = let eps = 1.0e-10 in
	if f > eps then 1 else if f < eps then -1 else 0;;

let changeParent newp hftree =
	| HNil -> failwith "Nil doesn't have a parent"
	| HLeaf(_, s, f) -> HLeaf(newp, s, f)
	| HNode(_, lt, f, rt) -> HNode(newp, lt, f, rt);;

let mkHuffEncTree s2f =
	let rec he trees = match trees with
		| [] -> failwith "trees have disappeared!"
		| [t] -> t
		| _ -> let t1::t2::ts = 
			List.sort (fun htl htr -> float2signum((getFreq htl) -. (getFreq htr))) trees
			in let tempNew = 
			in he (
				HNode(
					HNil,
					t1,
					(getFreq t1) +. (getFreq t2),
					t2
					)::ts
				)
	in he (List.map (fun (s, f) -> HLeaf(HNil, s, f)) s2f);;

let huffEncode hetree symbols =
	let encode symbol = 
	
	in List.map (fun s -> encode s) symbols;;