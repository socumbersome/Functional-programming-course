type 'a btree = Leaf of 'a | Node of 'a btree * 'a * 'a btree;;

let t0 = Node( Leaf 'a', 'b', Leaf 'c');;
let t1 = Node(Node(Leaf 'a', 'b', Leaf 'c'), 'd', Leaf 'e');;
let t2 = Node(Node(Leaf 'a', 'b', Leaf 'c'), 'd', Node(Leaf 'e', 'f', Leaf 'g'));;

let preorderLabeling tree =
	let rec plcps tree n cont = match tree with
		| Leaf a -> cont ((Leaf n), n)
		| Node(l, a, r) -> 
			plcps l (n+1) (
				fun (llabeled, lnb) -> plcps r (lnb+1) (
					fun (rlabeled, rnb) -> 
						cont (Node(llabeled, n, rlabeled), rnb)
				)
			)
	in let (ltree, _) = plcps tree 1 (fun x -> x) in ltree;;

let t0l = preorderLabeling t0;;
let t1l = preorderLabeling t1;;

let rec pr tree = match tree with
	| Leaf a -> [a]
	| Node(l, a, r) -> (pr l) @ [a] @ (pr r);;

type 'a stream = SNil | SCons of 'a * 'a stream Lazy.t;;

let bfslabeling tree =
	let rec glue fromn trees enumChildren = match trees with
		| (Leaf a)::ts -> (Leaf fromn)::(glue (fromn+1) ts enumChildren)
		| (Node(l, a, r))::ts -> let cl::cr::cs = enumChildren
			in (Node(cl, fromn, cr))::(glue (fromn+1) ts cs)
		| _ -> []
	in
	let rec enumerate fromn trees = 
		if trees = [] then [] 
		else
			let treesLen = List.length trees 
			in let nonleaves = List.filter 
				(fun t -> match t with 
					| Leaf _ -> false
					| _ -> true
				) trees
			in let children = List.flatten (
				List.map (fun (Node(l, _, r)) -> [l; r]) nonleaves
				)
			in let enumChildren = enumerate (fromn + treesLen) children
			in glue fromn trees enumChildren
	in let [t] = enumerate 1 [tree] in t;;

let last_two_and_before xs =
	let rec aux prefr suf = match suf with
		| [x; y] -> (List.rev prefr, x, y)
		| x::xs -> aux (x::prefr) xs
	in aux [] xs;;

let bfslabeling_cps tree =
	let rec enumberate tree nb cont = match tree with (* nb = number in bfs order *)
		| Leaf a -> (Leaf nb)::(next cont (nb+1))
		| Node(l, a, r) -> 
			let (rest, l', r') = last_two_and_before (next (cont @ [l; r]) (nb+1))
			in (Node(l', nb, r'))::rest
	and next cont nb = match cont with
		| [] -> []
		| t::ct -> enumberate t nb ct
	in let [t] = enumberate tree 1 [] in t;;