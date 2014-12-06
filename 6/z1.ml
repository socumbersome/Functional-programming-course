type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree;;
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;
type 'a stream = SNil | SCons of 'a * 'a stream Lazy.t

let rec ltake = function
	| (0, _) -> []
	| (_, LNil) -> []
	| (n, LCons(x, xf)) -> x::ltake(n-1, xf());;

let lsingleton a = LCons(a, function () -> LNil);;

let smfringe tree1 tree2 =
	let rec getfringe tree = match tree with
		| Leaf a -> [a]
		| Node(l, r) -> (getfringe l) @ (getfringe r)
	in (getfringe tree1) = (getfringe tree2);;

let t1 = Node (Node (Leaf 1, Leaf 2), Leaf 3);;
let t2 = Node (Leaf 1, Node (Leaf 2, Leaf 3));;
let t3 = Node (Leaf 1, Node (Leaf 3, Leaf 2));;

let rec (@$) ll1 ll2 =
	match ll1 with
		| LNil -> ll2
		| LCons(x, xf) -> LCons(x, function () -> (xf ()) @$ ll2);;

let samefringe tree1 tree2 =
	let rec mkstream tree = match tree with
		| Leaf a -> LCons(a, function () -> LNil)
		| Node(l, r) -> (mkstream l) @$ (mkstream r)
	in let rec smf t1 t2 = match (t1, t2) with
		| (LCons(l1, t1), LCons(l2, t2)) -> 
			if l1 = l2 then smf (t1()) (t2())
			else false
		| (LNil, LNil) -> true
		| _ -> false
	in smf (mkstream tree1) (mkstream tree2);;
	
let samefringe2 tree1 tree2 =
	let rec mkstream tree cont = match tree with
		| Leaf a -> cont (lsingleton a)
		| Node(l, r) -> 
			mkstream l (
				fun lleaves -> mkstream r (
					fun rleaves -> cont (lleaves @$ rleaves)
				)
			)
	in let rec smf t1 t2 = match (t1, t2) with
		| (LCons(l1, t1), LCons(l2, t2)) -> 
			if l1 = l2 then smf (t1()) (t2())
			else false
		| (LNil, LNil) -> true
		| _ -> false
	in smf (mkstream tree1 (fun x -> x)) (mkstream tree2 (fun x -> x));;

let samefringe3 tree1 tree2 =
	let rec mkstream tree tail_cont = match tree with
		| Leaf a -> SCons(a, tail_cont)
		| Node(l, r) -> mkstream l (lazy (mkstream r tail_cont))
	in let rec smf t1 t2 = match (t1, t2) with
		| (SCons(l1, t1), SCons(l2, t2)) -> 
			if l1 = l2 then smf (Lazy.force t1) (Lazy.force t2)
			else false
		| (SNil, SNil) -> true
		| _ -> false
	in smf (mkstream tree1 (lazy SNil)) (mkstream tree2 (lazy SNil));;
