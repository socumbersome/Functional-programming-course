type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;;

(* int denotes the highest available index of an array *)
type 'a arrayf = ArrayF of 'a btree * int;;

let aempty = ArrayF(Leaf, 0);;

let asub (ArrayF(tree, m)) n = 
	if n < 1 || n > m then failwith "index out of bounds"
	else
	let rec getelem k (Node(l, a, r)) =
		if k = 1 then a
		else let khalf = k / 2 in
		if k mod 2 = 0 then getelem khalf l
		else getelem khalf r
	in getelem n tree;;

let aupdate (ArrayF(tree, m)) k a =
	if k < 1 || k > m then failwith "index out of bounds"
	else
	let rec upd (Node(l, x, r)) k =
		if k = 1 then Node(l, a, r)
		else let khalf = k / 2 in
		if k mod 2 = 0 then Node(upd l khalf, x, r)
		else Node(l, x, upd r khalf)
	in ArrayF(upd tree k, m);;

let ahiext (ArrayF(tree, m)) a =
	if m = 0 then ArrayF(Node(Leaf, a, Leaf), 1)
	else
	let rec put tree k =
		if k = 1 then Node(Leaf, a, Leaf)
		else let khalf = k / 2
		and Node(l, x, r) = tree in
		if k mod 2 = 0 then Node(put l khalf, x, r)
		else Node(l, x, put r khalf)
	in ArrayF(put tree (m+1), m+1);;

let ahirem (ArrayF(tree, m)) =
	let rec rm tree k =
		if k = 1 then Leaf
		else let khalf = k / 2
		and Node(l, x, r) = tree in
		if k mod 2 = 0 then Node(rm l khalf, x, r)
		else Node(l, x, rm r khalf)
	in ArrayF(rm tree m, m-1);;

let t0 = aempty;;
let t1 = ahiext t0 1;;
let t2 = ahiext t1 2;;
let t3 = ahiext t2 3;;
let t4 = ahiext t3 4;;
let t5 = ahiext t4 5;;