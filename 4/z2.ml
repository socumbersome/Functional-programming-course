type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;;

let is_balanced tree =
	let rec isbalaux tree = match tree with
		| Leaf -> (true, 0)
		| Node (left, _, right) ->
			let (odpl, sizel) = isbalaux left
			and (odpr, sizer) = isbalaux right
			in (odpl && odpr && (abs (sizel - sizer) <= 1), sizel + sizer + 1)
	in let (odp, _) = isbalaux tree in odp;;

let treeunbal = Node( Node(Node(Leaf, 3, Leaf) , 4, Node(Leaf, 2, Leaf)) , 5, Node(Leaf, 1, Leaf));;
let unbal = Node(Leaf, 1, Node(Leaf, 2, Node(Leaf, 3, Node(Leaf, 4, Leaf))));;
let treebal = Node( Node(Node(Leaf, 3, Leaf) , 4, Node(Leaf, 2, Leaf)) , 5, Node(Node(Leaf, 7, Leaf), 1, Leaf));;

let split xs = 
	let rec spl leftr right licz = match licz with
		| [] | [_] -> (List.rev leftr, right)
		| _::_::cs -> spl ((List.hd right)::leftr) (List.tl right) cs
	in spl [] xs xs;;

let rec mktree xs = match xs with
	| [] -> Leaf
	| y::ys -> let (lh, rh) = split ys in
		Node(mktree lh, y, mktree rh);;

let rec preorder tree = match tree with
	| Leaf -> []
	| Node(left, n, right) -> (n::(preorder left)) @ (preorder right);;
