type 'a mtree = MNode of 'a * 'a forest 
	and 'a forest = EmptyForest | Forest of 'a mtree * 'a forest;;

type 'a mtree_lst = MTree of 'a * ('a mtree_lst) list;;

let exmtreer = MNode(1, Forest(MNode(2, EmptyForest), Forest(MNode(3, Forest(MNode(4, EmptyForest), EmptyForest)), Forest(MNode(5, EmptyForest), EmptyForest))));;
(*        1
       /  |  \
      2   3   5
          |
          4 
*)
let exmtreel = MTree(1, [MTree(2, []); MTree(3, [MTree(4, [])]); MTree(5, [])]);;

let children_mtr (MNode(_, forest)) = 
	let rec chldr forest acc = match forest with
		| EmptyForest -> List.rev acc
		| Forest(mtree, nxtforest) -> chldr nxtforest (mtree::acc)
	in (chldr forest []);;

let emptyQueue = []
let enqueue_el x queue = queue @ [x];;
let enqueue_els xs queue = queue @ xs;;
let dequeue = function
	| [] -> failwith "Trying to dequeue from empty queue"
	| x::qs -> (x, qs);;

let emptyStack = []
let pushOnStack_el x stack = x::stack;;
let pushOnStack_els xs stack = xs @ stack;;
let popFromStack = function
	| [] -> failwith "Trying to pop from empty stack"
	| x::ss -> (x, ss);;

let bfs_mtr mtree =
	let rec bfs queue acc = try 
		let (MNode(n, _) as mtree, nqueue) = dequeue queue 
			in let chldrn = children_mtr mtree
				in bfs (enqueue_els chldrn nqueue) (n::acc)
		with
			| _ -> List.rev acc
	in bfs (enqueue_el mtree emptyQueue) [];;

let dfs_mtr mtree =
	let rec dfs stack acc = try
		let (MNode(n, _) as mtree, nstack) = popFromStack stack
			in let chldrn = children_mtr mtree
				in dfs (pushOnStack_els chldrn nstack) (n::acc)
		with
			| _ -> List.rev acc
	in dfs (pushOnStack_el mtree emptyStack) [];;

let children_mtl (MTree(_, chldrn)) = chldrn;;

let bfs_mtl mtree =
	let rec bfs queue acc = try 
		let (MTree(n, _) as mtree, nqueue) = dequeue queue 
			in let chldrn = children_mtl mtree
				in bfs (enqueue_els chldrn nqueue) (n::acc)
		with
			| _ -> List.rev acc
	in bfs (enqueue_el mtree emptyQueue) [];;

let dfs_mtl mtree =
	let rec dfs stack acc = try
		let (MTree(n, _) as mtree, nstack) = popFromStack stack
			in let chldrn = children_mtl mtree
				in dfs (pushOnStack_els chldrn nstack) (n::acc)
		with
			| _ -> List.rev acc
	in dfs (pushOnStack_el mtree emptyStack) [];;

