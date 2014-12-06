type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec toLazy = function
	| [] -> LNil
	| x::xs -> LCons(x, function () -> toLazy xs);;

let rec ltake = function
	| (0, _) -> []
	| (_, LNil) -> []
	| (n, LCons(x, xf)) -> x::ltake(n-1, xf());;
	
let rec lfilter pred = function
	| LNil -> LNil
	| LCons(x, xf) -> if pred x then
		LCons(x, function () -> lfilter pred (xf()))
		else lfilter pred (xf());;

let rec (@$) ll1 ll2 =
	match ll1 with
		| LNil -> ll2
		| LCons(x, xf) -> LCons(x, function () -> (xf ()) @$ ll2);;

let rec lmap f = function
	| LNil -> LNil
	| LCons(x, xf) -> LCons(f x, function () -> lmap f (xf()));;

let lsingleton x = LCons(x, function () -> LNil);;

let lEmptyQueue = LNil;;
(* let enqueue_el x lqueue = lqueue @$ [x];; *)
let enqueue_els xs lqueue = lqueue @$ xs;;
let dequeue = function
	| LNil -> failwith "Trying to dequeue from empty queue"
	| LCons(x, qsf) -> (x, qsf());;

let bfsstates next state =
	let rec bfs queue = match queue with
		| [] -> LNil
		| (h::t) -> LCons(h, function () -> bfs (t @ (next h)))
	in bfs [state];;

let isQueenSafe oldqs newq =
	let rec nodiag = function
		| (i, []) -> true
		| (i, q::qt) -> abs(newq - q) <> i && nodiag(i + 1, qt)
	in not(List.mem newq oldqs) && nodiag(1, oldqs);;

let rec fromTo a b = if a>b then [] else a::(fromTo (a+1) b);;

let nextQueen n qs = List.map (fun h -> h::qs)
	(List.filter (isQueenSafe qs) (fromTo 1 n));;

let isSolution n qs = List.length qs = n;;

let solveQueens n =
	let rec solve qs =
		if isSolution n qs then [qs]
		else List.flatten(List.map (fun ust -> solve ust) (nextQueen n qs))
	in solve [];;