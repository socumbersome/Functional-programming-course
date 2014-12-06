type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree

let prod tree =
	let rec prod_cps tree cont = match tree with
		| Leaf -> cont 1
		| Node(left, n, right) -> 
			prod_cps left 
			(fun pl -> prod_cps right 
				(fun pr -> cont (pl * n * pr)))
	in let rec prod_cpsb tree cont = match tree with
		| Leaf -> cont 1
		| Node(left, n, right) -> 
			if n = 0 then 0 else
			prod_cps left 
			(fun pl -> prod_cps right 
				(fun pr -> cont (pl * n * pr)))
	in prod_cpsb tree (fun x -> x);;



let fib n =
	let rec fib_cps n cont = match n with
		| 0 -> cont 1
		| 1 -> cont 1
		| _ -> fib_cps (n - 2) (fun fn2 -> fib_cps (n - 1) (fun fn1 -> cont (fn2 + fn1)))
	in fib_cps n (fun x -> x);;