let (+:) x xs = x :: xs;;

let rec map f xs = match xs with
	| [] -> []
	| y::ys -> (f y)::(map f ys);;

let rec sublists xs = 
	let rec prefixes zs = match zs with
		| [] -> []
		| t::ts -> [t] :: (map ((+:) t) (prefixes ts))
	in match xs with
	| [] -> []
	| y::ys -> (prefixes xs) @ sublists ys;;
