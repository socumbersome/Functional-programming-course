let rec rmap f l = match l with
	| [] -> []
	| x::xs -> (rmap f xs) @ [f x];;

let rmapog f l =
	let rec rmapwacc f ll acc = match ll with
		| [] -> acc
		| x::xs -> rmapwacc f xs ((f x)::acc)
	in rmapwacc f l [];;
