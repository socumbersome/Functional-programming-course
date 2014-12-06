let rec merge cmp xs ys =
	let rec insert x xs = match xs with
		| [] -> [x]
		| y::ys -> if cmp x y then x::xs else y::(insert x ys)
	in match xs with
	| [] -> ys
	| z::zs -> merge cmp zs (insert z ys);;

let reverse xs = 
	let rec reversewacc xs acc = match xs with
		| [] -> acc
		| y::ys -> reversewacc ys (y::acc)
	in reversewacc xs [];;

let rec mergeog cmp xs ys = 
	let rec insert x left right = match right with
		| [] -> reverse (x::left)
		| y::ys -> 
			if cmp x y then reverse (x::left) @ right 
			else insert x (y::left) ys
	in match xs with
	| [] -> ys
	| z::zs -> mergeog cmp zs (insert z [] ys);;

let mergesort ?(cmp = (<=)) xs = 
	let
	split xs = let
		rec splitacc xs lh rh = match xs with
			| [] -> (lh, rh)
			| y::ys -> splitacc ys rh (y::lh)
		in splitacc xs [] []
	and
	merge = mergeog cmp
	in let
	rec sort xs = match xs with
		| [] -> []
		| [x] as s -> s
		| ys -> let (l, r) = split ys in merge (sort l) (sort r)
	in sort xs;;

let time comp = let t = Sys.time() in comp(); Sys.time() -. t;;

let mklist n =
	let rec mklistaux n ost acc = match n with
		| 0 -> List.rev acc
		| m -> let i = (Random.int 6) + ost in mklistaux (n-1) i (i::acc)
	in mklistaux n 0 [];;

let timem = let l1 = mklist 3000 and l2 = mklist 3000 in
	(
	time (fun () -> (merge (<=) l1 l2)),
	time (fun () -> (mergeog (<=) l1 l2))
	);;

