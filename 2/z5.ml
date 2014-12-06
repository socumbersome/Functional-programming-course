let insEverywh x xs = 
	let rec insAcc left right pres = match right with
		| [] -> (List.rev (x::left))::pres
		| y::ys -> insAcc (y::left) ys ((List.rev (x::left) @ right)::pres)
	in insAcc [] xs [];;

let flatten xxs =
	let rec flattenacc xxs acc = match xxs with
		| [] -> List.rev acc
		| ys::yys -> flattenacc yys ((List.rev ys) @ acc)
	in flattenacc xxs [];;

let rec perms xs = match xs with
	| [] -> []
	| [x] -> [[x]]
	| y::ys -> (flatten (List.map (insEverywh y) (perms ys)));;

let rec perms2 xs = match xs with
	| [] -> [[]]
	| y::ys -> (flatten (List.map (insEverywh y) (perms2 ys)));;
