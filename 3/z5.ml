(* mind reversed order, e.g. [3;4;2;1] corresponds to permutation (1,2,4,3) *)

let find_and_split p xs = 
	let rec fas left_rev right = match right with
		| [] -> raise Not_found
		| y::ys -> if p y then (List.rev left_rev, y, ys)
			else fas (y::left_rev) ys
	in fas [] xs;;

let next_lex_perm ?(cmp = (<)) xs =
	let rec nlp left_rev right = match right with
		| y::z::zs -> 
			if cmp z y then 
				try let (leftl, xel, leftr) = 
					find_and_split (fun el -> cmp z el) (List.rev left_rev)
				in (y::(List.rev leftr)) @ (z::(List.rev leftl)) @ (xel::zs)
				with
					| Not_found -> (z::left_rev) @ (y::zs)
			else nlp (y::left_rev) (List.tl right)
		| [y] -> y::left_rev
		| [] -> raise (Invalid_argument "empty list given")
	in nlp [] xs;;

assert (next_lex_perm [3;4;2;1] = [4;2;3;1]);;
assert (next_lex_perm ['b';'c';'a'] = ['c';'a';'b']);;

let lex_perms ?(cmp = (<)) xs =
	let rec lexp curr acc = 
		if curr = xs then acc 
		else let next = next_lex_perm curr in lexp next (curr::acc)
	in match xs with
	| [] -> [[]]
	| _ -> xs::(List.rev (lexp (next_lex_perm xs) []));;

assert (lex_perms [3;2;1] = 
	[[3; 2; 1]; [2; 3; 1]; [3; 1; 2]; [1; 3; 2]; [2; 1; 3]; [1; 2; 3]]);;
assert (lex_perms [2;1;3] =
	[[2; 1; 3]; [1; 2; 3]; [3; 2; 1]; [2; 3; 1]; [3; 1; 2]; [1; 3; 2]]);;
