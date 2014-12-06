type proposition = Var of string(*of bool*) | Not of proposition | And of proposition * proposition | Or of proposition * proposition ;;

(* doesn't preserve the original order! *)
let unique xs =
	let unique_from_sorted xs =
		let rec unq xs last = match xs with
			| [] -> [last]
			| y::ys -> if y = last then unq ys last
				else last::(unq ys y)
		in match xs with
			| [] -> []
			| y::ys -> unq ys y
	in unique_from_sorted (List.sort compare xs);;

let rec get_vars_from prop = match prop with
	| Var s -> [s]
	| Not p -> get_vars_from p
	| And(pl, pr) -> 
		unique ((get_vars_from pl) @ (get_vars_from pr))
	| Or(pl, pr) -> 
		unique ((get_vars_from pl) @ (get_vars_from pr));;

(* env is a list of pairs (variable name, its value) : string * bool *)
let rec eval env prop = match prop with
	| Var s -> List.assoc s env
	| Not p -> not (eval env p)
	| And(pl, pr) -> (eval env pl) && (eval env pr)
	| Or(pl, pr) -> (eval env pl) || (eval env pr);;

let rec generate_all_envs vars = match vars with
	| [] -> [[]]
	| x::xs -> let smaller = generate_all_envs xs
		in (List.map (fun env -> (x, true)::env) smaller) @
		(List.map (fun env -> (x, false)::env) smaller);;

let rec extend_env_with_trues from_env with_vars = match with_vars with
	| [] -> from_env
	| v::vs -> if List.exists (fun (var, _) -> v = var) from_env then
		extend_env_with_trues from_env vs
		else extend_env_with_trues ((v, true)::from_env) vs;;

let get_vars_from_env env = List.map (fun (s, _) -> s) env;;

let get_env_from_some = function
	| Some env -> env
	| None -> [];;

(* also gives an example of values making prop true, if possible *)
let rec is_satisfiable prop = match prop with
	| Var s -> (true, Some [(s, true)])
	| Not p -> let (tautp, ex) = is_tautology p in
		let env = if tautp then None else ex
		in (not tautp, env)
	| And(pl, pr) ->
		let vars = get_vars_from prop
		in let envs = generate_all_envs vars
		in (try let
			good_env = List.find (fun env -> eval env pl && eval env pr) envs
			in (true, Some good_env)
			with
				| Not_found -> (false, None)
			)
	| Or(pl, pr) -> let (satl, exl) = is_satisfiable pl
		and (satr, exr) = is_satisfiable pr
		in let envl = get_env_from_some exl
		and envr = get_env_from_some exr
		in if satl then
			(true, Some (extend_env_with_trues envl (get_vars_from_env envr)))
		else if satr then
			(true, Some (extend_env_with_trues envr (get_vars_from_env envl)))
		else (false, None)

(* also gives an example of values making prop false, if possible *)
(* to do: make version with reductions to satisfiability *)
and is_tautology prop = match prop with
	| Var s -> (false, Some [(s, false)])
	| Not p -> let (satp, ex) = is_satisfiable p in
		let env = if satp then None else ex
		in (not satp, env)
	| And(pl, pr) -> let (tautl, exl) = is_tautology pl
		and (tautr, exr) = is_tautology pr
		in let envl = get_env_from_some exl
		and envr = get_env_from_some exr
		in if not tautl then
			(false, Some (extend_env_with_trues envl (get_vars_from_env envr)))
		else if not tautr then
			(false, Some (extend_env_with_trues envr (get_vars_from_env envl)))
		else
			(true, None)
	| Or(pl, pr) -> 
		let vars = get_vars_from prop
		in let envs = generate_all_envs vars
		in try let
			bad_env = List.find (fun env -> not (eval env pl || eval env pr)) envs
			in (false, Some bad_env)
			with
				| Not_found -> (true, None)
				| e -> raise e;;

let extaut = Or(Or(Not (Var "p"), Var "q"), Or(Not (Var "q"), Var "p"));;
let exnottautsat = And(Or(Var "p", Var "q"), Or(Not(Var "p"), Not(Var "q")));;
let exnottautsat2 = Or(Var "p", Not (Or (Not (Var "p"), Var "q" )));;
let exnotsat = And(Not(Not(Var "p")), Not(Or(Var "p", Var "q")) );;
