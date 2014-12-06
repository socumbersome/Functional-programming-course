type proposition = Var of string | Not of proposition | And of proposition * proposition | Or of proposition * proposition ;;

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
let rec eval prop env = match prop with
	| Var s -> List.assoc s env
	| Not p -> not (eval p env)
	| And(pl, pr) -> (eval pl env) && (eval pr env)
	| Or(pl, pr) -> (eval pl env) || (eval pr env);;

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
			good_env = List.find (fun env -> eval pl env && eval pr env) envs
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
and is_tautology prop = match prop with
	| Var s -> (false, Some [(s, false)])
	| Not p -> let (satp, ex) = is_satisfiable p
		in (not satp, ex)
	| And(pl, pr) -> (* p&q is taut. iff ~p|~q is not satisf. *)
		let (satisf, ex) = is_satisfiable (Or(Not(pl), Not(pr)))
		in (not satisf, ex)
	| Or(pl, pr) ->
		let (satisf, ex) = is_satisfiable (And(Not(pl), Not(pr)))
		in (not satisf, ex);;

let extaut = Or(Or(Not (Var "p"), Var "q"), Or(Not (Var "q"), Var "p"));;
let exnottautsat = And(Or(Var "p", Var "q"), Or(Not(Var "p"), Not(Var "q")));;
let exnottautsat2 = Or(Var "p", Not (Or (Not (Var "p"), Var "q" )));;
let exnotsat = And(Not(Not(Var "p")), Not(Or(Var "p", Var "q")) );;

(**********************************)

let rec toNNF prop = match prop with
	| Var s -> prop
	| Not(Var s) -> prop
	| Not(And(p, q)) -> toNNF (Or(Not p, Not q))
	| Not(Or(p, q)) -> toNNF (And(Not p, Not q))
	| Not(Not p) -> toNNF p
	| And(p, q) -> And(toNNF p, toNNF q)
	| Or(p, q) -> Or(toNNF p, toNNF q);;

let toCNF prop =
	let rec or_over_and prop = match prop with
		| Or(p, And(q, r)) -> 
			let pq = tocnf (Or(p, q))
			and pr = tocnf (Or(p, r))
			in And(pq, pr)
		| Or(And(q, r), p) ->
			let qp = tocnf (Or(q, p))
			and rp = tocnf (Or(r, p))
			in And(qp, rp)
		| _ -> prop
	and tocnf prop = match prop with
		| Var s -> prop
		| Not p -> prop (* because prop is in NNF! *)
		| Or(p, q) -> let pnf = tocnf p and qnf = tocnf q
			in or_over_and (Or(pnf, qnf))
		| And(p, q) -> let pnf = tocnf p and qnf = tocnf q
			in And(pnf, qnf)
	in tocnf (toNNF prop);;

let cnfcheck = Or(Or(And(Var "a", Var "b"), And(Var "c", Var "d")), Var "e");;

let rec prop_to_string_cheap prop = match prop with
	| Var s -> s
	| Not p -> "~"^(prop_to_string_cheap p)
	| Or(p, q) -> (prop_to_string_cheap p)^" || "^(prop_to_string_cheap q)
	| And(p, q) -> (prop_to_string_cheap p)^" && "^(prop_to_string_cheap q);;

let rec get_negated_vars_from prop = match prop with
	| Var s -> []
	| Not(Var s) -> [s] 
	| Not p -> get_negated_vars_from p
	| And(pl, pr) -> 
		unique ((get_negated_vars_from pl) @ (get_negated_vars_from pr))
	| Or(pl, pr) -> 
		unique ((get_negated_vars_from pl) @ (get_negated_vars_from pr));;

let is_tautology_usingCNF prop =
	let is_taut_dnf prop = 
		let vars = get_vars_from prop
		and neg_vars = get_negated_vars_from prop
		in (List.sort compare vars) = (List.sort compare neg_vars)
	in let rec get_clauses cnf = match cnf with
		| And(p, q) -> (get_clauses p) @ (get_clauses q)
		| _ -> [cnf]
	in List.for_all (fun dnf -> is_taut_dnf dnf) (get_clauses (toCNF prop));;

let toDNF prop =
	let rec and_over_or prop = match prop with
		| And(p, Or(q, r)) -> 
			let pq = todnf (And(p, q))
			and pr = todnf (And(p, r))
			in Or(pq, pr)
		| And(Or(q, r), p) ->
			let qp = todnf (And(q, p))
			and rp = todnf (And(r, p))
			in Or(qp, rp)
		| _ -> prop
	and todnf prop = match prop with
		| Var s -> prop
		| Not p -> prop (* because prop is in NNF! *)
		| And(p, q) -> let pnf = todnf p and qnf = todnf q
			in and_over_or (And(pnf, qnf))
		| Or(p, q) -> let pnf = todnf p and qnf = todnf q
			in Or(pnf, qnf)
	in todnf (toNNF prop);;

(* buggy! on exnotsat gives false... :( *)
let is_contradictory_usingDNF prop =
	let is_contr_cnf prop =
		let vars = get_vars_from prop
		and neg_vars = get_negated_vars_from prop
		in (* true iff intersection of vars and neg_vars is empty *)
		List.for_all (fun v -> not (List.mem v neg_vars)) vars
	in let rec get_clauses dnf = match dnf with
		| Or(p, q) -> (get_clauses p) @ (get_clauses q)
		| _ -> [dnf]
	in List.for_all (fun cnf -> is_contr_cnf cnf) (get_clauses (toDNF prop));;

