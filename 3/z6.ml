let rec mklist from too =
	if from > too then [] else from::(mklist (from+1) too);;

let scands = mklist 5 99;;

let pairs_with_sum s =
	let shalf = s/2 in
	let rec pws x acc = if x > shalf then acc else pws (x+1) ((x, s-x)::acc)
	in pws 2 [];;

let is_prime n = n >=2 && List.for_all (fun d -> n mod d <> 0) (mklist 2 (n-1));;

let scands2 = List.filter 
	(fun s -> List.for_all 
		(fun (x, y) -> 
			(not (is_prime x)) || (not (is_prime y))) (pairs_with_sum s)
	) scands;;

let prods_for_sum s = List.map (fun (x, y) -> x*y) (pairs_with_sum s);;

let prods_for_sums = List.map (fun s -> (s::(prods_for_sum s))) scands2;;

(* P must belong to exactly one of the list from prods_for_sums *)

let unique_prods_for_sums = List.map
	(fun (sum::prods) -> (sum::(List.filter (
		fun p -> List.for_all 
			(fun (s2::prods2) -> sum = s2 || not (List.mem p prods2)) 
			prods_for_sums
		) prods ))
	) prods_for_sums;;

let unique_prod_for_s = List.filter 
	(fun (sum::prods) -> List.length prods = 1
	) unique_prods_for_sums;;

let [s; p] = List.hd unique_prod_for_s;;
let delta = s*s - 4*p;;
let sqrtd = sqrt (float_of_int delta);;
let xf = ((float_of_int s) -. sqrtd) /. 2.;;
let yf = ((float_of_int s) +. sqrtd) /. 2.;;
