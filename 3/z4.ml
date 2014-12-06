let isSquareMatrix xss = match xss with
	| [] -> true
	| ys::yss -> let dim = List.length ys in
		dim = List.length xss 
		&&
		List.for_all (fun ts -> dim == List.length ts) yss;;

assert (isSquareMatrix [] = true);;
assert (isSquareMatrix [[1]] = true);;
assert (isSquareMatrix [[1;2];[3;4]] = true);;
assert (isSquareMatrix [[1];[2]] = false);;
assert (isSquareMatrix [[1;2];[1;2;3]] = false);;

let nthColumn matrix n =
	let mynth n xs = List.nth xs (n - 1)
	in  try List.map (mynth n) matrix with
		| Failure _ | Invalid_argument _ -> 
			raise (Invalid_argument 
				((string_of_int n) ^ "'th column doesn't exist"));;

assert (nthColumn [[1;2;3];[4;5;6];[7;8;9]] 1 = [1;4;7]);;
assert (nthColumn [[1;2;3];[4;5;6];[7;8;9]] 2 = [2;5;8]);;
assert (try ignore(nthColumn [[1;2;3];[4;5;6];[7;8;9]] 4); false with
		| Invalid_argument _ -> true
		| _ -> false
		);;

let transpose matrix = 
	let n = List.length matrix
	in let rec gather nth acc = match nth with
		| 0 -> acc
		| nth -> gather (nth - 1) ((nthColumn matrix nth)::acc)
	in gather n [];;

assert (transpose [[1]] = [[1]]);;
assert (transpose [[1;2];[3;4]] = [[1;3];[2;4]]);;

let rec zip xs ys = match (xs, ys) with
	| ([], []) -> []
	| (b::bs, c::cs) -> (b, c)::(zip bs cs)
	| _ -> raise (Invalid_argument "lists have different lengths");;

assert (zip [1.;2.;3.] ["a";"b";"c"] = [(1., "a"); (2., "b"); (3., "c")]);;

let zipf f xs ys = List.map (fun (x, y) -> f x y) (zip xs ys);;

assert (zipf ( +. ) [1.;2.;3.] [4.;5.;6.] = [5.;7.;9.]);;

let mult_vec vec matrix =
	let dot_product v1 v2 = List.fold_left (+.) 0. (zipf ( *. ) v1 v2)
	in List.map (fun col -> dot_product vec col) (transpose matrix);;

assert (mult_vec [1.;2.] [[2.;0.];[4.;5.]] = [10.;10.]);;

let mult_matrices mat1 mat2 = 
	try List.map (fun row -> mult_vec row mat2) mat1 with
		| Invalid_argument _ -> 
			raise (Invalid_argument "matrices have different sizes");;

assert (mult_matrices [[1.;2.];[3.;4.]]  [[2.;0.];[4.;5.]] = [[10.; 10.]; [22.; 20.]]);;
