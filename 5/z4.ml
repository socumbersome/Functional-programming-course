
(* first arg in HNode is a list of symbols *)
type 'a huffTree = HLeaf of 'a * float | 
	HNode of ('a list * 'a huffTree * float * 'a huffTree);;

let symbol2freq = [('A', 0.1); ('B', 0.2); ('C', 0.3); ('D', 0.4)];;

let getFreq = function
	| HLeaf(_, f) -> f
	| HNode(_, _, f, _) -> f;;

let getSymbols = function
	| HLeaf(s, _) -> [s]
	| HNode(ss, _, _, _) -> ss;;

let float2signum f = let eps = 1.0e-10 in
	if f > eps then 1 else if f < eps then -1 else 0;;

let mkHuffEncTree s2f =
	let rec he trees = match trees with
		| [] -> failwith "tree has disappeared!"
		| [t] -> t
		| _ -> let t1::t2::ts = 
			List.sort (fun htl htr -> float2signum((getFreq htl) -. (getFreq htr))) trees
			in he (HNode(
				(getSymbols t1) @ (getSymbols t2),
				t1, (getFreq t1) +. (getFreq t2), t2)::ts)
	in he (List.map (fun (s, f) -> HLeaf(s, f)) s2f);;

let huffEncode hetree symbols =
	let rec encode symbol htree = match htree with
		| HLeaf(s, _) -> if s = symbol then [] else failwith "unrecognised symbol"
		| HNode(_, htl, _, htr) -> 
			if List.mem symbol (getSymbols htl) then
				0::(encode symbol htl)
			else if List.mem symbol (getSymbols htr) then
				1::(encode symbol htr)
			else failwith "unrecognised symbol"
	in List.flatten(List.map (fun s -> encode s hetree) symbols);;

let t = mkHuffEncTree symbol2freq;;
let badacenc = huffEncode t ['B'; 'A'; 'D'; 'A'; 'C'];;

let huffDecode hetree code =
	let rec decsymb hetree code = match hetree with
		| HLeaf(s, _) -> (s, code)
		| HNode(_, hl, _, hr) -> 
			let c::cs = code
			in if c = 0 then decsymb hl cs
			else decsymb hr cs
	in let rec proceed code = 
		let (symb, rest) = decsymb hetree code 
		in if rest = [] then [symb]
		else symb::(proceed rest)
	in proceed code;;

let badacdec = huffDecode t badacenc;;