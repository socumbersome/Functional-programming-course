type ('a, 'b) funmem = FMNil | FMCons of 'a * 'b * (('a, 'b) funmem);;

let fmEmpty = FMNil;;

let rec fmLookup mem x = match mem with
	| FMNil -> None
	| FMCons(x', y', t) -> if x = x' then Some y' else fmLookup t x;;

let fmAdd mem x y = FMCons(x, y, mem);;

let fmfun f = 
	let mem = ref fmEmpty in
	let comp x = match (fmLookup !mem x) with
		| None -> let y = f x in (mem := FMCons(x, y, !mem); y);
		| Some y -> y
	in comp;;

let rec fib n = match n with
	| 0 -> 1
	| 1 -> 1
	| _ -> (fib (n - 1)) + (fib (n - 2));;

let fib_mem = fmfun fib;;

let time comp = let t = Sys.time() in comp(); Sys.time() -. t;;
let rec mkrange a b = if a > b then [] else (a::(mkrange (a+1) b));;
let timefuns fs n = 
	List.map (fun f -> time (fun () -> f n)) fs;;

let slabo = timefibos 35 35;;

(* slaba memoizacja bo funkcja rekurencyjna - warto zapamietywac w kazdym
	wywolaniu rekurencyjnym *)

let fib_cps n cont = match n with
	| 0 -> 1
	| 1 -> 1
	| _ -> (cont (n-1)) + (cont (n-2));;

(* now, f takes a continuation as a last argument *)
let rec fmfunrec f =
	let mem = ref fmEmpty in
	let rec comp x = match (fmLookup !mem x) with
		| None -> let y = f x comp in (mem := FMCons(x, y, !mem); y);
		| Some y -> y
	in comp;;

let fib_mem_better = fmfunrec fib_cps;;