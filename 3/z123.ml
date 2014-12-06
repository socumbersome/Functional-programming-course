(* zad 1 *)
let evalog wsps x = 
	let rec evalacc wsps acc = match wsps with
		| [] -> acc
		| najw::reszta -> evalacc reszta (najw +. acc *. x)
	in evalacc wsps 0.;;

let eval wsps x = List.fold_left (fun acc wsp -> wsp +. acc *. x) 0. wsps;;

let w = evalog [1.;0.;-1.;2.] 3.;;

(* zad 2 *)
let evalrevog wsps x =
	let rec evalrevacc wsps acc potx = match wsps with
		| [] -> acc
		| akt::reszta -> evalrevacc reszta (potx *. akt +. acc) (x *. potx)
	in evalrevacc wsps 0. 1.;;

let w = evalrevog [2.;-1.;0.;1.] 3.;;

let evalrev wsps x = 
	let (wyn, _) = 
		List.fold_left 
		(fun (acc, potx) wsp -> (potx *. wsp +. acc, x *. potx )) 
		(0. , 1.) wsps
	in wyn;;

(* zad 3 *)
let derivog wsps =
	let rec derivacc wsps mult acc = match wsps with
		| [] -> List.rev acc
		| akt::reszta -> derivacc reszta (mult +. 1.) ((mult *. akt)::acc)
	in derivacc (List.tl wsps) 1. [];;

let w = deriv [1.;0.;-1.;2.];;

let deriv wsps =
	let (_, wyn) = 
		List.fold_left 
		(fun (mult, acc) wsp -> (mult +. 1., ((mult *. wsp)::acc) ))
		(1. , []) (List.tl wsps)
	in List.rev wyn;;
