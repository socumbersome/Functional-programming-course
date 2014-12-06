type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec lfromf k = LCons (k, function () -> lfromf (k +. 1.));;
let rec lfrom k = LCons (k, function () -> lfrom (k + 1));;

let rec ltake = function
	| (0, _) -> []
	| (_, LNil) -> []
	| (n, LCons(x, xf)) -> x::ltake(n-1, xf());;

let rec lmap f = function
	| LNil -> LNil
	| LCons(x, xf) -> LCons(f x, function () -> lmap f (xf()));;

let leibniz = 
	let rec approx indeks last = match indeks with
		| LCons(h, t) ->
			let znak = if h mod 2 = 0 then -1. else 1.
			and hf = float_of_int(h)
			in let nowy = (last +. 4. *. znak *. (1. /. (2. *. hf -. 1.)))
			in LCons(nowy, function () -> approx (t()) nowy)		
		| LNil -> failwith "aaaa!"
	in 
	let first = 4. in		
	LCons(first, function () -> approx (lfrom 2) first) ;;

let rec toftriples f s = match s with
	| LNil -> LNil
	| LCons(x, xf) -> let LCons(y, yf) as xfl = xf() in
		let LCons(z, zf) = yf() in
		LCons(f x y z, function () -> toftriples f xfl);;

let fastpi =
	let eulertr x y z = 
		let ymz = y -. z in		
		z -. ymz *. ymz /. (x -. 2. *. y +. z)
	in toftriples eulertr leibniz;;

(** lazy and force **)
type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons(k, lazy (lfrom (k+1)));;

let rec ltake = function
	| (0, _) -> []
	| (_, LNil) -> []
	| (n, LCons(x, lazy xs)) -> x::ltake(n-1, xs);;

let leibniz = 
	let rec approx indeks last = match indeks with
		| LCons(h, lazy t) ->
			let znak = if h mod 2 = 0 then -1. else 1.
			and hf = float_of_int(h)
			in let nowy = (last +. 4. *. znak *. (1. /. (2. *. hf -. 1.)))
			in LCons(nowy, lazy (approx (t) nowy))		
		| LNil -> failwith "aaaa!"
	in 
	let first = 4. in		
	LCons(first, lazy (approx (lfrom 2) first)) ;;

let rec toftriples f s = match s with
	| LNil -> LNil
	| LCons(x, lazy xf) -> let LCons(y, lazy yf) as xfl = xf in
		let LCons(z, lazy zf) = yf in
		LCons(f x y z, lazy(toftriples f xfl));;

