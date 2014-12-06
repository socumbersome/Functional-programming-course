let rec yrec f x = f (yrec f) x;;
let fixrec f = yrec f;;

let factfixrec = fixrec (
	fun f -> 
		fun n -> if n = 0 then 1
			else n * (f (n-1)) 
	);;

let fix f = let y = ref (fun _ -> failwith "aa")
	in (y := fun f x -> f (!y f) x); (!y) f;;

let fact = fix (
	fun f -> 
		fun n -> if n = 0 then 1
			else n * (f (n-1)) 
	);;

let factsam n =
	let y = ref (fun _ -> failwith "aasdaa")
	in (y := fun x -> if x = 0 then 1 else x * ((!y) (x-1)));
	(!y) n;;
