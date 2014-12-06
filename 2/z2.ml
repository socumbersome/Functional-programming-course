let rec funa x = 
	match x with
	| 0 -> 1
	| 1 -> 2
	| n when n>1 -> 2*(funa (n-2)) - (funa (n-1)) + 1
	| _ -> failwith "ujemny arg";;

let funaog x =
	let 
	rec funwacc przedost ost x = 
		if x=0 then przedost
		else funwacc ost (2*przedost - ost + 1) (x-1)
	and zerowe = 1
	and pierwsze = 2
	in match x with
		| 0 -> zerowe
		| 1 -> pierwsze
		| n when n>1 -> funwacc zerowe pierwsze x
		| _ -> failwith "ujemny arg";;
