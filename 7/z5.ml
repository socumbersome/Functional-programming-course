type 'a lnode = {item: 'a; mutable next: 'a lnode};;
#print_length 13;;

let rec mkrange a b = if a > b then [] else (a::(mkrange (a+1) b));;

(* a circular list points to its tail (i.e. last element) *)
let mk_singleton_circular_list e =
	let rec x = {item = e; next = x}
	in x;;

let insert_head e l =
	let x = {item = e; next = l.next}
	in l.next <- x; l;;

let insert_tail e l =
	let x = {item = e; next = l.next}
	in l.next <- x; x;;

let mk_circular_list (e::es) = 
	let l = mk_singleton_circular_list e
	in List.fold_right insert_tail (List.rev es) l;;

let elim_head l = l.next <- (l.next).next; l;;

let is_singleton l = l == l.next;;

let c = mk_circular_list [1;2;3;4;5];;

let jozef n m =
	let rec walk circ k = 
		if k = 0 then circ
		else walk (circ.next) (k - 1)
	in let rec eliminate circ = 
		if is_singleton circ then [circ.item]
		else 
			let shiftedcirc = walk circ (m - 1) 
			in let nitem = shiftedcirc.next.item
			in nitem::(eliminate (elim_head shiftedcirc))
	in eliminate (mk_circular_list (mkrange 1 n));;
