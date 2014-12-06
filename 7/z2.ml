type 'a list_mutable = LMnil | LMcons of 'a * 'a list_mutable ref;;

let rec concat_copy lm1 lm2 = match lm1 with
	| LMnil -> lm2
	| LMcons(a, tm) -> 
		let newt = ref (concat_copy (!tm) lm2)
		in LMcons(a, newt);;

let x1 = LMcons(1, ref (LMcons(2, ref (LMcons(3, ref LMnil)))));;
let x2 = LMcons(4, ref (LMcons(5, ref (LMcons(6, ref LMnil)))));;
let x3 = LMcons(1, ref (LMcons(2, ref (LMcons(3, ref LMnil)))));;
let x4 = LMcons(7, ref (LMcons(8, ref LMnil)));;

let updt lm1 nt = match lm1 with
	| LMnil -> ()
	| LMcons(_, tm) -> tm := nt;;

let rec concat_share lm1 lm2 = 
	let rec aux lm1 lm2 = match lm1 with
	| LMnil -> ()
	| LMcons(a, tm) -> 
		if (!tm) = LMnil then tm := lm2
		else aux !tm lm2
	in aux lm1 lm2;;


