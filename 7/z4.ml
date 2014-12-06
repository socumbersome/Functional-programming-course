
let (fresh, reset) = 
	let rec nr = ref 0
	and fresh name = nr := !nr + 1; name ^ string_of_int !nr
	and reset from = nr := from - 1
	in (fresh, reset);;