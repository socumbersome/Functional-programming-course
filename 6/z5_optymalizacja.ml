type regexp =
	| Atom of char
	| And of regexp * regexp (* r1r2 *)
	| Or of regexp * regexp (* r1|r2 *)
	|Star of regexp;; (* r* *)

(* match_regexp : 
regexp -> char list -> (char list -> (unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a *)
let rec match_regexp reg text succ_cont fail_cont = match reg with
	| Atom c -> 
		(match text with
			| t::ts -> if t = c then succ_cont ts
				else fail_cont()
			| _ -> fail_cont()
		)
	| And(reg1, reg2) -> 
		match_regexp reg1 text (fun txt_rest -> 
			match_regexp reg2 txt_rest succ_cont fail_cont) fail_cont
	| Or(reg1, reg2) ->
		match_regexp reg1 text succ_cont (fun () -> 
			match_regexp reg2 text succ_cont fail_cont)
	| Star r -> 
		match_regexp r text (fun txt_rest -> 
			match_regexp reg txt_rest succ_cont fail_cont) 
			(fun () -> succ_cont text)
	;;

let run reg text = match_regexp reg text (fun t -> t = []) (fun () -> false);;

let stol s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in exp (String.length s - 1) [];;

let reg1 = And(Atom 'a', And(Atom 'b', Atom 'c'));;
let reg2 = And(And(Atom 'a', Star(Or(Atom 'b', Atom 'c'))), Atom 'd');;
let txts = List.map stol ["abc"; "qabc"; "aabc"; "abcd"; "bcd"; "abbbcd"; "ad"; "acd"];;
let res1 = [true; false; false; false; false; false; false; false];;
let res1' = List.map (run reg1) txts;;
let res2 = [false; false; false; true; false; true; true; true];;
let res2' = List.map (run reg2) txts;;
let check1 = List.for_all2 (=) res1' res1;;
let check2 = List.for_all2 (=) res2' res2;;