
let lit (l:string) cont = cont l;;
let eol cont = cont "\n";;
let inr cont = fun i -> cont (string_of_int i);;
let flt cont = fun f -> cont (string_of_float f);;
let str cont = fun (s:string) -> cont s;;

let (++) dir1 dir2 cont = dir1 (fun x -> dir2 (fun y -> cont (x ^ y)));;

let sprintf dirs = dirs (fun s -> s);;

let kot = sprintf (lit "Ala ma " ++ inr ++ lit " kot" ++ str ++ lit ".");;

let nkotow n = 
	sprintf (lit "Ala ma " ++ inr ++ lit " kot" ++ str ++ lit ".")
	n
	(if n = 1 then "a" else if 1 < n && n < 5 then "y" else "ow");;
