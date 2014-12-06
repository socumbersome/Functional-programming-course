
let lit l cont s = cont (s ^ l);;
let eol cont s = cont (s ^ "\n");;
let inr cont = fun s i -> cont (s ^ string_of_int(i));;
let flt cont = fun s f -> cont (s ^ string_of_float(f));;
let str cont = fun s ss -> cont (s ^ ss);;

let (++) dir1 dir2 = fun s -> dir1(dir2 s);;

let sprintf cont = cont (fun s -> s) "";;

let kot = sprintf (lit "Ala ma " ++ inr ++ " kot" ++ str ++ lit ".");;

let nkotow n = 
	sprintf (lit "Ala ma " ++ inr ++ " kot" ++ str ++ lit ".")
	n
	(if n = 1 then "a" else if 1 < n && n < 55 then "y" else "ów");;
