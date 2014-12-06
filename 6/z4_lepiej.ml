
let eol cont = fun (s:string) -> cont (s ^ "\n");;
let inr cont = fun (s:string) (i:int) -> cont (s ^ (string_of_int i));;
let flt cont = fun (s:string) (f:float) -> cont (s ^ (string_of_float f));;
let str cont = fun (s:string) (ss:string) -> cont (s ^ ss);;
let lit (l:string) cont = fun (s:string) -> cont (s ^ l);;

(* it's good to think that a directive always has a type
	cont_type -> str -> args -> result_of_cont_type
and, going deeper, assuming cont_type = str -> a, it becomes
	(str -> a) -> str -> args -> a
where args might be epsilon! (such that a -> epsilon -> b = a -> b)
that way, having two directives
dir1: (str -> a1) -> str -> args1 -> a1
dir2: (str -> a2) -> str -> args2 -> a2
if we want to compose them somehow, it turns out that
an ordinary function composition will do.
Proof:
We want to obtain: dir1 ++ dir2: (str -> a3) -> str -> args3 -> a3
Because a composition of two directives has to be a directive too.
Let's assume that (f ++ g) x = f (g x)
So if f: alpha -> beta, g: gamma -> delta, then
alpha = delta and (f ++ g): gamma -> beta
In our case (with dir1 and dir2), we have
alpha = str -> a1
beta = str -> args1 -> a1
gamma = str -> a2
delta = str -> args2 -> a2
And we want alpha = delta, i.e.
str -> a1 = str -> args2 -> a2
so a1 = args2 -> a2
And the result dir1 ++ dir2 is of type gamma -> beta, i.e.
(str -> a2) -> str -> args1 -> a1
and substituting args2 -> a2 for a1, we finally obtain
(str -> a2) -> str -> args1 -> args2 -> a2
(str -> a3) -> str ->      args3     -> a3
And voila! It turned out that a3 = a2 and args3 = args1 -> args2
which seems quite likely to be "correct", or at least what we wanted to achieve...
[]
Recapitualing: function composition is a good (maybe even only) candidate
for composing directives.
*)

let (++) dir1 dir2 = fun x -> dir1 (dir2 x);;

let sprintf dirs = dirs (fun x -> x) "";;

let nkotow n = sprintf
	(lit "Ala ma " ++ inr ++ lit " kot" ++ str ++ lit ".")
	n
	(if n = 1 then "a" else if 1 < n && n < 5 then "y" else "ow");;
