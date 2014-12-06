let hd s = s 0
let tl s = fun el -> s (el + 1)
let add c s = fun index -> (s index) + c
let map f s = fun index -> f(s index)
let map2 f s1 s2 = fun index -> f((s1 index) (s2 index))
let replace n a s = fun index -> if index mod n = 0 then a else s index;;
let take n s = fun index -> s (index * n);;
let rec fold f a s index = 
	if index = 0 then f a (s 0)
	else f (fold f a s (index - 1)) (s index);;
let rec tabulate ?(l=0) r s = 
	if l > r then []
	else (s l)::(tabulate ~l:(l + 1) r s);;

let nat i = i;;
let parz = take 2 nat;;
let sumynat = fold (+) 0 nat;;
open Printf;;
List.iter (printf "%d ") (tabulate 7 sumynat);;
