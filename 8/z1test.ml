module type PQUEUE =
sig
type priority
type 'a t
exception EmptyPQueue
val empty : 'a t
val insert : 'a t -> priority -> 'a -> 'a t
val remove : 'a t -> priority * 'a * 'a t
end
module type ORDTYPE =
sig
type t
val compare : t -> t -> int
end
module OrdInt : ORDTYPE with type t = int = struct
type t = int
let compare x y =
if x = y
then 0
else (if x > y
then 1
else -1)
end

module OPqueue = functor (Ordt: ORDTYPE) -> (struct
type priority = Ordt.t
type 'a t = (priority * 'a) list
exception EmptyPQueue
let empty = ([] : 'a t)
let insert queue pr x =
(pr,x)::queue
let find_max_p lst = match lst with
| []	-> raise EmptyPQueue
| hd::tl ->
List.fold_right
(fun (p,_) maxp -> (if Ordt.compare p maxp = 1 then p else maxp))
tl (fst hd)
let rec remove queue =
let rec _remove qlist maxp = match qlist with
| []	-> raise EmptyPQueue
| (p,x)::qrest	->
if Ordt.compare maxp p = 0
then (p,x,qrest)
else (let (resp, resx, rest) = _remove qrest maxp
in (resp, resx, (p,x)::rest))
in _remove queue (find_max_p queue)
end : PQUEUE with type priority = Ordt.t)
