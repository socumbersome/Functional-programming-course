let is_pal xs =
	let rec palaux leftr right licz = match licz with
		| [] -> let x::xs = leftr and y::ys = right
			in (x = y, xs, ys)
		| [_] -> let x::xs = leftr and y::w::ys = right
			in (x = w, xs, ys)
		| _::_::cs -> 
			let (czy, odlewej, odprawej) = 
			palaux ((List.hd right)::leftr) (List.tl right) cs
			in match odlewej with
				| [] -> (czy, [], [])
				| t::ts -> let p::ps = odprawej in
				(czy && (t = p), ts, ps)
	in match xs with
	| [] -> true
	| xs -> let (czy, _, _) = palaux [] xs xs in czy;;

