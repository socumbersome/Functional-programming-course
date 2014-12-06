let zloz f g x = f(g x);;

let rec iteruj f ile = 
	if ile=0 then (fun y -> y) 
	else zloz f (iteruj f (ile-1))

let mnoz x y = iteruj ((+) x) y 0;;

let (^^^) x y = iteruj (mnoz x) y 1;;

print_int(mnoz 3 5);;
print_newline ();
print_int(mnoz 3 0);;
print_newline ();
print_int(mnoz 0 5);;
print_newline ();
print_int(2 ^^^ 4);;
print_newline ();
print_int(5 ^^^ 0);;
print_newline ();
print_int(0 ^^^ 8);;
print_newline ();
