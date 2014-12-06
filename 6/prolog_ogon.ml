(* prolog.ml *)

(* An atom is either a propositional variable or an alternative of two goals. *)
type atom = 
  | Atom of string 
  | Or of goal * goal
(* A goal is a list of atoms. *)
and goal = atom list
(* A clause consists of a head (a propositional variable) and a body (a goal). *)
type clause = string * goal
(* A Prolog program is a list of clauses. *)
type program = clause list

(* Search a program for a clause with a given head. *)    
let rec lookup x pgm =
  match pgm with
    | [] ->
      None
    | (y, g) :: p ->
      if x = y then Some g else lookup x p

(* 
A propositional Prolog interpreter written in CPS with two layers of continuations: 
a success and a failure continuation. The failure continuation is parameterless and 
it specifies what should happen next in case of a failure in the current goal. The 
success continuation takes a failure continuation as an argument and it specifies 
what should happen next in case the current goal is satisfied. 
*)
	
(* eval_atom : atom -> program -> ((unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a *)
let rec eval_atom a p sc fc n =
  match a with
    | Atom x ->
      (match (lookup x p) with
		| None -> 
		  fc n
		| Some g -> 
		  eval_goal g p sc fc n)
    | Or (g1, g2) ->
      eval_goal g1 p sc (fun k -> eval_goal g2 p sc fc k) n

(*  eval_goal : goal -> program -> ((unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a  *)
and eval_goal g p sc fc n =
  match g with
    | [] -> 
	(* jedyne miejsce, gdzie zwiekszamy licznik sukcesow *)
      sc fc n
    | a :: gs -> 
      eval_atom a p (fun fc' k -> eval_goal gs p sc fc' k) fc n;;

(*  run : goal ->  program -> bool  *)
(* let run g p = eval_goal g p (fun _ -> true) (fun () -> false) *)

let run_with_counting2 g p = eval_goal g p (fun f n -> f (n+1)) (fun n -> n) 0;;

(* tests *)
  
let p1 = [("a", [Atom "b"; Atom "c"]);
	  ("b", [])]
  
let p2 = [("a", [Atom "b"; Or ([Atom "c"], [Atom "d"]); Atom "e"]);
	  ("b", [Atom "d"]);
	  ("d", []);
	  ("e", [Atom "d"])]
  
let p3 = [("a", [Atom "b"; Or ([Atom "c"], [Atom "d"]); Atom "e"]);
	  ("b", [Atom "d"]);
	  ("c", []);
	  ("d", []);
	  ("e", [Atom "d"])]
  
let g1 = [Atom "a"] 

let v1_1 = run_with_counting2 g1 p1
let v1_2 = run_with_counting2 g1 p2
let v1_3 = run_with_counting2 g1 p3  

(* eof *)