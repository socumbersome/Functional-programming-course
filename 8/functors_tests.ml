module type AT =
sig
	type t
end;;
module type BT =
sig
	type t
	type a
end;;

module A : AT =
struct
	type t = int
end;;
module B : BT with type a = a =
struct
	type a
	type t = a list
end;;

module type ABT =
sig
	type t
	module InsideA : AT
	module InsideB : BT
end;;

module ABT_Functor (AArg:AT) (BArg:BT with type a = AArg.t) : ABT =
struct
	module InsideA = AArg
	module InsideB = BArg
	type t = Sth of InsideA.t * InsideB.t
end;;

module ABTA = ABT_Functor (A);;
module ABTAB = ABT_Functor (A) (B);;
