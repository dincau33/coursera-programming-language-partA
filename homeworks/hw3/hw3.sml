(* Benoit D'Incau, Coursera PL, HW3 Code *)

(* ----- Problem 1 -> 6 ----- *)

(* val only_capitals = fn : string list -> string list *)
(* only_capitals takes a string list *)
(* only_capitals returns a string list that has only the strings in the argument that start with an uppercase letter *)
val only_capitals = List.filter (fn str => Char.isUpper(String.sub(str , 0)))

(* val longest_string1 = fn : string list -> string *)
(* longest_string1 takes a string list *)
(* longest_string1 returns the longest string in the list *)
(* In the case of a tie, return the string closest to the beginning of the list *)
val longest_string1 =
	List.foldl (fn (str1, str2) => if (String.size(str1) > String.size(str2)) then str1 else str2 ) ""

(* val longest_string2 = fn : string list -> string *)
(* longest_string2 takes a string list *)
(* longest_string2 returns the longest string in the list *)
(* in the case of ties it returns the string closest to the end of the list *)
val longest_string2 =
	List.foldl (fn (str1, str2) => if (String.size(str1) >= String.size(str2)) then str1 else str2 ) ""

(* val longest_string_helper = fn : (int * int -> bool) -> string list -> string *)
(* longest_string_helper takes a function and a string list *)
(* longest_string_helper returns the longest string in the list *)
fun longest_string_helper f =
	List.foldl (fn (str1, str2) => if f(String.size(str1),String.size(str2)) then str1 else str2 ) ""

(* val longest_string3 = fn : string list -> string *)
(* longest_string3 takes a string list *)
(* longest_string3 returns the longest string in the list *)
(* in the case of ties it returns the string closest to the beginning of the list *)
val longest_string3 = longest_string_helper (fn (x,y) => x > y)

(* val longest_string4 = fn : string list -> string *)
(* longest_string4 takes a string list *)
(* longest_string4 returns the longest string in the list *)
(* in the case of ties it returns the string closest to the end of the list *)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

(* val longest_capitalized = fn : string list -> string *)
(* longest_capitalized takes a string list *)
(* longest_capitalized returns the longest string in the list that begins with an uppercase letter, or "" if there are no such strings  *)
(* in the case of ties it returns the string closest to the beginning of the list *)
val longest_capitalized  = longest_string3 o only_capitals

(* val rev_string = fn : string -> string *)
(* rev_string takes a string *)
(* rev_string returns the string that is the same characters in reverse order *)
val rev_string = String.implode o List.rev o String.explode

(* ----- Problem 7 -> 8 ----- *)

exception NoAnswer

(* val first_answer = fn : (’a -> ’b option) -> ’a list -> ’b *)
(* first_answer takes a function and a list *)
(* first_answer returns v as long as applying first argument to element of second argument return SOME(v) *)
(* first_answer returns NoAnswer exception if applying first argument to all element of second argument return NONE *)
fun first_answer f lst =
	case lst of
		[] => raise NoAnswer
	| l::lst' => case f(l) of
								SOME(v) => v
							|	NONE 		=> first_answer f lst'

(* val all_answers = fn : (’a -> ’b list option) -> ’a list -> ’b list option *)
(* all_answers takes a function and an option of list*)
(* all_answers returns NONE if applying first argument to element of second argument return NONE for one element *)
(* all_answers returns SOME lst if applying first argument to element of second argument return SOME(lst1) for every element *)
fun all_answers f lst =
	let
		fun all_answers_helper ([],acc) = SOME(acc)
		 	| all_answers_helper (l::lst',acc) = case f(l) of
																							NONE 		=> NONE
																						|	SOME(v) => all_answers_helper(lst',acc @ v)
	in
		all_answers_helper (lst,[])
	end

(* ----- Problem 9 -> 12 ----- *)

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
			val r = g f1 f2
    in
			case p of
			    Wildcard          => f1 ()
			  | Variable x        => f2 x
			  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
			  | ConstructorP(_,p) => r p
			  | _                 => 0
    end

(* val count_wildcards = fn : pattern -> int *)
(* count_wildcards takes a pattern *)
(* count_wildcards returns how many Wildcard patterns it contains. *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(* val count_wild_and_variable_lengths = fn : pattern -> int *)
(* count_wild_and_variable_lengths takes a pattern *)
(* count_wild_and_variable_lengths returns the number of Wildcard patterns it contains plus the sum of the string lengths of all the variables in the variable patterns it contains. *)
val count_wild_and_variable_lengths = g (fn () => 1) String.size

(* val count_some_var = fn : string * pattern -> int *)
(* count_some_var takes a pattern *)
(* count_some_var returns the number of times the string appears as a variable in the pattern *)
fun count_some_var (str,p) = g (fn _ => 0) (fn str' => if (str = str') then 1 else 0) p

(* val check_pat = fn : pattern -> boolean *)
(* check_pat takes a pattern *)
(* check_pat returns true if and only if all the variables appearing in the pattern are distinct from each other *)
fun check_pat p =
	let
		fun all_variables_name (p) =
			case p of
				Variable x 				=> [x]
			| ConstructorP(_,p') => all_variables_name(p')
			| TupleP ps  				=> List.foldl (fn (p',acc) => all_variables_name(p') @ acc) [] ps
			| _          				=> []

		fun unique ([]) = true
			| unique (x::xs) = (not (List.exists (fn y => (y = x)) xs)) andalso unique(xs)
	in
		(unique o all_variables_name) p
	end

(* val match = fn : valu * pattern -> (string * valu) list option *)
(* match takes a valu and pattern *)
(* match returns a (string * valu) list option, namely NONE if the pattern does not match and SOME lst where lst is the list of bindings if it does. *)
fun match (v,p) =
	case (v,p) of
		(_,Wildcard)          										=> SOME([])
	| (_,Variable s)        										=> SOME([(s,v)])
	| (Unit,UnitP)															=> SOME([])
	| (Const n,ConstP n')												=> if (n = n') then SOME([]) else NONE
	| (Tuple vs,TupleP ps)         							=> if List.length vs <> List.length ps
										                             then NONE
										                             else all_answers (fn (v', p') => match (v', p')) (ListPair.zip (vs, ps))
	| (Constructor(s,v'),ConstructorP(s',p')) 	=> if (s = s') then match(v',p') else NONE
	| _                													=> NONE

(* val first_match = fn : valu * pattern list -> (string * valu) list option *)
(* first_match takes a value and a list of patterns *)
(* first_match returns (string * valu) list option, namely NONE if no pattern in the list matches or SOME lst where lst is the list of bindings for the first pattern in the list that matches *)
fun first_match v pl =
	SOME(first_answer (fn p => match (v,p)) pl)
	handle NoAnswer => NONE
