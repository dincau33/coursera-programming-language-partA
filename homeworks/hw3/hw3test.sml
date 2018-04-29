(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3.sml";

(* ----- Problem 1 -> 6 ----- *)

val test1_0 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_1 = only_capitals ["a","B","c"] = ["B"]
val test1_2 = only_capitals ["a","b","c"] = []
val test1_3 = only_capitals ["afadfa","Arina","Benoit"] = ["Arina","Benoit"]
val test1_4 = only_capitals [] = []

val test2_0 = longest_string1 ["A","bc","C"] = "bc"
val test2_1 = longest_string1 ["A","bc","ab"] = "bc"
val test2_2 = longest_string1 [] = ""
val test2_3 = longest_string1 ["Afdfadfa","bc","ab"] = "Afdfadfa"

val test3_0 = longest_string2 ["A","bc","C"] = "bc"
val test3_1 = longest_string2 ["A","bc","ab"] = "ab"
val test3_2 = longest_string2 [] = ""
val test3_3 = longest_string2 ["Afdfadfa","bc","ab"] = "Afdfadfa"

val test4a_0 = longest_string3 ["A","bc","C"] = "bc"
val test4a_1 = longest_string3 ["A","bc","ab"] = "bc"
val test4a_2 = longest_string3 [] = ""
val test4a_3 = longest_string3 ["Afdfadfa","bc","ab"] = "Afdfadfa"

val test4b_0 = longest_string4 ["A","bc","C"] = "bc"
val test4b_1 = longest_string4 ["A","bc","ab"] = "ab"
val test4b_2 = longest_string4 [] = ""
val test4b_3 = longest_string4 ["Afdfadfa","bc","ab"] = "Afdfadfa"

val test5_0 = longest_capitalized ["A","bc","C"] = "A"
val test5_1 = longest_capitalized ["Afdfadfa","bc","ab"] = "Afdfadfa"
val test5_2 = longest_capitalized [] = ""

val test6_0 = rev_string "abc" = "cba"
val test6_1 = rev_string "" = ""

(* ----- Problem 7 -> 8 ----- *)

val test7_0 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8_0 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_1 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME([2,3,4,5,6,7])
val test8_2 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME([])

(* ----- Problem 9 -> 12 ----- *)

val test9a_0 = count_wildcards (Wildcard) = 1
val test9a_1 = count_wildcards (Variable("x")) = 0
val test9a_2 = count_wildcards (ConstructorP("Constructor",Wildcard)) = 1
val test9a_3 = count_wildcards (TupleP([Wildcard,Wildcard,(Variable "x")])) = 2
val test9a_4 = count_wildcards (TupleP([Wildcard,ConstructorP("Constructor",Wildcard),Wildcard])) = 3
val test9a_5 = count_wildcards (ConstP(1)) = 0

val test9b_0 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_1 = count_wild_and_variable_lengths (Variable("abcd")) = 4
val test9b_2 = count_wild_and_variable_lengths (TupleP([Wildcard,Wildcard,(Variable "xxx")])) = 5
val test9b_3 = count_wild_and_variable_lengths (Wildcard) = 1
val test9b_4 = count_wild_and_variable_lengths (ConstP(1)) = 0

val test9c_0 = count_some_var ("x", Variable("x")) = 1
val test9c_1 = count_some_var ("x", ConstP(1)) = 0
val test9c_2 = count_some_var ("x", TupleP([(Variable "x"),Wildcard,(Variable "x")])) = 2

val test10_0 = check_pat (Variable("x")) = true
val test10_1 = check_pat (TupleP([(Variable "x"),Wildcard,(Variable "x")])) = false
val test10_2 = check_pat (TupleP([(Variable "x"),Wildcard,(Variable "y")])) = true

val test11_0 = match (Const(1), UnitP) = NONE
val test11_1 = match (Unit, UnitP) = SOME([])
val test11_2 = match (Unit, Wildcard) = SOME([])
val test11_3 = match (Const(1), (Variable "x")) = SOME([("x",Const(1))])
val test11_4 = match (Const(1), ConstP(1)) = SOME([])
val test11_5 = match (Constructor("Constructor",Const(1)), ConstructorP("Constructor",Wildcard)) = SOME([])
val test11_6 = match (Constructor("Constructor",Const(1)), ConstructorP("Constructor",(Variable "x"))) = SOME([("x",Const(1))])
val test11_7 = match (Tuple([Const(1), Const(2)]), TupleP([(Variable "x"),(Variable "y")])) = SOME([("x",Const(1)),("y",Const(2))])

val test12_0 = first_match Unit [UnitP] = SOME([])
val test12_1 = first_match Unit [ConstP(1),Wildcard] = SOME([])
val test12_2 = first_match Unit [ConstP(1),ConstP(2)] = NONE
val test12_3 = first_match Unit [] = NONE
