(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2.sml";

(* ----- Problem 1 ----- *)

val test1_0 = all_except_option ("string", ["string"]) = SOME []
val test1_1 = all_except_option ("string", ["string1"]) = NONE
val test1_2 = all_except_option ("string", []) = NONE
val test1_3 = all_except_option ("string", ["string2","string","string3"]) = SOME ["string2","string3"]

val test2_0 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2_1 = get_substitutions1 ([[],[]], "foo") = []
val test2_2 = get_substitutions1 ([], "foo") = []
val test2_3 = get_substitutions1 ([["foo","f","fo"],[]], "foo") = ["f","fo"]
val test2_4 = get_substitutions1 ([["foo","f","fo"],["bla","black"]], "foo") = ["f","fo"]
val test2_5 = get_substitutions1 ([["foo","f","fo"],["bla","black"],["fooo","fo","foo"]], "foo") = ["f","fo","fooo","fo"]

val test3_0 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3_1 = get_substitutions2 ([[],[]], "foo") = []
val test3_2 = get_substitutions2 ([], "foo") = []
val test3_3 = get_substitutions2 ([["foo","f","fo"],[]], "foo") = ["f","fo"]
val test3_4 = get_substitutions2 ([["foo","f","fo"],["bla","black"]], "foo") = ["f","fo"]
val test3_5 = get_substitutions2 ([["foo","f","fo"],["bla","black"],["fooo","fo","foo"]], "foo") = ["f","fo","fooo","fo"]

val test4_0 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
val test4_1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred1", middle="W", last="Smith"}) =
	    [{first="Fred1", last="Smith", middle="W"}]
val test4_2 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred1","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"}]

(* ----- Problem 2 ----- *)

val test5_0 = card_color (Clubs, Num 2) = Black
val test5_1 = card_color (Clubs, Num 4) = Black
val test5_2 = card_color (Spades, Num 2) = Black
val test5_3 = card_color (Diamonds, Num 2) = Red
val test5_4 = card_color (Hearts, Num 2) = Red

val test6_0 = card_value (Clubs, Num 2) = 2
val test6_1 = card_value (Spades, Num 9) = 9
val test6_2 = card_value (Diamonds, Ace) = 11
val test6_3 = card_value (Diamonds, King) = 10
val test6_4 = card_value (Diamonds, Queen) = 10
val test6_5 = card_value (Diamonds, Jack) = 10

val test7_0 = remove_card ([(Hearts, Ace),(Diamonds, Ace)], (Diamonds, Ace), IllegalMove) = [(Hearts, Ace)]
val test7_1 = remove_card ([(Hearts, Ace),(Diamonds, Ace)], (Hearts, Ace), IllegalMove) = [(Diamonds, Ace)]
val test7_2 = remove_card ([(Hearts, Ace),(Diamonds, Ace),(Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Diamonds, Ace),(Hearts, Ace)]
(*val test7_3 = remove_card ([(Diamonds, Ace)], (Hearts, Ace), IllegalMove)
val test7_4 = remove_card ([], (Hearts, Ace), IllegalMove) = IllegalMove*)

val test8_0 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test8_1 = all_same_color [(Hearts, Ace), (Diamonds, Ace)] = true
val test8_2 = all_same_color [(Hearts, Ace)] = true
val test8_3 = all_same_color [(Hearts, Ace), (Diamonds, Num(4))] = true
val test8_4 = all_same_color [(Hearts, Ace), (Spades, Num(4))] = false
val test8_5 = all_same_color [] = true

val test9_0 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test9_1 = sum_cards [] = 0
val test9_2 = sum_cards [(Clubs, Ace),(Clubs, Num 2)] = 13
val test9_3 = sum_cards [(Clubs, King),(Clubs, Queen)] = 20

val test10_0 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test10_1 = score ([(Hearts, Num 2),(Clubs, Ace)],10) = 9
val test10_2 = score ([(Hearts, Num 6),(Clubs, Num 4)],10) = 0
val test10_3 = score ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2

val test11_0 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

(*
val test12_0 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13_0 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false)
              handle IllegalMove => true)
*)
