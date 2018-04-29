(* Homework0 Simple Test *)

(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw0.sml";

val test1_0 = double 17 = 34
val test1_1 = double 0 = 0

val test2_0 = triple ~4 = ~12
val test2_1 = triple 0 = 0

val test0_1 = f(12,27) = 324

(* You can add more tests here, for example you can uncomment the line below
by deleting the first two character and last two characters on the line *)

val test2_2 = triple ~1 = ~3
