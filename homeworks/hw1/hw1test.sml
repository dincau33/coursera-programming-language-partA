(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw1.sml";

val test0_0 = is_older ((1,2,3),(2,3,4)) = true
val test0_1 = is_older ((1,1,3),(1,2,3)) = true
val test0_2 = is_older ((1,2,2),(1,2,3)) = true
val test0_3 = is_older ((2,3,4),(1,2,3)) = false
val test0_4 = is_older ((1,2,3),(1,2,3)) = false
val test0_5 = is_older ((1,2,3),(1,1,3)) = false
val test0_6 = is_older ((1,2,3),(1,2,2)) = false

val test1_0 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test1_1 = number_in_month ([(2012,2,28),(2013,2,1)],2) = 2
val test1_2 = number_in_month ([(2012,1,28),(2013,4,1)],2) = 0
val test1_3 = number_in_month ([(2012,2,28),(2013,2,1),(2015,2,28),(2013,12,1),(2016,2,28),(2017,2,1)],2) = 5
val test1_4 = number_in_month ([],2) = 0
val test1_5 = number_in_month ([(2012,2,28)],2) = 1
val test1_6 = number_in_month ([(2012,2,28)],1) = 0

val test2_0 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test2_1 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[5,6,9]) = 0
val test2_2 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val test2_3 = number_in_months ([],[]) = 0
val test2_4 = number_in_months ([(2012,2,28),(2013,2,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 4

val test3_0 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test3_1 = dates_in_month ([(2012,2,28),(2013,12,1)],3) = []
val test3_2 = dates_in_month ([(2012,2,28),(2013,2,1)],2) = [(2012,2,28),(2013,2,1)]
val test3_3 = dates_in_month ([(2012,2,28),(2013,3,1),(2013,2,1)],2) = [(2012,2,28),(2013,2,1)]
val test3_4 = dates_in_month ([],3) = []

val test4_0 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test4_1 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[5,6,7]) = []
val test4_2 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = []
val test4_3 = dates_in_months ([],[]) = []
val test4_5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,12]) = [(2012,2,28),(2011,3,31),(2011,4,28),(2013,12,1)]

val test5_0 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test5_1 = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi"
val test5_2 = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"
val test5_3 = get_nth (["hi"], 1) = "hi"

val test6_0 = date_to_string (2013, 1, 1) = "January 1, 2013"
val test6_1 = date_to_string (2013, 2, 1) = "February 1, 2013"
val test6_2 = date_to_string (2013, 3, 1) = "March 1, 2013"
val test6_3 = date_to_string (2013, 4, 1) = "April 1, 2013"
val test6_4 = date_to_string (2013, 5, 1) = "May 1, 2013"
val test6_5 = date_to_string (2013, 6, 1) = "June 1, 2013"
val test6_6 = date_to_string (2013, 7, 1) = "July 1, 2013"
val test6_7 = date_to_string (2013, 8, 1) = "August 1, 2013"
val test6_8 = date_to_string (2013, 9, 1) = "September 1, 2013"
val test6_9 = date_to_string (2013, 10, 1) = "October 1, 2013"
val test6_10 = date_to_string (2013, 11, 1) = "November 1, 2013"
val test6_11 = date_to_string (2013, 12, 1) = "December 1, 2013"

val test7_0 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test7_1 = number_before_reaching_sum (2, [1,2,3,4,5]) = 1
val test7_2 = number_before_reaching_sum (14, [1,2,3,4,5]) = 4

val test8_0 = what_month 70 = 3
val test8_1 = what_month 2 = 1
val test8_2 = what_month 365 = 12
val test8_3 = what_month 36 = 2
val test8_4 = what_month 320 = 11

val test9_0 = month_range (31, 34) = [1,2,2,2]
val test9_1 = month_range (31, 31) = [1]
val test9_2 = month_range (23, 24) = [1,1]
val test9_3 = month_range (23, 25) = [1,1,1]
val test9_4 = month_range (27, 25) = []

val test10_0 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test10_1 = oldest([(2012,2,28)]) = SOME (2012,2,28)
val test10_2 = oldest([(2010,2,28),(2011,3,31),(2011,4,28)]) = SOME (2010,2,28)
val test10_3 = oldest([(2014,2,28),(2015,3,31),(2011,4,28)]) = SOME (2011,4,28)
val test10_4 = oldest([]) = NONE
