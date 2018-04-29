(* Benoit D'Incau, Coursera PL, HW1 Code *)

(* val year: int is positive *)
(* val month: int between [1,12] *)
(* val day: int between [1,31] depending of the month *)
(* val date : (int * int * int) = (year, month, day) *)

(* val is_older = fn : (int * int * int) * (int * int * int) -> bool *)
(* is_older takes two dates *)
(* is_older return true if the first argument is a date that comes before the second argument *)
fun is_older(date1: int * int * int, date2: int * int * int) =
  (#1 date1 < #1 date2) orelse
  ((#1 date1 = #1 date2) andalso (#2 date1 < #2 date2)) orelse
  ((#1 date1 = #1 date2) andalso (#2 date1 = #2 date2) andalso (#3 date1 < #3 date2))

(* val number_in_month = fn : ((int * int * int) list) * int -> int *)
(* number_in_month takes a list of dates and a month (i.e., an int) *)
(* number_in_month returns how many dates in the list are in the given month. *)
fun number_in_month(dates_list: (int * int * int) list, month: int) =
  if null dates_list
  then 0
  else
    let
      val tl_number_in_month :int = number_in_month(tl dates_list, month)
    in
      if (#2 (hd dates_list)) = month
      then 1 + tl_number_in_month
      else tl_number_in_month
    end

(* val number_in_months = fn : ((int * int * int) list) * (int list) -> int *)
(* number_in_months takes a list of dates and a list of months (i.e., an int list) *)
(* number_in_months returns the number of dates in the list of dates that are in any of the months in the list of months *)
fun number_in_months(dates_list: (int * int * int) list, months_list: int list) =
  if null months_list
  then 0
  else number_in_month(dates_list, hd months_list) + number_in_months(dates_list, tl months_list)

(* val dates_in_month = fn : ((int * int * int) list) * int -> (int * int * int) list *)
(* dates_in_month takes a list of dates and a month (i.e., an int) *)
(* dates_in_month returns a list holding the dates from the argument list of dates that are in the month. *)
fun dates_in_month(dates_list: (int * int * int) list, month: int) =
  if null dates_list
  then []
  else
    let val tl_dates_in_month :(int * int * int) list = dates_in_month(tl dates_list, month)
    in
      if (#2 (hd dates_list)) = month
      then (hd dates_list) :: tl_dates_in_month
      else tl_dates_in_month
    end

(* val dates_in_months = fn : ((int * int * int) list) * (int list) -> (int * int * int) list *)
(* dates_in_months takes a list of dates and a list of months (i.e., an int list) *)
(* dates_in_months returns a list holding the dates from the argument list of dates that are in any of the months in the list of months. *)
fun dates_in_months(dates_list: (int * int * int) list, months_list: int list) =
  if null months_list
  then []
  else dates_in_month(dates_list, hd months_list) @ dates_in_months(dates_list, tl months_list)

(* val get_nth = fn : string list * int -> string *)
(* get_nth takes a list of strings and an int n *)
(* get_nth returns the nth element of the list where the head of the list is 1st *)
fun get_nth(strings_list: string list, n: int) =
  if n = 1
  then hd strings_list
  else get_nth(tl strings_list, n - 1)

(* val date_to_string = fn : (int * int * int) -> string *)
(* date_to_string takes a date *)
(* date_to_string returns a string of the form January 20, 2013*)
fun date_to_string(date: (int * int * int)) =
  let
    val months: string list = ["January", "February", "March", "April", "May", "June", "July",
                "August", "September", "October", "November", "December"]
  in
    get_nth(months,#2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

(* val number_before_reaching_sum = fn : int * (int list) -> int *)
(* number_before_reaching_sum takes an int called sum, which you can assume is positive, and an int list *)
(* number_before_reaching_sum returns an int n such that the first n elements of the list add to less than sum, but the first n + 1 elements of the list add to sum or more. *)
fun number_before_reaching_sum(sum: int,ints_list : int list) =
  if hd ints_list >= sum
  then 0
  else 1 + number_before_reaching_sum(sum - (hd ints_list), tl ints_list)

(* val what_month = fn : int -> int *)
(* what_month takes a day of year (i.e., an int between 1 and 365) *)
(* what_month returns what month that day is in (1 for January, 2 for February, etc.) *)
fun what_month(day: int) =
  let
    val days_in_month: int list = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(day, days_in_month) + 1
  end

(* val month_range = fn : int * int -> int list *)
(* month_range takes two days of the year day1 and day2 *)
(* month_range returns an int list [m1,m2,...,mn] where m1 is the month of day1, m2 is the month of day1+1, ..., and mn is the month of day day2. *)
fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

(* val oldest = fn : (int * int * int) list -> (int*int*int) option) *)
(* oldest takes a list of dates *)
(* oldest evaluates to an (int * int * int) option. It evaluates to NONE if the list has no dates and SOME d if the date d is the oldest date in the list. *)
fun oldest(dates_list: (int * int * int) list) =
  if null dates_list
  then NONE
  else
    let
      fun oldest_nonempty (dates_list : (int * int * int) list) =
        if null (tl dates_list)
        then hd dates_list
        else
          let
            val tl_oldest_date: (int * int * int) = oldest_nonempty (tl dates_list)
            val date: (int * int * int) = hd dates_list
          in
            if is_older (date, tl_oldest_date)
            then date
            else tl_oldest_date
        end
    in
    SOME (oldest_nonempty(dates_list))
    end
