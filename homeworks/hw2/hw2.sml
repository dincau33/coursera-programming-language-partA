(* Benoit D'Incau, Coursera PL, HW2 Code *)

(* ----- Problem 1 ----- *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)

(* val same_string = fn : string * string -> bool *)
(* same_string takes 2 strings *)
(* same_string returns true if the 2 strings match and false if not *)
fun same_string(string1 : string, string2 : string) =
    string1 = string2

(* put your solutions for problem 1 here *)

(* val all_except_option = fn : string * string -> bool *)
(* all_except_option takes a string and a string list *)
(* all_except_option returns NONE if the string is not in the list, else return SOME lst where lst is identical to the argument list except the string is not in it *)
fun all_except_option(string_to_match: string, [] : string list) = NONE
  | all_except_option(string_to_match: string, current_string::strings_list : string list) =
      case all_except_option(string_to_match,strings_list) of
        NONE => if same_string(string_to_match,current_string) then SOME(strings_list) else NONE
      | SOME(strings_list') => if same_string(string_to_match,current_string) then SOME(strings_list) else SOME(current_string::strings_list')

(* val get_substitutions1 = fn : string list list * string -> bool *)
(* get_substitutions1 takes a string list list (a list of list of strings, the substitutions) and a string s *)
(* get_substitutions1 returns a string list. The result has all the strings that are in some list in substitutions that also has s, but s itself should not be in the result *)
fun get_substitutions1([]: string list list, string_to_match: string) = []
  | get_substitutions1(potential_substitutions_list::potential_substitutions_lists: string list list, string_to_match: string) =
      let val tl_get_substitutions1 = get_substitutions1(potential_substitutions_lists,string_to_match)
      in
        case all_except_option(string_to_match,potential_substitutions_list) of
          NONE => tl_get_substitutions1
        | SOME(substitutions_list) => substitutions_list @ tl_get_substitutions1
      end

(* val get_substitutions2 = fn : string list list * string -> bool *)
(* get_substitutions2 takes a string list list (a list of list of strings, the substitutions) and a string s *)
(* get_substitutions2 returns a string list. The result has all the strings that are in some list in substitutions that also has s, but s itself should not be in the result *)
fun get_substitutions2(potential_substitutions_lists: string list list, string_to_match: string) =
  let fun aux([]:string list list, substitutions_list: string list) = substitutions_list
        | aux(potential_substitutions_list::potential_substitutions_lists:string list list, substitutions_list: string list) =
            case all_except_option(string_to_match,potential_substitutions_list) of
              NONE => aux(potential_substitutions_lists,substitutions_list)
            | SOME(substitutions_list') => aux(potential_substitutions_lists,substitutions_list @ substitutions_list')
  in
    aux(potential_substitutions_lists,[])
  end

(* val similar_names = fn : string list list * {first:string,middle:string,last:string} -> {first:string,middle:string,last:string} list *)
(* similar_names which takes a string list list of substitutions and a full name of type {first:string,middle:string,last:string} *)
(* similar_names returns a list of full names (type {first:string,middle:string,last:string} list). The result is all the full names you can produce by substituting for the first name (and only the first name) using substitutions and parts (b) or (c). The answer should begin with the original name (then have 0 or more other names) *)
fun similar_names(potential_substitutions_lists: string list list, {first = first_name: string, middle = middle_name: string, last = last_name: string}) =
  let
    fun full_name_lists ([]: string list) = []
      | full_name_lists (first_name'::substitutions_list': string list) = {first = first_name',middle = middle_name,last = last_name} :: full_name_lists(substitutions_list')
  in
    {first = first_name, middle = middle_name, last = last_name} :: full_name_lists(get_substitutions2(potential_substitutions_lists, first_name))
  end

(* ----- Problem 2 ----- *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

(* val card_color = fn : card -> color *)
(* card_color takes card *)
(* card_color returns its color *)
fun card_color (selected_card: card) =
  case selected_card of
    (Diamonds,_) => Red
  | (Hearts,_) => Red
  | _ => Black

(* val card_value = fn : card -> int *)
(* card_value takes card *)
(* card_value returns its value *)
fun card_value (selected_card: card) =
  case selected_card of
    (_,Num(value)) => value
  | (_,Ace) => 11
  | _ => 10

(* val remove_card = fn : card list * card * exp -> card list  *)
(* remove_card takes takes a list of cards cs, a card c, and an exception e. *)
(* remove_card returns a list that has all the elements of cs except c. If c is in the list more than once, remove only the first one. If c is not in the list, raise the exception e *)
fun remove_card ([]: card list, card_to_remove: card, e: exn) = raise e
  | remove_card (first_card::cards_list: card list, card_to_remove: card, e: exn) =
      if first_card = card_to_remove
      then cards_list
      else first_card::remove_card(cards_list, card_to_remove, e)

(* val all_same_color = fn : card list -> boolean *)
(* all_same_color takes list of cards *)
(* all_same_color returns true if all the cards have the same color *)
fun all_same_color ([]: card list) = true
  | all_same_color (_::[]: card list) = true
  | all_same_color (first_card::second_card::cards_list: card list) = (card_color(first_card) = card_color(second_card)) andalso all_same_color(second_card::cards_list)

(* val sum_cards = fn : card list -> int *)
(* sum_cards takes list of cards *)
(* sum_cards returns the sum of the cards value *)
fun sum_cards (cards_list: card list) =
  let
    fun aux([]: card list, sum: int) = sum
      | aux(first_card::cards_list': card list, sum: int) = aux(cards_list', card_value(first_card) + sum)
  in
    aux(cards_list,0)
  end

(* val score = fn : card list * int -> int *)
(* score takes list of cards and the goal *)
(* score returns the score *)
fun score (cards_list: card list, goal: int) =
  let
    fun compute_preliminary_score(sum: int, goal: int) =
      if sum > goal
      then (sum - goal) * 3
      else goal - sum
    val preliminary_score: int = compute_preliminary_score(sum_cards(cards_list), goal)
  in
    if all_same_color(cards_list)
    then preliminary_score div 2
    else preliminary_score
  end

(* val officiate = fn : card list * move list * int  -> int *)
(* officiate takes a list of cards, a move list and the goal *)
(* officiate returns the score at the end of the game after processing (some or all of) the moves in the move list in order *)
fun officiate (cards_list: card list, moves_list: move list, goal: int) =
  let
    fun play_game (cards_list, moves_list, cards_held_list) =
      case moves_list of
        [] => score (cards_held_list, goal)
      | move::remaining_moves_list =>
          case move of
            Discard selected_card => play_game (cards_list, remaining_moves_list,
                                  remove_card (cards_held_list, selected_card, IllegalMove))
          | Draw => case cards_list of
                      [] => score (cards_held_list, goal)
                    | drew_card::remaining_cards_list =>
                        let val new_cards_held_list = drew_card::cards_held_list
                        in
                          if sum_cards(new_cards_held_list) > goal
                          then score (new_cards_held_list, goal)
                          else play_game (remaining_cards_list, remaining_moves_list, new_cards_held_list)
                        end
  in
    play_game(cards_list, moves_list, [])
  end
