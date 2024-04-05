(* problem 1, this uses the found boolean value to determine whether it has matched, if not returns NONE else it returns SOME without the target, the found variables state is essentially stored in the stack, once the base case is reached this is how it know to append or not. Also the initial call "remove(false, lst)" means that within the functions logic is found = false, which becomes its state before each recursive call *)

fun all_except_option(target, lst) =
    let
        fun remove(found, []) = if found then SOME([]) else NONE
          | remove(found, x::xs) =
            if x = target then
                remove(true, xs)
            else
                case remove(found, xs) of
                     NONE => NONE
                   | SOME(rest) => SOME(x::rest)
    in
        remove(false, lst)
    end;

(* problem 2 this function loops through, using the List.exist standard lib function that return true if an item in x is equal to s then a recursive call that call all_except options > which removes non matches and then appends and calls the base function till it hits the base case to where it will return acc  *)
fun get_substitutions1(substitutions, s) =
    let
        fun append_option(NONE, acc) = acc
          | append_option(SOME(lst), acc) = lst @ acc

        fun process_substitutions([], acc) = acc
          | process_substitutions(x::xs, acc) =
            if List.exists (fn item => item = s) x then
                process_substitutions(xs, append_option(all_except_option(s, x), acc))
            else
                process_substitutions(xs, acc)
    in
        process_substitutions(substitutions, [])
    end;

(* problem 3, the newAcc acts as a tail recursive element, if there is no match within x (NONE) then acc is returned back in recursive call, else lst is appended and stored in newACC, the list needs to be reversed once base case is hit to go back to the original order *)					      
fun get_substitutions2(substitutions, s) =
    let
        fun process_substitutions_tail([], acc) = List.rev(acc)
          | process_substitutions_tail(x::xs, acc) =
            let
                val newAcc = case all_except_option(s, x) of
                                 NONE => acc 
                               | SOME(lst) => lst @ acc  
            in
                process_substitutions_tail(xs, newAcc)  
            end
    in
        process_substitutions_tail(substitutions, [])  
    end; 

(* problem 4 the subFirst Names uses a previous function to get the non unique names with list list of names, a helper function defined in the second let uses the subFirst Names (if returns) [] or x::xs then recursives calls items in output of subfirsNames whilst appending the full names to another list,*)

fun similar_names (substitutions, {first = firstname, middle = middlename, last = lastname}) =
    let
	val subFirst = get_substitutions2 (substitutions, firstname)
    in
	let
	    fun substituteNames (subFirstNameList, allNamesList) =
		case subFirstNameList of
		    [] => allNamesList
		  | x :: x' => substituteNames (x', {first = x, middle = middlename, last = lastname} :: allNamesList)
	in
	    substituteNames (subFirst, [{first = firstname, middle = middlename, last = lastname}])
	end
    end

				 
(* problem 5 *)
	
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype color = Red | Black
datatype rank = Num of int | Ace | Jack | Queen | King
exception IllegalMove
type card = suit * rank
datatype move = Discard of card | Draw

fun card_color(color, _) =
    case color of
	Clubs => Black
      | Spades => Black
      | Diamonds => Red
      | Hearts => Red
			
fun card_value(_, value) =
    case value of
	Num x => x
      | Jack => 10
      | Queen => 10
      | King => 10
      | Ace => 11

(* this waits till the first occurance of the match, then appends the past cards to the current cards *)
		   
fun remove_card(cs, c, e) =
    let
	fun remove_c_sub(currentCards, pastCards) =
	    case currentCards of
		[] => raise e
	      | x :: x' => if x = c
			   then pastCards @ x'
			   else remove_c_sub(x', x :: pastCards)
    in
	remove_c_sub(cs, [])
    end

(* this looks for common color, fcolor is the initial setting and is defined as only the first items color (as it is not recalled with xs), the helper function checks if restoflist is empty, in which case it returns true (ie. the list was same color) else it gets the color of the head of the list and compares to common color, if no match it returns false (the loop is ended immediately in this case) *) 

fun all_same_color lst =
    let
	val fcolor = case lst of [] => Red 
			      | x :: x' => card_color x
	fun helpy (restOfList, commonColor) =
	    case restOfList of
		[] => true
	      | x :: x' => if card_color (x) = commonColor
			   then helpy (x', commonColor)
			   else false
    in
	helpy (lst, fcolor)
    end;

(* function to sum cards in list using tail recursion and card_value function *)		   
fun sum_cards(list) =
    let
	fun helpy([], acc) = acc
	  | helpy(x::xs, acc) = helpy(xs, card_value(x) + acc)
    in
	helpy(list, 0)
    end;

(* straight forward *) 
	
fun score (lst, goal) = 
    let
	val value = sum_cards lst
	val pre_score = if value > goal 
			then 3 * (value - goal) 
			else (goal - value)
    in
	if all_same_color lst
	then pre_score div 2
	else pre_score
    end 

(* screenshot> datatype definition *) 
	
fun officiate (cardlist, movelist, goal) =
    let
	fun helpy1 (cardlist, movelist, heldcards) =
	    if sum_cards heldcards > goal
	    then score(heldcards, goal)
	    else case movelist of
		     [] => score(heldcards, goal)
		   | x::xs => case x of
				  Discard i => helpy1(cardlist, xs, remove_card(heldcards, i, IllegalMove))
			       |  Draw => case cardlist of
					      [] => score(heldcards, goal)
					    | y::ys => helpy1(ys, xs, y::heldcards)
    in
	helpy1(cardlist, movelist, [])
    end; 
