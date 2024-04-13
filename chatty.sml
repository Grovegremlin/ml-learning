
(*  creating a map function. I was realised that i was making a filter function my first attempt (after insight from chatgpt), the map function applies the function directly to input *) 
fun map (f, x) =
    let
        fun helpy (_, []) = []
          | helpy (f, x::xs) = (f x) :: helpy(f, xs)
    in
        helpy(f, x)
    end;

			    		   
val cool = map((fn x => x mod 2 = 0), [1,2,3,4,5,6])       
       
(* In the first call of the function the output is z => 9 + z, then when calling again with (val z = g 6) the z is substituted for 6 and then the function equates to 15 *)
val x = 1
fun f y = 
    let 
        val x = y+1
    in
        fn z => x + y  + z
    end
val x = 3
val g = f 4 (* z => 9 + z *) 
val y = 5
val z = g 6 (* z = 9 + 6 *) 

(*this is another example of lexical scope *) 	  

fun f g = 
    let 
        val x = 3
    in
        g 2
    end
val x = 4
fun h y = x + y 
val z = f h (* this equates to 6 because f is being called (the top function) with function h so it demonstrates the in function scope *) 	  

(* created a find max function that returns an option, I initially created it where the recursive function was using the x::xs pattern match, this can work if you use the #index notation, and ends up being less efficient, this is also tail recursive as it requires no further computation after the base case is reached *) 	  

fun find_max (lst) =
    case lst of
	[] => NONE
      | x::xs => let
	  fun helpy1(words, max) =
	    case words of 
		[] => SOME(max)
	      | y::ys => if y > max then helpy1(ys, y)
			 else helpy1(ys, max)
    in
	helpy1(xs, x)

      end;


(* working towards a quick sort function, this will involve partitioning, and then comparing values in seperate lists, the optimal situation seems to be where the lists are of similar lenghts *)
(* need a way to seperate into list of int list, i am thinking use counter and append up to half of the counter *)
(* attempt 1, appends incorrectly and inefficiently *) 
(*
fun partition (lst) =
    let
	val length = (length lst) div 2
	fun helpy([], a, b, counter) = [a, b]
	  | helpy(x::xs, a, b, counter) = if counter > length then
						  helpy(xs, a @ x, b, 1 + counter)
					     else
						 helpy(xs, a, b @ x, 1 + counter)
    in
	helpy(lst, [], [], 0)
    end;
*)  

(* implemented different appending method, and have to reverse order to maintain "original" order *) 
fun partition (lst) =
    let
        val length = (List.length lst) div 2
        fun helpy([], a, b, counter) = (List.rev a, List.rev b)
          | helpy(x::xs, a, b, counter) = 
                if counter < length then
                    helpy(xs, x::a, b, counter + 1)
                else
                    helpy(xs, a, x::b, counter + 1)
    in
        helpy(lst, [], [], 0)
    end;

(*
fun quick_sort [] = []
  | quick_sort (x::xs) =
    let
        val (left, right) = partition (x, xs)
    in
        quick_sort left @ [x] @ quick_sort right
    end;
*)

(* this merges 2 lists, i wasnt sure why i couldnt use wild card pattern matching for the top 2 base cases, but that was more of a curiosity thing, this probably could have been made alot more concise *) 

fun merge (list1, list2) =
    let
	fun helpy([], ys) = ys
	  | helpy(xs, []) = xs
	  | helpy(x::xs, y::ys) = if y < x then
				      y :: helpy(x::xs, ys)
				  else x :: helpy(xs, y::ys)
    in
	helpy(list1, list2)
    end;

(* "deep_reversion" can reverse a' list list > a' list list and reverse order of all layers of the list of list *) 

fun deep_reversion (listyy) =
    let
	fun helpy([], acc) = acc
	  | helpy(x::xs, acc)  = let val reversedList = List.rev x
				 in helpy(xs, reversedList::acc)
				 end;				      
    in
	helpy(listyy, [])
    end;
		     
(* Tail-recursive Fibonacci generator using an accumulator *)

fun fibbonaci n =
    let
	fun helpy(a, b, 0, acc) = List.rev (acc)
	  | helpy(a, b, count, acc) = helpy(b, a+b, count-1, a::acc)
    in
	if n = 0 then []
	else if n = 1 then [0]
	else helpy(0, 1, n, [])
    end;
	
  
    
