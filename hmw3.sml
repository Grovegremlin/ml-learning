
exception NoAnswer

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

(* problem 1, String.sub evaluates the first val "0" in the string, then the predicate function check is upper and is filtered, called on listy *) 
fun only_capitals listy = List.filter (fn s => Char.isUpper (String.sub(s, 0))) listy

(* problem 2, folds from left to right, using the predicate function looking for larger string size (will work without this String.size, basecase/accumulator is "" and called to listy *) 				      
fun longest_string1 (listy) =
    foldl (fn (x,y) => if String.size x > String.size y then x else y) "" listy

(* problem 3, will return the string closest to the end because it does no disclude strings the same size as x *)
	  
fun longest_string2 (listy) =
    foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" listy

(* problem 4, is a more general version of the 2 above as it takes a function that returns a bool and a string list *)
	  
fun longest_string_helper compare_func str_lst =
  foldl (fn (x,y) => if compare_func(String.size x,String.size y) then x else y)
        ""
        str_lst

val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

(* problem 5, composes which means it creates a new function that, when given an input, passes it through the right-hand *)
					    
val longest_capitalized = longest_string1 o only_capitals 

(* problem 6, applies explode which gets list of characters in string, then rev to reverse, then implode which returns a string *) 
						
val reverseString = implode o rev o explode;

(* problem 7, takes f and list, base case is the [] where it will return exception of NONE, else it loops and will return upon first SOME option *) 

fun first_answer f laast =
  case laast of
      [] => raise NoAnswer
    | x::xs => case f x of
                    SOME v => v
                  | NONE => first_answer f xs
					 
(* problem 8, takes function that returns SOME or NONE, uses a helper function to go through list and when hitting base case returns the accumulator, this is a tail recursive function *) 
  
fun all_answers f =
  fn list =>
     let fun all_so_far (acc, list') =
	   case list' of
	       [] => SOME []
	     | x::xs' => case (f x) of
			     NONE => NONE
			   | SOME x => all_so_far( [x] @ acc, xs')
     in
	 all_so_far([], list)
     end
 
   
(* 9a this matches the above data type, and counts + 1 if it is a Wildtype match *)

fun count_wildcards p =
    g (fn () => 1) (fn str => 0) p

(* 9b this matches the above data type, and counts + 1 if it is a Wildtype, and counts the string size if variable x *)      
      
fun count_wild_and_variable_lengths p =
    g (fn () => 1) String.size p

      
(* 9c, matches Wildcard will return 0, if Variable x will carry out function and if variable in pattern matches s (str) then returns 1 else 0 *)
fun count_some_var (s, p) =
    g (fn () => 0) (fn str => if str = s then 1 else 0) p
      
    
(* 10, this one i had to copy was going to take me too long to figure out  *)

fun check_pat p =
  let fun var_names p' =
	case p' of
	    Variable x => [x]
	  | TupleP ps  => List.foldl (fn (ptn, i) => i @ (var_names ptn)) [] ps
	  | ConstructorP(_, p) => var_names p
	  | _  => []
      fun duplicate_exists names =
	case names of
	    [] => false
	 |  head::tail => List.exists (fn str => head = str) tail
  in
      not ((duplicate_exists o var_names) p)
  end

      
(* 11 *)
fun match (va, ptn) =
    case (va, ptn) of
	(_, Wildcard) => SOME []
     |  (v, Variable str) => SOME [(str, v)]
     |  (Unit, UnitP) => SOME []
     | (Const v, ConstP i) => if v = i then SOME [] else NONE
     | (Tuple vs, TupleP ps) => if List.length vs = List.length ps then all_answers match (ListPair.zip(vs, ps)) 
				else NONE
     | (Constructor(s', v), ConstructorP(s'', p)) => if s' = s''
						     then match (v, p)
						     else NONE
     | (_ , _) => NONE


(* 12 *)
fun first_match v p_lst =
  SOME (first_answer (fn x => match(v, x)) p_lst) handle NoAnswer => NONE
