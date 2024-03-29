(*=====================USEFUL-FUNCTIONS=====================*)

(* calculates the length of list. int list -> list *)
fun length l = if (null l) then 0 else (1+ length (tl l))

(* returns the nth element of list. int -> int list -> int *)
fun get_nth (head::tail) index = if index = 0 then head else get_nth tail (index-1)

(* returns the minimum element of real list. real list -> real *)
fun minlist l :real = foldl (fn (a,b) => if a<b then a else b) (hd l ) (tl l);

(* returns the index of the minimum element of real list. real list -> int *)
fun minarg nil = ~1
  | minarg (head::tail) = if (null tail orelse head < minlist tail) then 0 else 1 + minarg tail  

(*constructs list of consecutive elements from num to zero. int -> int list*)
fun consecutive 0 = [0]
  | consecutive num = (num)::(consecutive (num-1)) 

(*takes a list of functions and a list of parameters and returns a list of
 * functions to be executed with these parameters *)
fun ultra_map nil nil = nil
  | ultra_map nil _ = nil
  | ultra_map _ nil = nil
  | ultra_map func_list char_list = [map (hd func_list) char_list]@(ultra_map (tl func_list) char_list)

(* returns a list of rotation functions, one for each rotation length*)
fun beast_rotn num_list = 
  let 
    fun rotn number character =
      let
        fun isupper c = (65 <= ord c) andalso (ord c <= 90)
        fun islower c = (97 <= ord c) andalso (ord c <= 122)
        fun ischar c = (isupper c) orelse (islower c)  
      in
        case (ischar character, isupper character, (ord character) - number < ord #"A",(ord character) - number < ord #"a") of
             (false, _, _, _) => character |
             (true, true, true, _) => chr (ord character - number + ord #"Z" - ord #"A" + 1)|
             (true, true, false, _) => chr (ord character - number )| 
             (true, false, _, true) => chr (ord character - number + ord #"z" - ord #"a" +1)|
             (true, false, _, false) =>  chr (ord character - number )
      end
  in
    map (fn num => rotn num) num_list
  end

(*calculates the frequency of each letter in given texts*)
fun beast_calc_freqs string_list = 
  let 
    fun calc_freqs intext =
      let
        fun isupper c = (65 <= ord c) andalso (ord c <= 90)
        fun islower c = (97 <= ord c) andalso (ord c <= 122)
        fun ischar c = (isupper c) orelse (islower c)
        fun total_chars nil = 0.0
          | total_chars (h::tail) = 
          if ischar h 
          then 
            1.0+(total_chars tail)
          else
            total_chars tail
        fun increment_nth nil _ = nil :real list
          | increment_nth (h::rest) 0 = (h+1.0)::rest
          | increment_nth (h::rest) num = [h]@(increment_nth rest (num-1))
        fun count_occur nil alphabet = alphabet  
          | count_occur (c::tail) alphabet =
          if not (ischar c)
          then
            count_occur tail alphabet
          else
            if isupper c
            then
              count_occur tail (increment_nth alphabet (ord c - 65))
            else
              count_occur tail (increment_nth alphabet (ord c - 97))
        val total = total_chars (explode intext)
        val occur =[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
  in
    map (fn x => x / total) (count_occur (explode intext) occur)
  end
      in     
        map (fn str => calc_freqs str) (map implode string_list)
      end
(* inputs are the normal alphabet frequency and a list of calculated frequencies
 * and output is a list of entropies *)
fun beast_entropy normal real_list_list =
  let
    fun entropy nil nil = 0.0
      | entropy _ nil = 0.0
      | entropy nil _ = 0.0
      | entropy normal current =
      let
        fun listmul nil _ = [0.0] 
          | listmul _ nil = [0.0]
          | listmul (h1::tail1) (h2::tail2) = (h1*h2)::(listmul tail1 tail2);
      in
        ~ (foldr (op +) 0.0 (listmul normal (map Math.ln (map (fn x => x+0.000001) current)))) 
      end;
  in
    map (fn real_list => entropy normal real_list) real_list_list 
  end

(*reads contents from file end returns string. string -> string*)
fun readfile file = 
      let
        val inpstr = TextIO.openIn file
        val text_string = TextIO.inputAll inpstr
        val closed = TextIO.closeIn inpstr
      in
        text_string
      end
