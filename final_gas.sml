fun min_fill file =
  let
    (* reads an integer *)
    fun readInt input =
      Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)

    val inStream = TextIO.openIn file (* create input stream from file *)
    val towns = readInt inStream (* read number of towns *)
    val roads = readInt inStream (* read number of roads *)
    val _ = TextIO.inputLine inStream (* skip next line escape character *)
   
    (* skip num characters and read the (num+1)th*)
    fun readremain input 0 = readInt input
      | readremain input num = 
      let
        val _ = readInt input
      in
          readremain input (num-1)
      end
    (* read the 3rd character of num number of lines*)  
    fun readlines 0 linelist = linelist
      | readlines num linelist = [readremain inStream 2]@(readlines (num-1) nil)
    (* find maximum element of list*)
    fun max l = foldl (fn (a,b) => if a>b then a else b) (hd l) (tl l) 
  in
    (* read the weight of each road-line
     * merge them into ascending order
     * take the "number of towns" first minima
     * find the maximum element of those
     * convert it to string and print it*)
    print (Int.toString (max (List.take (ListMergeSort.sort (op >) (readlines roads nil), towns))));
    print "\n"
  end

