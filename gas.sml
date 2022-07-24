(* function that takes as input the name of
 * an input file where data regarding a graph
 * is written. The graph represents towns 
 * connected by roads and the weights of the
 * edges represent the necessary gas in order
 * to travel from one town to another. Gas can
 * be refilled only in towns. The function returns
 * the minimum size of the gas deposit in order to 
 * be able to travel from any town everywhere. *)
fun min_fill inputfile =
  let
    (* function that reads an integer from input file stream *)
    fun readInt inputstream =
      case (TextIO.scanStream (Int.scan StringCvt.DEC) inputstream) of
           SOME num => num
         | NONE     => ~1
    val inStream = TextIO.openIn inputfile (* input stream created from file *)
    val towns = readInt inStream (* get number of towns / vertices *)
    val roads = readInt inStream (* get number of rowds / edges *)

    (* function that reads "times" number of lines with 3 integers.
     * It basically reads the data of every edge / road. *)
    fun readIntLines inputstream 0 = nil
      | readIntLines inputstream times =
      (readInt inputstream -1,readInt inputstream -1,readInt inputstream)::(readIntLines
      inputstream (times-1))  
    val edges = ListMergeSort.sort (fn ((_,_,a),(_,_,b)) => a > b) (readIntLines
    inStream roads) (*sorted by weight list of graph's edges in order to perform
    kruskal's algorithm later*)


    (* for disjoint set structure, initialization of parent and rank *)
    val parent = Array.tabulate (towns, (fn a => a))
    val rank = Array.tabulate (towns, (fn _ => 0))

    (* union find functions *) 
    fun find num = 
      if (Array.sub (parent, num) = num) 
      then num 
      else find (Array.sub (parent, num))
    fun merge (num1, num2) =
      let
        val par1 = find(num1)
        val par2 = find(num2)
  in
    case (Array.sub (rank,par1) > Array.sub (rank,par2),
    Array.sub (rank,par1) < Array.sub (rank,par2),
    Array.sub (rank,par1) = Array.sub (rank,par2)) of
         (true,false,false) => Array.update (parent,par2,par1)
       | (false,true,false) => Array.update (parent,par1,par2)
       | (false,false,true) => (Array.update (parent,par1,par2);
          Array.update (rank,par2, Array.sub (rank,par2) + 1))
      end
      (* function that returns the maximum element of a list *) 
    fun max l = foldl (fn (a,b) => if a>b then a else b) (hd l) l
      in
        (* takes the sorted list of edges, checks if the addition of a list
         * in to the minimal spanning tree forms a circle. If it does not, 
         * then the vertices get merged / united and the weight of the
         * edge is returned, otherwise zero is returned. Thus, a list that 
         * contains the weights of the edges of the minimum spanning tree of
         * the graph is returned and, finally, the maximum value is acquired*)
         print (Int.toString (max (map (fn (a,b,c) => if (not (find a = find b)) then
           (merge (find(a),find(b)); c ) else 0 ) edges)) ^ "\n");

            (* close the input stream *)
            TextIO.closeIn inStream 
      end
