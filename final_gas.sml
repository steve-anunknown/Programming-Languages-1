fun smalltank file =
  let
    fun readInt input =
      case (TextIO.scanStream (Int.scan StringCvt.DEC) input) of
           SOME num => num
         | NONE     => 0

    val inStream = TextIO.openIn file (* create input stream from file *)
    val towns = readInt inStream (* read number of towns *)
    val roads = readInt inStream (* read number of roads *)
    val _ = TextIO.inputLine inStream (* skip next line escape character *)
    val min_gas_towns = Array.array (towns, 0)

    fun max arr = Array.foldl (fn (a,b) => if a>b then a else b) (Array.sub (arr,0)) arr
    fun readRoads 0 _ _ _ _= min_gas_towns
      | readRoads numRoads from to weight _ =
      (case ((Array.sub (min_gas_towns,from)=0 orelse weight<Array.sub (min_gas_towns,from)),
       (Array.sub (min_gas_towns,to)=0 orelse weight<Array.sub (min_gas_towns,to))) of
            (false,false) => ()
          | (false, true) => Array.update (min_gas_towns,to,weight)
          | (true, false) => Array.update (min_gas_towns,from,weight)
          | (true, true) => Array.update (min_gas_towns,to,weight); Array.update(min_gas_towns,from,weight);
             readRoads (numRoads-1) (readInt inStream -1) (readInt inStream -1) (readInt inStream) (TextIO.inputLine inStream))
  in
    print (Int.toString (max (readRoads roads (readInt inStream -1) (readInt inStream -1) (readInt inStream) (TextIO.inputLine inStream))) ^ "\n") before TextIO.closeIn inStream
  end
