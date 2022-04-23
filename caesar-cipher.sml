(*=====================FUNCTIONS======================*)

use "functions.sml";

 (*====================MAIN-FUNCTION==================*)

fun decrypt filename = 
  let
    val english_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    val english_frequencies = [0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.0228,
    0.02015, 0.06094, 0.06966, 0.00153, 0.00772, 0.04025,
    0.02406, 0.06749, 0.07507, 0.01929, 0.00095, 0.05987,
    0.06327, 0.09056, 0.02758, 0.00978, 0.02360, 0.00150,
    0.01974, 0.00074];
    val rot_lengths = consecutive (length (explode english_alphabet)-1); 
    (*list of all rotation lengths*)
    val all_rotations = beast_rotn rot_lengths; 
    (*list of rotation functions*)
    val all_texts = ultra_map all_rotations (explode (readfile filename));
    (*list of all rotated texts*)
    val all_freqs = beast_calc_freqs all_texts;
    (*list of letter frequencies of texts*)
    val all_entropies = beast_entropy english_frequencies all_freqs;
    (*list of entropies of texts*)
    val min_index = minarg all_entropies;
    (*index of the entropies' list minimum*)
  in
    print (implode (get_nth all_texts min_index));
    (*prints the char_list with minimum entropy and returns it as string*)
    print "\n"
  end

