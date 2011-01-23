(***********)
(* Numeral *)
(***********)


signature NUMERAL = 
sig

    type num

    val num2int : num -> int 

    val int2num : int -> num

end