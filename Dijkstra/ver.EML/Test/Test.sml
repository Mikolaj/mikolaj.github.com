(********)
(* Test *)
(********)


structure MultiChar = MultiChar ()

structure CharList = CharList ( structure MultiChar' = MultiChar )

structure Linker = Linker ( structure CharList' = CharList

			    structure MultiChar' = MultiChar )

structure String2MultiChar = String2MultiChar ( structure MultiChar' = Linker.MultiChar 

						structure CharList' = Linker.CharList )
	  
fun file2string name = let

			   fun stream2string (stream, close_proc) = 
			       if end_of_stream stream
				   then (close_proc stream; "")
			       else let 
					val c = input(stream, 1)
				    in
					c^(stream2string (stream, close_proc))
				    end
		       in

			   stream2string (open_in name, close_in)

		       end

fun c name = Linker.compile (String2MultiChar.string2mchar (file2string name))

fun t (Linker.OkError_parse.OK(Linker.OkError_elab.OK(tau, f))) = f

fun s (Linker.Post.GOING(sble)) = Linker.Storable.sto2functional sble

(*  

c "dij/square_root.dij"; val f = t it; 

f (Linker.Storable.integer2sto (Linker.Storable.Integer.int2integer 26));

c "dij/Prime_factor.dij"; val f = t it;

f (Linker.Storable.integer2sto (Linker.Storable.Integer.int2integer 51));

c "dij/Hamming.dij"; val f = t it;

f (Linker.Storable.integer2sto (Linker.Storable.Integer.int2integer 30)); val g = s it;

g (Linker.Storable.integer2sto (Linker.Storable.Integer.int2integer 8));

g (Linker.Storable.integer2sto (Linker.Storable.Integer.int2integer 0));

g (Linker.Storable.integer2sto (Linker.Storable.Integer.int2integer ~17));
    
*)







