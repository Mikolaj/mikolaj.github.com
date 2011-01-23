(************)
(* Storable *)
(************)


signature STORABLE = 
sig

    structure Integer : INTEGER
  
    structure Boolean : BOOLEAN

    structure Post : POST

    type storable
    
    val integer2sto : Integer.integer -> storable

    val boolean2sto : Boolean.boolean -> storable

    val functional2sto : (storable -> storable Post.post) -> storable

    val sto2functional : storable -> (storable -> storable Post.post) (* unsafe *)

    val sapp : storable * storable -> storable Post.post

    val sneg : storable

    val snot : storable

    val sdiv : storable

    val smod : storable

    val stimes : storable

    val splus : storable

    val sminus : storable 

    val sequal : storable

    val sdiff : storable

    val sless : storable

    val sgreat : storable 

    val seqless : storable

    val seqgreat : storable

    val sand : storable  

    val sor : storable

    val is_storable_false : storable -> bool

    val is_storable_true : storable -> bool

    val storable_false : storable

    val storable_true : storable     
	
end