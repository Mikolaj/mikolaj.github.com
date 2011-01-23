(**********)
(* Linker *)
(**********)


signature LINKER =
sig

    structure CharList : CHAR_LIST

    structure MultiChar : MULTI_CHAR
	  
    sharing CharList.MultiChar = MultiChar

    structure OkError_elab : OK_ERROR

    structure CharInfo : CHAR_INFO

    sharing OkError_elab.CharInfo = CharInfo

    structure ElaboratorError : ELABORATOR_ERROR

    sharing OkError_elab.Error = ElaboratorError

    structure NameSet : NAME_SET

    sharing ElaboratorError.NameSet = NameSet

    structure Ttype : TTYPE
     
    sharing ElaboratorError.Ttype = Ttype

    structure OkError_parse : OK_ERROR

    sharing OkError_parse.CharInfo = CharInfo

    structure ParserError : PARSER_ERROR

    sharing OkError_parse.Error = ParserError

    structure Tokens : TOKENS

    sharing ParserError.Tokens = Tokens

    structure Post : POST

    structure Storable : STORABLE

    sharing Storable.Post = Post

    val compile : CharList.char_list -> 
	(Ttype.ttype * (Storable.storable -> Storable.storable Post.post)) OkError_elab.ok OkError_parse.ok

end