(**********)
(* Tokens *)
(**********)


signature TOKENS = 
sig

    structure Identifier : sig type id end

    structure Numeral : sig type num end

    structure Falseval : sig type fal end

    datatype token =
	T_SEMICOLON         
      | T_SKIP              
      | T_ABORT
      | T_IF                
      | T_FI                
      | T_DO                
      | T_OD                
      | T_VIR               
      | T_ASSIGN            
      | T_ARROW            
      | T_TACT               
      | T_BEGIN             
      | T_END               
      | T_PRIVAR            
      | T_PRICON            
      | T_VIRVAR            
      | T_VIRCON            
      | T_GLOVAR            
      | T_GLOCON
      | T_COLON
      | T_TYPE_INT
      | T_TYPE_BOOL            
      | T_NEG               
      | T_NOT 
      | T_DOT              
      | T_LPAREN            
      | T_RPAREN            
      | T_DIV               
      | T_MOD               
      | T_TIMES             
      | T_PLUS              
      | T_MINUS             
      | T_EQUAL             
      | T_DIFF              
      | T_LESS              
      | T_GREAT             
      | T_EQLESS            
      | T_EQGREAT           
      | T_AND           
      | T_OR            
      | T_ID of Identifier.id
      | T_NUM of Numeral.num
      | T_FAL of Falseval.fal

end
