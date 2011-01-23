(* Copyright (C) 2003 Stasiek Skowron
 *
 * This file is part of the Dule compiler.
 * The Dule compiler is released under the GNU General Public License (GPL).
 * Please see the file Dule-LICENSE for license information.
 *
 * $Id: error_rep.ml,v 1.13 2005/05/27 18:03:55 mikon Exp $
 *) 

module type Location =
  sig
    type t

    val int2t : int -> int -> t
    val none : t
    val input_name : string ref
    val print : Format.formatter -> t -> unit
    val t2string : t -> string
    val eq : t -> t -> bool
    val t2int : t -> int
  end

module Location : Location =
  struct
    open Lexing
      
    type t = {l_start : int; l_end : int}

    let int2t l_start l_end = {l_start = l_start; l_end = l_end}

    let none = {l_start = -1; l_end = -1}
	
    let input_name = ref ""

    let for_position file loc =
      let ic = open_in_bin file in
      let ungetBuf = ref [] in
      let getChar () =
	match !ungetBuf with
	| [] -> input_char ic
	| c::t -> ungetBuf:=t; c
      in
      let ungetChar c = ungetBuf:=c::(!ungetBuf) in
      let inputLine () =
	let rec inputLine acc =
	  match getChar () with
	  | '\n' -> acc+1
	  | '\r' ->
	      (try
		match getChar () with
		  '\n' -> acc+2
		| c -> ungetChar c; acc+1
	      with End_of_file -> acc+1)
	  | _ -> inputLine (acc+1)
	in inputLine 0
      in
      let rec find linenum linebeg =
	let len = inputLine () in
	if linebeg+len>loc then (linenum, linebeg)
	else find (linenum+1) (linebeg+len)
      in
      try
	let (linenum, linebeg) = find 1 0 in
	let _ = close_in ic in
	Some (linenum, linebeg)
      with End_of_file ->
	let _ = close_in ic in
	None

    let print ppf loc =
      if String.length !input_name = 0 then
	Format.fprintf ppf "Characters %i-%i:@." loc.l_start loc.l_end
      else
	let filename = !input_name in
	Format.fprintf ppf "File \"%s" filename;
	match (for_position filename loc.l_start) with
	| Some (linenum, linebeg) ->
	    Format.fprintf ppf "\", line %i" linenum;
	    Format.fprintf ppf ", characters %i" (loc.l_start - linebeg);
	    Format.fprintf ppf "-%i:@." (loc.l_end - linebeg)
	| None ->
	    Format.fprintf ppf "\", at the end of file@."

    let t2string loc = 
      let _ = print Format.str_formatter loc in
      Format.flush_str_formatter () 

    let eq l l' = (l = l')

    let combine m n = m * 65599 + n
    let t2int {l_start = l; l_end = l'} = combine l l'
  end


module type ErrorRep =
  sig

    module Location : Location

    val colors: bool ref

    type flag = string
    type flagValue = 
	FlagUp | FlagDn | FlagStr of string
    type errorParam = 
	Flag of flag * flagValue | Msg of string | Loc of Location.t

    class type errorClass = object
      method instance: errorParam list -> error
      method matchError: error -> bool
      method subclass: (int -> errorParam list -> error) -> errorClass
    end
    and error = object
      method report: Format.formatter -> unit
      method private prolog: Format.formatter -> unit
      method private helloMsg: Format.formatter -> unit
      method private flags: Format.formatter -> unit
      method private epilog: Format.formatter -> unit
      method setFlagValue: flag -> flagValue -> unit
      method getFlagValue: flag -> flagValue
      method setFlag: flag -> unit
      method unsetFlag: flag -> unit
      method isSet: flag -> bool
      method getClass: int
    end

    class rootClass : errorClass
    class rootError : int -> errorParam list -> error

    val rootClass : errorClass
  end

module ErrorRep'
    (Location : Location)
    : ErrorRep with module Location = Location =
  struct

    module Location = Location

    let colors = ref true

    type flag = string
    type flagValue = 
	FlagUp | FlagDn | FlagStr of string
    type errorParam = 
	Flag of flag * flagValue | Msg of string | Loc of Location.t

    class seq = object
      val mutable s = 0
      method get = let pom = s in s <- s+1; pom
    end
    let seq = new seq

    class type errorClass = object
      method instance: errorParam list -> error
      method matchError: error -> bool
      method subclass: (int -> errorParam list -> error) -> errorClass
    end
    and error = object
      method report: Format.formatter -> unit
      method private prolog: Format.formatter -> unit
      method private helloMsg: Format.formatter -> unit
      method private flags: Format.formatter -> unit
      method private epilog: Format.formatter -> unit
      method setFlagValue: flag -> flagValue -> unit
      method getFlagValue: flag -> flagValue
      method setFlag: flag -> unit
      method unsetFlag: flag -> unit
      method isSet: flag -> bool
      method getClass: int
    end

    class rootClass : errorClass = object (self)
      val id = seq#get
      method instance = new rootError id
      method matchError (e:error) = e#getClass=id
      method subclass constructor = new subclass self#matchError constructor
    end
    and rootError cls pms : error =
      let fls = ref [] in
      let lcs = ref [] in
      let mss = ref [] in
      let _ =
	let rec parsePms lst =
	  match lst with
	    [] -> ()
	  | (Flag (fl, flv))::t -> fls:=(fl, flv)::(!fls); parsePms t
	  | (Loc l)::t -> lcs:=l::(!lcs); parsePms t
	  | (Msg m)::t -> mss:=m::(!mss); parsePms t
	in parsePms pms
      in
      object (self)
	val mutable flags = !fls
	val loc =
	  match !lcs with
	    [] -> None
	  | l::_ -> Some l
	val allLocs = !lcs
	val msg =
	  match !mss with
	    [] -> None
	  | m::_ -> Some m
	val allMsgs = !mss
(*	val mutable savedOut = None
	val mutable savedFlush = None

	method private saveOutputFunctions pp =
	  let (out, flush) = pp_get_Format.formatter_output_functions pp () in
	  savedOut <- Some out;
	  savedFlush <- Some flush

	method private restoreOutputFunctions pp =
	  match (savedOut, savedFlush) with
	    (Some out, Some flush) ->
	      pp_set_Format.formatter_output_functions pp out flush
	  | _ -> ()

	method private setOutputFunctions pp =
	  match (savedOut, savedFlush) with
	    (Some out, Some flush) ->
	      let newOut s l c =
		let highlight = "\027[1m" in
		let normvideo = "\027[0m" in
		out highlight 0 (String.length highlight);
		out s l c;
		out normvideo 0 (String.length normvideo)
	      in
	      pp_set_Format.formatter_output_functions pp newOut flush;
	  | _ -> ()*)

	method private beforeErrorReport pp =
	  Format.pp_print_flush pp ();
(*	  self#saveOutputFunctions pp;
	  self#setOutputFunctions pp;
	  pp_open_box pp 0*)
	method private afterErrorReport pp =
(*	  pp_close_box pp ();*)
	  Format.pp_print_newline pp ();
(*	  self#restoreOutputFunctions pp*)
	method report pp =
	  let rec przeplot locs msgs =
	    match (locs, msgs) with
	      ([], []) -> []
	    | (l::tLoc, m::tMsg) -> (Some l, Some m)::(przeplot tLoc tMsg)
	    | (l::tLoc, []) -> (Some l, None)::(przeplot tLoc [])
	    | ([], m::tMsg) -> (None, Some m)::(przeplot [] tMsg)
	  in
	  let rec for_all lst =
	    match lst with
	      [] -> ()
	    | (Some l, Some msg)::rst ->
		self#location pp l;
		self#doPrintMsg pp msg;
		for_all rst
	    | (Some l, None)::rst ->
		self#location pp l;
		for_all rst
	    | (None, Some msg)::rst ->
		self#doPrintMsg pp msg;
		for_all rst
	    | (None, None)::rst -> for_all rst
	  in
	  self#beforeErrorReport pp;
	  self#prolog pp;
	  self#hello pp;
	  for_all (przeplot allLocs allMsgs);
	  self#flags pp;
	  self#epilog pp;
	  self#afterErrorReport pp
	method private prolog pp = 
	  if !colors then Format.pp_print_string pp "\027[1m" else ()
	method private hello pp =
	  self#helloMsg pp;
	  match msg with
	    Some _ -> Format.fprintf pp ":@ "
	  | None -> Format.fprintf pp "@ "
	method private doPrintMsg pp msg =
	  Format.pp_print_string pp msg;
	  Format.pp_print_space pp ()
	method private helloMsg pp = Format.fprintf pp "Surprising error@," (* when does this happen? *)
	method private location pp loc =
	  Location.print pp loc;
	  Format.pp_print_cut pp ();
	method private flags pp =
	  match flags with
	    [] -> ()
	  | _ ->
	      let flv2string flv =
		match flv with
		  FlagUp -> "[*]"
		| FlagDn -> "[ ]"
		| FlagStr s -> s in
	      let printFl fl flv =
		Format.fprintf pp "%s: %s" fl (flv2string flv) in
	      let rec printFls lst =
		match lst with
		  [] -> ()
		| (fl, flv)::[] -> printFl fl flv; Format.pp_print_cut pp ()
		| (fl, flv)::t -> 
		    printFl fl flv; Format.pp_print_space pp (); printFls t
	      in printFls flags
	method private epilog pp = 
	  if !colors then Format.pp_print_string pp "\027[0m" else ()
	method setFlagValue fl flv = flags <- (fl, flv)::flags
	method getFlagValue fl =
	  let rec search lst =
	    match lst with
	      [] -> FlagDn
	    | (f, v)::t -> if f=fl then v else search t
	  in search flags
	method setFlag fl = self#setFlagValue fl FlagUp
	method unsetFlag fl = self#setFlagValue fl FlagDn
	method isSet fl =
	  match self#getFlagValue fl with
	    FlagUp -> true
	  | _ -> false
	method getClass = cls
      end
    and subclass superMatch constructor : errorClass = object (self)
      val mutable id = -1
      initializer id <- seq#get
      method instance = constructor id
      method matchError e = (e#getClass=id || superMatch e)
      method subclass constructor = new subclass self#matchError constructor
    end

    let rootClass = new rootClass
  end

module ErrorRep = ErrorRep' (Location)


(***************************************************************)
(* This piece of documentation is in Polish --- sorry.         *)
(*                                                             *)
(* Wlasciwy modul do raportowania bledow.                      *)
(* Sposob uzycia jest prosty:                                  *)
(*   Mamy nastepujace obiekty klas bledow:                     *)
(*                                                             *)
(*     fileError                                               *)
(*      +-- coreError                                          *)
(*      |    +-- coreBackError                                 *)
(*      |    +-- coreMiddleError                               *)
(*      |    \-- coreFrontError                                *)
(*      |                                                      *)
(*      +-- middleError                                        *)
(*      |    \-- middleMiddleError                             *)
(*      |                                                      *)
(*      \-- modError                                           *)
(*           +-- modBackError                                  *)
(*           +-- modMiddleError                                *)
(*           \-- modFrontError                                 *)
(*                                                             *)
(*                                                             *)
(*   Do bledow generowanych w poszczegolnych modulach nalezy   *)
(*   uzywac klas odpowiadajacych im nazwa                      *)
(*   (w mod_front uzywac ModFrontError,                        *)
(*    w mod_back uzywac ModBackError)                          *)
(*                                                             *)
(*   Tworzenie bledow udbywa sie przez uzycie metody           *)
(*   #instance pms                                             *)
(*   z odpowiedniego obiektu klasy, gdzie pms jest lista       *)
(*   elementow postaci:                                        *)
(*      Flag (fl, flv)  --- ustawienie flagi fl na wartosc flv *)
(*                          (dostepne wartosci FlagUp, FlagDn, *)
(*                           FlagStr of string)                *)
(*      Loc l           --- wskazanie lokacji w ktorej         *)
(*                          wystapil blad                      *)
(*      Msg s           --- ustawienie dodatkowego komunikatu  *)
(*                          zwiazanego z bledem                *)
(*                                                             *)
(*   uzycie tej metody daje nam obiekt bledu, jego raportowanie*)
(*   odbywa sie poprzez uzycie metody                          *)
(*   #report pp                                                *)
(*   gdzie pp jest Format.formatterem na ktory nalezy wypisac  *)
(*   informacje o bledzie.                                     *)
(*                                                             *)
(*   W obiekcie bledu mozna ustawiac flagi metodami            *)
(*   #setFlagValue fl flv,                                     *)
(*   #setFlag fl, lub                                          *)
(*   #unsetFlag fl                                             *)
(*   gdzie fl    --- ustawiana flaga                           *)
(*         flv   --- ustawiana wartosc                         *)
(*   (flagi identyfikowane sa poprzez ich nazwe (string),      *)
(*    ktora pozniej jest wypisywana przy raportowaniu;         *)
(*    #setFlag i #unsetFlag ustawiaja flage na odpowiednio     *)
(*    FlagUp i FlagDn)                                         *)
(*                                                             *)
(*   Z obiektu bledu mozna odzyskac wartosc flagi metodami     *)
(*   #getFlagValue fl --- zwraca wartosc flagi fl              *)
(*                        (FlagDn jesli brak wartosci)         *)
(*   #isSet fl        --- zwraca                               *)
(*                          true jesli wartosc jest FlagUp     *)
(*                          false wpp                          *)
(*                                                             *)
(*                                                             *)
(*   Ale przede wszystkim mozna dowiedziec sie, czy dany blad  *)
(*   nalezy do danej klasy bledow w nastepujacy sposob:        *)
(*     jesli `klasa' jest obiektem danej klasy                 *)
(*     jesli `blad' jest danym bledem                          *)
(*     to                                                      *)
(*       klasa#matchError blad                                 *)
(*              jest rowne true, gdy `blad' nalezy do `klasy', *)
(*              jest rowne false wpp.                          *)
(*                                                             *)
(*                                                             *)
(*   Mamy jeszcze jedna prosta funkcje:                        *)
(*      string2error str                                       *)
(*   ktora tworzy obiekt bledu zawierajacy jako komunikat      *)
(*   zadany napis (str). Stworzony blad nie nalezy do zadnej   *)
(*   z powyzszyh klas.                                         *)
(***************************************************************)

module type ErrorRepLib =
  sig
    module Location : Location

    type flag = string
    type flagValue = 
	FlagUp | FlagDn | FlagStr of string
    type errorParam = 
	Flag of flag * flagValue | Msg of string | Loc of Location.t

    class type errorClass = object
      method instance: errorParam list -> error
      method matchError: error -> bool
      method subclass: (int -> errorParam list -> error) -> errorClass
    end
    and error = object
      method report: Format.formatter -> unit
      method setFlagValue: flag -> flagValue -> unit
      method getFlagValue: flag -> flagValue
      method setFlag: flag -> unit
      method unsetFlag: flag -> unit
      method isSet: flag -> bool
      method getClass: int
    end

    val string2error : string -> error

    val fileError : errorClass

    val coreError : errorClass
    val coreBackError : errorClass
    val coreMiddleError : errorClass
    val coreFrontError : errorClass
    val middleError : errorClass
    val middleMiddleError : errorClass
    val modError : errorClass
    val modBackError : errorClass
    val modMiddleError : errorClass
    val modFrontError : errorClass

    val syntaxError : errorClass
    val lexingError : errorClass
  end

module ErrorRepLib'
    (Location : Location)
    (ErrorRep : ErrorRep
     with module Location = Location)
    : (ErrorRepLib
     with module Location = Location) =
  struct
    module Location = Location
    open ErrorRep

    exception ShouldNotBeUsed

    type flag = ErrorRep.flag
    type flagValue = ErrorRep.flagValue = 
	FlagUp | FlagDn | FlagStr of string
    type errorParam = ErrorRep.errorParam =
        Flag of flag * flagValue | Msg of string | Loc of Location.t

    class stringErrorImplementation cls pms = object
      inherit rootError cls pms
      method private helloMsg pp = Format.fprintf pp "Unlocated error@,"
    end

    class fileErrorImplementation cls pms = object (self)
      inherit rootError cls pms
      method private fileName pp = raise ShouldNotBeUsed
      method private helloMsg pp =
	Format.pp_print_string pp "Error raised by a function defined in file";
	Format.pp_print_space pp ();
	self#fileName pp;
	Format.pp_print_cut pp ()
    end

    class coreErrorImplementation cls pms = object
      inherit fileErrorImplementation cls pms
    end

    class coreBackErrorImplementation cls pms = object
      inherit coreErrorImplementation cls pms
      method private fileName pp = Format.fprintf pp "core_back.ml"
    end

    class coreMiddleErrorImplementation cls pms = object
      inherit coreErrorImplementation cls pms
      method private fileName pp = Format.fprintf pp "core_middle.ml"
    end

    class coreFrontErrorImplementation cls pms = object
      inherit coreErrorImplementation cls pms
      method private fileName pp = Format.fprintf pp "core_front.ml"
    end

    class middleErrorImplementation cls pms = object
      inherit fileErrorImplementation cls pms
    end

    class middleMiddleErrorImplementation cls pms = object
      inherit middleErrorImplementation cls pms
      method private fileName pp = Format.fprintf pp "middle_middle.ml"
    end

    class modErrorImplementation cls pms = object
      inherit fileErrorImplementation cls pms
    end

    class modBackErrorImplementation cls pms = object
      inherit modErrorImplementation cls pms
      method private fileName pp = Format.fprintf pp "mod_back.ml"
    end

    class modMiddleErrorImplementation cls pms = object
      inherit modErrorImplementation cls pms
      method private fileName pp = Format.fprintf pp "mod_middle.ml"
    end

    class modFrontErrorImplementation cls pms = object
      inherit modErrorImplementation cls pms
      method private fileName pp = Format.fprintf pp "mod_front.ml"
    end

    let stringError = rootClass#subclass (new stringErrorImplementation)
    let string2error str = stringError#instance [Msg str]

    let fileError = rootClass#subclass (new fileErrorImplementation)
    let coreError = fileError#subclass (new coreErrorImplementation)
    let coreBackError = coreError#subclass (new coreBackErrorImplementation)
    let coreMiddleError = 
      coreError#subclass (new coreMiddleErrorImplementation)
    let coreFrontError = coreError#subclass (new coreFrontErrorImplementation)
    let middleError = fileError#subclass (new middleErrorImplementation)
    let middleMiddleError = 
      middleError#subclass (new middleMiddleErrorImplementation)
    let modError = fileError#subclass (new modErrorImplementation)
    let modBackError = modError#subclass (new modBackErrorImplementation)
    let modMiddleError = modError#subclass (new modMiddleErrorImplementation)
    let modFrontError = modError#subclass (new modFrontErrorImplementation)

    class syntaxErrorImplementation cls pms = object
      inherit rootError cls pms
      method private helloMsg pp = Format.fprintf pp "Syntax error"
    end

    class lexingErrorImplementation cls pms = object
      inherit rootError cls pms
      method private helloMsg pp = Format.fprintf pp "Lexing error"
    end

    let syntaxError = rootClass#subclass (new syntaxErrorImplementation)
    let lexingError = rootClass#subclass (new lexingErrorImplementation)

    class type errorClass = object
      method instance: errorParam list -> error
      method matchError: error -> bool
      method subclass: (int -> errorParam list -> error) -> errorClass
    end
    and error = object
      method report: Format.formatter -> unit
      method setFlagValue: flag -> flagValue -> unit
      method getFlagValue: flag -> flagValue
      method setFlag: flag -> unit
      method unsetFlag: flag -> unit
      method isSet: flag -> bool
      method getClass: int
    end
  end

module ErrorRepLib = ErrorRepLib' (Location) (ErrorRep)


module type TextualError = 
  sig
    module Location : Location
    module ErrorRepLib : ErrorRepLib
    with module Location = Location

    val makeUnclosed : 
	Location.t -> string -> Location.t -> string -> ErrorRepLib.error
    val makeOtherSyntax : Location.t -> ErrorRepLib.error
    val makeIllegalCharacter : char -> int ->int-> ErrorRepLib.error
    val makeRunoffComment : int -> int -> ErrorRepLib.error

    exception TextualError of ErrorRepLib.error
end

module TextualError'
    (Location : Location)
    (ErrorRepLib : ErrorRepLib
    with module Location = Location)
    : TextualError
    with module Location = Location
    with module ErrorRepLib = ErrorRepLib =
  struct
    module Location = Location
    module ErrorRepLib = ErrorRepLib
    open ErrorRepLib

    let makeUnclosed opening_loc opening closing_loc closing =
      syntaxError#instance [Msg ("This '"^opening^"' might be unmatched"); 
			    Loc opening_loc;
			    Msg ("Here '"^closing^"' is expected"); 
			    Loc closing_loc]
    let makeOtherSyntax loc =
      syntaxError#instance [Loc loc]

    let makeIllegalCharacter c start stop =
      lexingError#instance [Msg ("Illegal character ("^(Char.escaped c)^")");
			    Loc (Location.int2t start stop)]
    let makeRunoffComment start stop =
      lexingError#instance [Msg "Comment not terminated";
			    Loc (Location.int2t start stop)]

    exception TextualError of ErrorRepLib.error
end

module TextualError = TextualError' (Location) (ErrorRepLib)
