(* Copyright (C) 2003 Mikolaj Konarski
 *
 * This file is part of the Dule compiler.
 * The Dule compiler is released under the GNU General Public License (GPL).
 * Please see the file Dule-LICENSE for license information.
 *
 * $Id: ll_parser.ml,v 1.4 2003/12/04 05:41:33 mikon Exp $
 *) 

(* This is the LL(1) parser of the Dule language.
   It should be used on top-level, with the help of Camlp4.
   Compile with [make ll] run with [dule.top] in [dule/code] 
   put commands into [.ocamlinit]. Requires OCaml 3.07. *)

(* new kewords wrt OCaml:
   \\|library\\|spec\\|ind\\|coind\\|map\\|con\\|fold\\|de\\|unfold\\|unde
   \\|load   \\|link *)

let rec mklistexp loc =
  function
    | [] -> <:expr< [] >>
    | x :: xs -> <:expr< [$x$ :: $mklistexp loc xs$] >>

open Pcaml
EXTEND
  GLOBAL: expr;
  expr: FIRST
    [ "dule"
     [ li = start -> 
       <:expr< main Main_common.elm $li$ >>
     | "there_comes_the_core"; t = core ->
       <:expr< main elcp $t$ >> ]];

(* labels *)

  type_label:
    [[ i = LIDENT -> <:expr< s2type $str:i$ >> ]];
  value_label:
    [[ i = LIDENT -> <:expr< s2value $str:i$ >> ]];
  case_label:
    [[ i = UIDENT -> <:expr< s2case $str:i$ >>
     | i = LIDENT -> <:expr< s2case $str:i$ >> ]];
  dule_label:
    [[ i = UIDENT -> <:expr< s2dule $str:i$ >> ]];
  sp_label:
    [[ i = UIDENT -> <:expr< s2sp $str:i$ >> ]];
  tilco_label: (* lexer demands this *)
    [[ i = LABEL -> <:expr< s2value $str:i$ >> ]];
  til_label:   (* and this *)
    [[ i = TILDEIDENT -> <:expr< s2value $str:i$ >> ]];

(* types *)

  field_typ:
    [[ i = value_label; ":"; f = typ ->
         <:expr< ($i$, $f$) >> ]];

  case_typ:
    [[ "`"; i = case_label; f = typ ->
         <:expr< ($i$, $f$) >>
     | "`"; i = case_label ->
	 <:expr< ($i$, mktyp (C.F_pp IList.nil)) >> ]];

  param_typ:
    [[ i = tilco_label; f = typ ->
         <:expr< ($i$, $f$) >> ]];

  typ:
    [ "typ"
     [ f1 = SELF; "."; f2 = SELF ->
	 <:expr< mktyp(C.F_COMP ($f1$, $f2$)) >>
     | i = type_label ->
	 <:expr< mktyp(C.F_PR ($i$)) >>
     | i = dule_label ->
	 <:expr< mktyp(C.F_PR ($i$)) >>
     | "{"; lf = LIST0 field_typ SEP ";"; "}" ->
         <:expr< mktyp(C.F_pp (check_repeat $mklistexp loc lf$)) >>
     | "["; lf = LIST0 case_typ SEP "|"; "]" ->
         <:expr< mktyp(C.F_ss (check_repeat $mklistexp loc lf$)) >>
     | lf = LIST0 param_typ; "->"; f = typ LEVEL "typ" ->
	 <:expr< mktyp(C.F_ee (check_repeat $mklistexp loc lf$, $f$)) >>
     | "ind"; i = type_label; ":"; f = typ ->
	 <:expr< mktyp(C.F_ii($i$, $f$)) >>
     | "coind"; i = type_label; ":"; f = typ ->
	 <:expr< mktyp(C.F_tt($i$, $f$)) >>
     | "("; f = typ; ")" -> <:expr< $f$ >> ]];

(* values *)
 
  field:
    [[ i = value_label; "="; t = valu ->
         <:expr< ($i$, $t$) >>
     | i = value_label ->
         <:expr< ($i$, mkvalu(C.T_pr $i$)) >> ]];

  case:
    [[ "`"; i = case_label; t = embedding ->
         <:expr< ($i$, $t$) >> ]];

  embedding:
    [[ i = value_label; "->"; t = valu -> (* special case *)
	 <:expr< mkvalu(C.T_curry (IList.cons (it, ($i$, mktyp(C.F_x)))
				     IList.nil, $t$)) >>
     | i = value_label -> (* special case *)
	 <:expr< mkvalu(C.T_pr $i$) >>
     | p = pattern; "->"; t = valu ->
	 <:expr< mkvalu(C.T_curry (IList.cons (it, $p$)
				     IList.nil, $t$)) >>
     | t = valu ->
	 <:expr< $t$ >>
     | "->"; t = valu ->
	 <:expr< mkvalu(C.T_curry (IList.cons (it, 
					       (mkwild(), mktyp(C.F_x)))
				     IList.nil, $t$)) >> ]];

  argument:
    [[ i = tilco_label; t = valu LEVEL "other" ->
         <:expr< ($i$, $t$) >> 
     | i = til_label ->
         <:expr< ($i$, mkvalu(C.T_pr $i$)) >> ]];


  let_item:
    [[ p = pattern; "="; t = valu ->
         <:expr< let (k, f) = $p$ in (k, (f, $t$)) >> 
     | "rec"; p = pattern; "="; t = valu ->
         <:expr< let (k, f) = $p$ in 
	 (k, (f, mkvalu(C.T_fix (k, $t$)))) >> ]];

  param:
    [[ i = til_label ->
         <:expr< ($i$, ($i$, mktyp(C.F_x))) >> 
     | i = tilco_label; p = pattern ->
         <:expr< ($i$, $p$) >> 
     | "~"; "("; i = value_label; ":"; f = typ; ")" ->
         <:expr< ($i$, ($i$, $f$)) >> ]];

  pattern:
    [[ i = value_label; ":"; t = typ ->
         <:expr< ($i$, $t$) >> 
     | i = value_label ->
         <:expr< ($i$, mktyp(C.F_x)) >>
     | "_" ->
         <:expr< (mkwild(), mktyp(C.F_x)) >>
     | "{"; lt = LIST0 field SEP ";"; "}" ->
         <:expr< (mkpatid(), mkpat(check_repeat $mklistexp loc lt$)) >> ]];

  partial_arguments:
    [[ "("; lt = LIST1 argument; ")" ->
         <:expr< $lt$ >> ]];

  valu:
    [ "app"
     [ t = valu; lt = LIST1 argument  ->
	 <:expr< mkvalu(C.T_appl (check_repeat $mklistexp loc lt$, $t$)) >>
     | t = valu; "~"  -> (* the case of zero arguments *)
	 <:expr< mkvalu(C.T_appl (IList.nil, $t$)) >> ]
    | "other"
     [ ":"; f = typ ->
         <:expr< mkvalu(C.T_id $f$) >>
     | t1 = valu; "."; t2 = valu ->
	 <:expr< mkvalu(C.T_comp ($t1$, $t2$)) >>
     | i = value_label ->
	 <:expr< mkvalu(C.T_pr $i$) >>
     | i = dule_label ->
	 <:expr< mkvalu(C.T_pr $i$) >>
     | "{"; lt = LIST0 field SEP ";"; "}" ->
         <:expr< mkvalu(C.T_record (check_repeat $mklistexp loc lt$)) >>
     | t1 = valu; "."; "`"; i = case_label ->
	 <:expr< mkvalu(C.T_comp ($t1$, mkvalu(C.T_in $i$))) >>
     | "`"; i = case_label ->
	 <:expr< mkvalu(C.T_comp (mkvalu(C.T_record IList.nil),
				  mkvalu(C.T_in $i$))) >>
     | "["; lt = LIST0 case SEP "|"; "]" ->
         <:expr< mkvalu(C.T_case (check_repeat $mklistexp loc lt$)) >>
     | "map"; t = embedding ->
	 <:expr< mkvalu(C.T_map $t$) >>
     | "con" ->
	 <:expr< mkvalu(C.T_con) >>
     | "fold"; t = embedding ->
	 <:expr< mkvalu(C.T_fold $t$) >>
     | "de" ->
	 <:expr< mkvalu(C.T_de) >>
     | "uncon" ->
	 <:expr< mkvalu(C.T_uncon) >>
     | "unfold"; t = embedding ->
	 <:expr< mkvalu(C.T_unfold $t$) >>
     | "unde" ->
	 <:expr< mkvalu(C.T_unde) >>
     | "fun"; lf = LIST0 param; "->"; t = valu LEVEL "app" ->
	 <:expr< mkvalu(C.T_curry (check_repeat $mklistexp loc lf$, $t$)) >>
     | "match"; t1 = valu LEVEL "app"; "with"; t2 = valu LEVEL "app" ->
	 <:expr< mkvalu(C.T_appl 
			  (IList.cons (it, $t1$) IList.nil, $t2$)) >>
     | "let"; li = LIST0 let_item; "in"; t = valu LEVEL "app" ->
	 <:expr< let lift = check_repeat $mklistexp loc li$ in
                 let lt = IList.vmap (fun (f, t) -> t) lift in
		 let lif = IList.bmap (fun (k, (f, t)) -> (k, f)) lift in
		 mkvalu(C.T_appl (lt, (mkvalu(C.T_curry (lif, $t$))))) >>
     | t = valu; lt = partial_arguments  ->
	 <:expr< mkvalu(C.T_pappl (check_repeat $mklistexp loc lt$, $t$)) >>
     | "if"; t1 = valu LEVEL "app"; "then"; t2 = valu LEVEL "app"; "else"; t3 = valu  LEVEL "app" ->
         <:expr< 
	 let case = 
	   IList.cons (tt, 
		       mkvalu(C.T_curry (IList.cons (it, 
						   (mkwild(), 
						    mktyp(C.F_x))) 
					 IList.nil, $t2$)))
	     (IList.cons (ff, 
			  mkvalu(C.T_curry (IList.cons (it, 
						      (mkwild(), 
						       mktyp(C.F_x))) 
					    IList.nil, $t3$)))
		IList.nil)
         in
         mkvalu(C.T_appl (IList.econs (it, $t1$) IList.nil,
			  mkvalu(C.T_case case))) >>
     | "assert"; t1 = valu LEVEL "app"; "in"; t2 = valu LEVEL "app" ->
	 <:expr< mkvalu(C.T_assert($t1$, $t2$)) >>
     | "fail" ->
         <:expr< mkvalu(C.T_fail) >>
     | n = INT -> 
         <:expr< mkint($int:n$) >>
     | "("; t = valu LEVEL "app"; ")" -> <:expr< $t$ >> ]];

(* specifications *)

  field_sp:
    [[ i = dule_label; ":"; p = sp ->
         <:expr< ($i$, $p$) >>    
     | i = dule_label ->
	 <:expr< ($i$, mksp(M.S_Ii (dule2sp $i$))) >> ]];

  param_sp:
    [[ "~"; ip = field_sp ->
         <:expr< $ip$ >> ]];

  bb_item:
    [[ "type"; i = type_label ->
         <:expr< ($i$, M.Bb_type) >> 
     | "value"; i = value_label; ":"; f = typ ->
         <:expr< ($i$, M.Bb_value $f$) >> ]];

  sp:
    [ "param"
     [ lp = LIST0 param_sp; "->"; p = sp ->
	 <:expr< mksp(M.S_Ee (check_repeat $mklistexp loc lp$, $p$)) >> ]
    | "other"
     [ "{"; lp = LIST0 field_sp SEP ";"; "}" ->
         <:expr< mksp(M.S_Aa (check_repeat $mklistexp loc lp$)) >>
     | "{"; "{"; lp = LIST0 field_sp SEP ";"; "}"; "}" ->
         <:expr< mksp(M.S_Cc (check_repeat $mklistexp loc lp$)) >>
     | "sig"; li = LIST0 bb_item; "end" ->
	 <:expr< mksp(M.S_Bb (check_repeat $mklistexp loc li$)) >>
     | p = sp; "with"; m = dule ->
         <:expr< mksp(M.S_Ww ($m$, $p$)) >>
     | i = sp_label -> 
	 <:expr< mksp(M.S_Ii ($i$)) >>
     | "("; p = sp; ")" -> <:expr< $p$ >> ]];

(* modules *)

  field_dule:
    [[ i = dule_label; "="; m = dule ->
         <:expr< ($i$, $m$) >>
     | i = dule_label ->
         <:expr< ($i$, mkdule(M.M_Pr $i$)) >> ]];

  base_item:
    [[ "type"; i = type_label; "="; f = typ ->
         <:expr< ($i$, M.Base_type $f$) >>
     | "value"; i = value_label; "="; t = valu ->
         <:expr< ($i$, M.Base_value $t$) >>
     | "value"; "rec"; i = value_label; "="; t = valu ->
         <:expr< ($i$, M.Base_value (mkvalu(C.T_fix ($i$, $t$)))) >> ]];

  def_dule:
    [[ i = dule_label; "="; m = dule ->
         <:expr< ($i$, $m$) >> ]];

  def_sp:
    [[ i = sp_label; "="; p = sp ->
         <:expr< ($i$, $p$) >> ]];

  one_dule:
    [[ lm = LIST1 def_dule SEP "and" ->
	 <:expr< check_repeat $mklistexp loc lm$ >> ]];

  one_sp:
    [[ lp = LIST1 def_sp SEP "and" ->
	 <:expr< check_repeat $mklistexp loc lp$ >> ]];

  link_item:
    [[ im = def_dule ->
	 <:expr< let (i, m) = $im$ in M.Link_Dule (i, m) >>
     | "module"; im = def_dule ->
	 <:expr< let (i, m) = $im$ in M.Link_Dule (i, m) >>
     | "module"; "ind"; lm = one_dule ->
	 <:expr< M.Link_Ind_Dule $lm$ >>
     | "module"; "coind"; lm = one_dule ->
	 <:expr< M.Link_CoInd_Dule $lm$ >>
     | "spec"; ip = def_sp ->
	 <:expr< let (i, p) = $ip$ in M.Link_Sp (i, p) >>
     | "spec"; "rec"; lp = one_sp ->
	 <:expr< M.Link_Rec_Sp $lp$ >>
     | "library"; im = def_dule ->
	 <:expr< let (i, m) = $im$ in M.Link_Lib (i, m) >>
     | "library"; "ind"; lm = one_dule ->
	 <:expr< M.Link_Ind_Lib $lm$ >>
     | "library"; "coind"; lm = one_dule ->
	 <:expr< M.Link_CoInd_Lib $lm$ >> ]];
	
  dule:
    [ "right" RIGHTA
     [ "::"; p = sp; m = dule ->
	 <:expr< mkdule(M.M_Spec ($m$, $p$)) >>
     | m1 = dule; "with"; m2 = dule ->
	 <:expr< mkdule(M.M_With ($m2$, $m1$)) >> ]
    | "left"
     [ ":"; "{"; "{"; lp = LIST0 field_sp SEP ";"; "}"; "}" ->
         <:expr< mkdule(M.M_Id (check_repeat $mklistexp loc lp$)) >>
     | m1 = SELF; "."; m2 = SELF ->
	 <:expr< mkdule(M.M_Comp ($m1$, $m2$)) >>
     | i = dule_label ->
         <:expr< mkdule(M.M_Pr $i$) >> 
     | "{"; lm = LIST0 field_dule SEP ";"; "}" ->
	 <:expr< mkdule(M.M_Accord (check_repeat $mklistexp loc lm$)) >>
     | "{"; "{"; lm = LIST0 field_dule SEP ";"; "}"; "}" ->
	 <:expr< mkdule(M.M_Concord (check_repeat $mklistexp loc lm$)) >>
     | "struct"; li = LIST0 base_item; "end" ->
	 <:expr< mkdule(M.M_Base (check_repeat $mklistexp loc li$)) >>
     | m1 = dule; "|"; m2 = dule ->
	 <:expr< mkdule(M.M_Inst ($m1$, $m2$)) >>
     | m = dule; ":>"; p = sp ->
	 <:expr< mkdule(M.M_Trim ($m$, $p$)) >>
     | "link"; li = LIST0 link_item; "end" ->
	 <:expr< mkdule(M.M_Link (List.rev $mklistexp loc li$)) >>
     | "load"; i = dule_label ->
	 <:expr< mkdule(M.M_Load $i$) >>
     | "("; m = dule; ")" -> <:expr< $m$ >> ]];

(* entry points *)

  start:
    [[ li = LIST0 link_item -> 
       <:expr< mkstart(List.rev $mklistexp loc li$) >> ]];

  core:
    [[ t = valu -> 
       <:expr< $t$ >> ]];

END
