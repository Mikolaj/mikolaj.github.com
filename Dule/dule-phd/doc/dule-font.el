;(* Copyright (C) 2006 Mikolaj Konarski
; *
; * This file is now a part of the Dule compiler.
; * The Dule compiler is released under the GNU General Public License (GPL).
; * Please see the file Dule-LICENSE for license information.
; * $Id: dule-font.el,v 1.1 2006-04-06 14:47:08 mikon Exp $
;*)
;
; File caml-font.el modified by mikon to also highlight Dule 
; --- an ugly hack overall. Can now also be used as one's .emacs.
; Standard ocaml-mode is still required, though caml-font.el is overridden.
;
; Original copyright of caml-font.el,v 1.19, follows:
;
;(*                Jacques Garrigue and Ian T Zimmerman                 *)
;(*                                                                     *)
;(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
;(*  en Automatique.  All rights reserved.  This file is distributed    *)
;(*  under the terms of the GNU General Public License.                 *)

;mikon:
;use this file as your .emacs 
;or in your .emacs add
;  (load "~/dule/doc/dule-font")
;
;If you have your local copy of ocaml-mode include this:
;(setq load-path
;      (nconc load-path
;             (list (concat "~/.ocaml-mode"))))
:
;
;the usual things so that this file suffices as .emacs:
(setq auto-mode-alist (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
;colorize also Dule files:
(setq auto-mode-alist
       (append '(("\\.dul$" . caml-mode)
		 ) auto-mode-alist))

;will get used only on windowed systems
(if window-system

(progn

;modified caml-font.el,v 1.19 follows:

;; useful colors

(cond
 ((x-display-color-p)
  (require 'font-lock)
  (cond
   ((not (boundp 'font-lock-type-face))
    ; make the necessary faces
    (make-face 'Firebrick)
    (set-face-foreground 'Firebrick "Firebrick")
    (make-face 'RosyBrown)
    (set-face-foreground 'RosyBrown "RosyBrown")
    (make-face 'Purple)
    (set-face-foreground 'Purple "Purple")
    (make-face 'MidnightBlue)
    (set-face-foreground 'MidnightBlue "MidnightBlue")
    (make-face 'DarkGoldenRod)
    (set-face-foreground 'DarkGoldenRod "DarkGoldenRod")
    (make-face 'DarkOliveGreen)
    (set-face-foreground 'DarkOliveGreen "DarkOliveGreen4")
    (make-face 'CadetBlue)
    (set-face-foreground 'CadetBlue "CadetBlue")
    ; assign them as standard faces
    (setq font-lock-comment-face 'Firebrick)
    (setq font-lock-string-face 'RosyBrown)
    (setq font-lock-keyword-face 'Purple)
    (setq font-lock-function-name-face 'MidnightBlue)
    (setq font-lock-variable-name-face 'DarkGoldenRod)
    (setq font-lock-type-face 'DarkOliveGreen)
    (setq font-lock-reference-face 'CadetBlue)))
;mikon --- a face for variant names
  (make-face 'Coral)
  (set-face-foreground 'Coral "coral2")
  (setq font-lock-builtin-face 'Coral)
;
  ; extra faces for documention
  (make-face 'Stop)
  (set-face-foreground 'Stop "White")
  (set-face-background 'Stop "Red")
  (make-face 'Doc)
  (set-face-foreground 'Doc "Red")
  (setq font-lock-stop-face 'Stop)
  (setq font-lock-doccomment-face 'Doc)
))

; The same definition is in caml.el:
; we don't know in which order they will be loaded.
(defvar caml-quote-char "'"
  "*Quote for character constants. \"'\" for Objective Caml, \"`\" for Caml-Light.")

(defconst caml-font-lock-keywords
  (list
;stop special comments
   '("\\(^\\|[^\"]\\)\\((\\*\\*/\\*\\*)\\)"
     2 font-lock-stop-face)
;doccomments
   '("\\(^\\|[^\"]\\)\\((\\*\\*[^*]*\\([^)*][^*]*\\*+\\)*)\\)"
     2 font-lock-doccomment-face)
;comments
   '("\\(^\\|[^\"]\\)\\((\\*[^*]*\\*+\\([^)*][^*]*\\*+\\)*)\\)"
     2 font-lock-comment-face)
;character literals
   (cons (concat caml-quote-char "\\(\\\\\\([ntbr" caml-quote-char "\\]\\|"
                 "[0-9][0-9][0-9]\\)\\|.\\)" caml-quote-char
                 "\\|\"[^\"\\]*\\(\\\\\\(.\\|\n\\)[^\"\\]*\\)*\"")
         'font-lock-string-face)
;mikon --- dule constructors
   '("`\\<[A-Za-z][A-Za-z0-9_']*\\>" . font-lock-builtin-face)
;modules and constructors
   '("`?\\<[A-Z][A-Za-z0-9_']*\\>" . font-lock-function-name-face)
;definition
   (cons "\\<\\(a\\(nd\\|s\\|ssert\\)\\|c\\(?:lass\\|o\\(?:ind\\|n\\(?:straint\\)?\\)\\)\\|de\\|ex\\(?:ception\\|ternal\\)\\|f\\(?:old\\|un\\(?:ct\\(?:ion\\|or\\)\\)?\\)\\|in\\(?:d\\|herit\\|itializer\\)?\\|l\\(?:et\\|ibrary\\)\\|m\\(?:ap\\|ethod\\|\\(?:odu\\|utab\\)le\\)\\|of\\|p\\(?:arser\\|rivate\\)\\|rec\\|spec\\|type\\|un\\(?:con\\|de\\|fold\\)\\|v\\(al\\(ue\\)?\\|irtual\\)\\)\\>"

;mikon --- library, etc. added
;(concat
;          "\\<\\(a\\(nd\\|s\\|ssert\\)\\|c\\(onstraint\\|lass\\)"
;          "\\|ex\\(ception\\|ternal\\)\\|fun\\(ct\\(ion\\|or\\)\\)?"
;          "\\|in\\(herit\\|itializer\\)?\\|let"
;          "\\|m\\(ethod\\|utable\\|odule\\)"
;          "\\|of\\|p\\(arser\\|rivate\\)\\|rec\\|type"
;"\\|library\\|spec\\|ind\\|coind\\|map\\|con\\|fold\\|de\\|uncon\\|unfold\\|unde" 
;          "\\|v\\(al\\(ue\\)?\\|irtual\\)\\)\\>")

         'font-lock-type-face)
;blocking --- link added
   '("\\<\\(begin\\|end\\|link\\|object\\|s\\(ig\\|truct\\)\\)\\>"
     . font-lock-keyword-face)
;control --- load added
   (cons (concat
          "\\<\\(do\\(ne\\|wnto\\)?\\|else\\|for\\|i\\(f\\|gnore\\)"
          "\\|l\\(azy\\|oad\\)\\|match\\|new\\|or\\|t\\(hen\\|o\\|ry\\)"
          "\\|w\\(h\\(en\\|ile\\)\\|ith\\)\\)\\>"
          "\\|\|\\|->\\|&\\|#")
         'font-lock-reference-face)
   '("\\<\\(fail\\|raise\\)\\>" . font-lock-comment-face)
;labels (and open) --- changed
   '("\\(\\([~?]\\|\\<\\)[a-z][a-zA-Z0-9_']*:\\)[^:=]" 1
     font-lock-variable-name-face)
   '("\\<\\(open\\|include\\)\\>\\|[~?][ (]*[a-z][a-zA-Z0-9_']*"
     . font-lock-variable-name-face)
))

(defconst inferior-caml-font-lock-keywords
  (append
   (list
;inferior
    '("^[#-]" . font-lock-comment-face))
   caml-font-lock-keywords))

;; font-lock commands are similar for caml-mode and inferior-caml-mode
(add-hook 'caml-mode-hook
      '(lambda ()
         (cond
          ((fboundp 'global-font-lock-mode)
           (make-local-variable 'font-lock-defaults)
           (setq font-lock-defaults
                 '(caml-font-lock-keywords nil nil ((?' . "w") (?_ . "w")))))
          (t
           (setq font-lock-keywords caml-font-lock-keywords)))
         (make-local-variable 'font-lock-keywords-only)
         (setq font-lock-keywords-only t)
         (font-lock-mode 1)))

(defun inferior-caml-mode-font-hook ()
  (cond
   ((fboundp 'global-font-lock-mode)
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults
          '(inferior-caml-font-lock-keywords
            nil nil ((?' . "w") (?_ . "w")))))
   (t
    (setq font-lock-keywords inferior-caml-font-lock-keywords)))
  (make-local-variable 'font-lock-keywords-only)
  (setq font-lock-keywords-only t)
  (font-lock-mode 1))

(add-hook 'inferior-caml-mode-hooks 'inferior-caml-mode-font-hook)
))

;mikon --- we fool the real ocaml-mode ;)
(provide 'caml-font) 
