%% 
%% This is file `rinf.cls', generated on <1995/12/12> 
%% with the docstrip utility (2.2h).
%% 
%% The original source files were:
%% 
%% rinf.dtx  (with options: `rinf')
%% 
%% IMPORTANT NOTICE:
%% You are not allowed to distribute this file.
%% For distribution of the original source see
%% the copyright notice in the file rinf.dtx .
%% 
%% \CharacterTable
%%  {Upper-case    \A\B\C\D\E\F\G\H\I\J\K\L\M\N\O\P\Q\R\S\T\U\V\W\X\Y\Z
%%   Lower-case    \a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z
%%   Digits        \0\1\2\3\4\5\6\7\8\9
%%   Exclamation   \!     Double quote  \"     Hash (number) \#
%%   Dollar        \$     Percent       \%     Ampersand     \&
%%   Acute accent  \'     Left paren    \(     Right paren   \)
%%   Asterisk      \*     Plus          \+     Comma         \,
%%   Minus         \-     Point         \.     Solidus       \/
%%   Colon         \:     Semicolon     \;     Less than     \<
%%   Equals        \=     Greater than  \>     Question mark \?
%%   Commercial at \@     Left bracket  \[     Backslash     \\
%%   Right bracket \]     Circumflex    \^     Underscore    \_
%%   Grave accent  \`     Left brace    \{     Vertical bar  \|
%%   Right brace   \}     Tilde         \~}
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{rinf}
              [1995/12/11 v0.2
 IInf UW Tech Rep document class]
\DeclareOption*{%
\PassOptionsToClass{\CurrentOption}{article}
}
\ProcessOptions
\LoadClass[a4paper]{article}
\input{rin1\@ptsize.clo}
\if@twoside
    \setlength\@tempdima        {\paperwidth}
    \addtolength\@tempdima      {-\textwidth}
    \setlength\oddsidemargin    {.4\@tempdima}
    \addtolength\oddsidemargin  {-1in}
    \setlength\marginparwidth   {.6\@tempdima}
    \addtolength\marginparwidth {-\marginparsep}
    \addtolength\marginparwidth {-0.4in}
  \else
    \setlength\@tempdima        {\paperwidth}
    \addtolength\@tempdima      {-\textwidth}
    \setlength\oddsidemargin    {.5\@tempdima}
    \addtolength\oddsidemargin  {-1in}
    \setlength\marginparwidth   {.5\@tempdima}
    \addtolength\marginparwidth {-\marginparsep}
    \addtolength\marginparwidth {-0.4in}
    \addtolength\marginparwidth {-.4in}
  \fi
  \@settopoint\oddsidemargin
  \@settopoint\marginparwidth
  \setlength\evensidemargin  {\paperwidth}
  \addtolength\evensidemargin{-2in}
  \addtolength\evensidemargin{-\textwidth}
  \addtolength\evensidemargin{-\oddsidemargin}
  \@settopoint\evensidemargin
\ClassInfo{rinf}{odd side margin set to \the\oddsidemargin}
\ClassInfo{rinf}{even side margin set to \the\evensidemargin}
\def\ps@myheadings{%
    \def\@oddhead{\hbox{}{\small {\sl Warsaw University \hfill
    Institute of Informatics}}}
    \def\@oddfoot{{\small {\sl \@date} \hfill \thepage \hfill \tr}}
    \def\@evenhead{{\small \sl Institute of Informatics \hfill Warsaw University
    \hbox{}}}
    \def\@evenfoot{\tr \hfill \rm \thepage \hfill {\small \sl \@date}}
    \let\@mkboth\@gobbletwo
    \let\sectionmark\@gobble
    \let\subsectionmark\@gobble
    }
\iffalse
\newcommand\contentsname{Contents}
\newcommand\listfigurename{List of Figures}
\newcommand\listtablename{List of Tables}
\newcommand\refname{References}
\newcommand\indexname{Index}
\newcommand\figurename{Figure}
\newcommand\tablename{Table}
\newcommand\partname{Part}
\newcommand\appendixname{Appendix}
\newcommand\abstractname{Abstract}
\fi
\renewcommand\today{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi,\space
  \number\year}
\endinput
%% 
%% End of file `rinf.cls'.
