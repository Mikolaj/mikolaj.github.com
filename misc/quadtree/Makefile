# Copyright (C) 2006 Mikolaj Konarski
#
# This file is part of the IAngband rouge-like game.
# IAngband is released under the GNU General Public License (GPL).
# Please see the file IAngband-LICENSE for license information.

SOURCES := \
hashcons.mli hashcons.ml \
\
position.ml item.ml feature.ml \
\
quadtree.ml \
\
octree.ml dungeon.ml test.ml \
\
quadutils.ml quadtest.ml test_bool.ml test_old.ml

all: nc #debug-code #bc #nc #pnc
RESULT := iangband

OCAMLFLAGS := #-noassert #-dtypes 
OCAMLNCFLAGS := -inline 100

OCAMLBLDFLAGS = unix.cma
OCAMLNLDFLAGS = unix.cmxa

OCAMLC := ocamlc.opt
OCAMLOPT := ocamlopt.opt

OCAMLMAKEFILE := OCamlMakefile
-include $(OCAMLMAKEFILE)
