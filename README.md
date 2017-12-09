# TLAPM

[![Build Status](https://travis-ci.org/tlaplus/v2-tlapm.svg?branch=master)](https://travis-ci.org/tlaplus/v2-tlapm)

This is the next version of the TLA proof manager, which is part of the
 TLA+ Tools [1]. Since is still under development, please also refer to the
 current version[2].

[1] https://github.com/tlaplus/tlaplus

[2] https://tlaps.codeplex.com


## Installation
The easiest way to install is via opam:

    opam pin add tlapm2 https://github.com/tlaplus/v2-tlapm.git

If it is already pinned but not installed a simple

    opam install tlapm2

will suffice.

## Compilation
The compilation requirements are:

* a recent version of ocaml (at  least 4.02)
* containers (https://github.com/c-cube/ocaml-containers)
* xmlm (http://erratique.ch/software/xmlm)
* kaputt (http://kaputt.x9c.fr) (optional)
* oasis (http://oasis.forge.ocamlcore.org)
* result (if you are running ocaml < 4.03 )
* sexplib

The easiest way to install them is via opam (https://opam.ocaml.org):

opam install containers oasis xmlm kaputt result sexplib

To initialize the configuration, call

    oasis setup ;
    ./configure --enable-tests --enable-debug

in the v2-tlapm base directory. For the actual compilation, the usual

    make

will compile the project, wheras

    make test

will run the test suite.

## Running

For execution, a Java Runtime Environment (at least 7) is also necessary.
