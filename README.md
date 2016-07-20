# TLAPM

This is the next version of the TLA proof manager, which is part of the
 TLA+ Tools [1]. Since is still under development, please also refer to the
 current version[2].

[1] https://tlaplus.codeplex.com
[2] https://tlaps.codeplex.com


## Installation
The compilation requirements are:

* a recent version of ocaml (at  least 4.02)
* xmlm (http://erratique.ch/software/xmlm)
* kaputt (http://kaputt.x9c.fr)
* oasis (http://oasis.forge.ocamlcore.org)
* result (if you are running ocaml < 4.03 )
* sexplib

The easiest way to install them is via opam (https://opam.ocaml.org):

opam install oasis xmlm kaputt result sexplib

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
