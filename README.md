# CompCert
The verified C compiler.

## Overview of Extension
This project adds the ability to call functions in external libraries 
by disassembling and then interpreting the disassembly inside the CompCert interpreter.

## Installation of Extension
This installation assumes a linux distribution, the installation has been (poorly) tested on Ubuntu 18.04

Install Coq `v8.9.0`, OCaml `v4.06.1`, BAP `v1.6.0` and `bap-server` using opam  
Install the `bap` Python2 package using pip

Clone this repository

    ./configure
    make
    sudo make install

If `make` fails then run `./swap` and try again, if `make` fails yet again do `make clean` and then try again.

## Running the Extended Interpreter

Run `bap-server` in a separate (or background) process

Navigate to the `dev` folder inside the CompCert project.

    CompCert/dev~$ ccomp -interp <file.c>                   # interpret a single C file
    CompCert/dev~$ ccomp -interp <file.c> -link <lib.o>     # link to external library    



## License
CompCert is not free software.  This non-commercial release can only
be used for evaluation, research, educational and personal purposes.
A commercial version of CompCert, without this restriction and with
professional support, can be purchased from
[AbsInt](https://www.absint.com).  See the file `LICENSE` for more
information.

## Copyright
The CompCert verified compiler is Copyright Institut National de
Recherche en Informatique et en Automatique (INRIA) and 
AbsInt Angewandte Informatik GmbH.


