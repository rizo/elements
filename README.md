proto — Small and modular standard library alternative.
-------------------------------------------------------------------------------
%%VERSION%%

proto is TODO

proto is distributed under the ISC license.

Homepage: https://github.com/rizo/proto  

## Installation

proto can be installed with `opam`:

    opam install proto

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
proto`.

[doc]: http://rizo.odis.io/proto/doc

## Sample programs

If you installed proto with `opam` sample programs are located in
the directory `opam var proto:doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory. They can be built and run
with:

    topkg build --tests true && topkg test 
