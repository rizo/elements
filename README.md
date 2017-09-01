# Proto

Proto is a small and modular standard library alternative for OCaml.

Currently the following modules are provided:

```
Proto
 ├── Kernel
 │  ├── Int
 │  ├── Float
 │  ├── Char
 │  ├── Bool
 │  ├── Unit
 ├── Control
 │  ├── Semigroup
 │  ├── Monoid
 │  ├── Functor
 │  ├── Exception
 │  └── Function
 ├── Data
 │  ├── Array
 │  ├── Either
 │  ├── List
 │  ├── Map
 │  ├── Option
 │  ├── Result
 │  ├── Set
 │  ├── Stream
 │  ├── String
 │  └── Void
 ├── System
 │  └── IO
 └── Debug
```

## Goals

### Extensibility

Elements is designed to be easily extensible by external libraries. The `Base`
module defines the minimal set of primitives that can be used separately and
the rest of the library is built by extending the implementation of the data
types for different applications.

For example there is small `Int` module in `Base` with just a few fundamental
interfaces and operations, but, by importing the `Data` namespace, new
implementations are added, such as `Int.Map` and `Int.Set`.


### Composability

The central focus of the library is the creation and promotion of a shared
language: module interfaces, types and function signatures. The same vocabulary
is used across all the modules to ensure a composable and predictable API.


### Performance

The implementation of the library makes balanced trade-offs between usability
and performance. All the decisions are based on automatic CPU and memory
measurements, that are triggered on each build and stored for review.


## Installation

Proto can be installed with `opam`:

    opam install proto

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.


## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
proto`.

[doc]: http://rizo.odis.io/proto/doc


## License

This library is distributed under the ISC license, see full terms in the
`LICENSE.md` file.


## Contributors

- Rafael Amaral ([@BrsaAmaral](https://github.com/BrsaAmaral))
- Nuno Cravino ([@ncravino](https://github.com/ncravino))
