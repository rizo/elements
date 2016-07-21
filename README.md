# Elements

A collection of small modules, types and functions that will make your life easier.

Currently the following modules are provided:

```
Elements
 ├── Base
 ├── Control
 │  ├── Exn
 │  └── Fn
 ├── Data
 │  ├── Array
 │  ├── Char
 │  ├── Counter
 │  ├── Either
 │  ├── Float
 │  ├── Int
 │  ├── Iter
 │  ├── List
 │  ├── Map
 │  ├── Option
 │  ├── Vec
 │  ├── Range
 │  ├── Result
 │  ├── Stream
 │  ├── String
 │  └── Void
 └── System
    └── IO
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


## Contributors

- Rafael Amaral ([@BrsaAmaral](https://github.com/BrsaAmaral))
- Nuno Cravino ([@ncravino](https://github.com/ncravino))

