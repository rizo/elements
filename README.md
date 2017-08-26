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


## Naming Conventions

| Item               | Convention                          | Examples                                       |
|--------------------|-------------------------------------|------------------------------------------------|
| Values             | Short lowercase snake case          | `x`, `xs'`, `req`, `body`, `nitems`            |
| Types              | Lowercase snake case                | `int`, `bytes`, `identifier`, `status_code`    |
| Types parameters   | Preferably single-letter snake case | `'a list`, `('a, 'e) Result.t`, `'token lexer` |
| Modules            | Capitalized snake case              | `Stream_writer`, `HTTP_request`, `File_path`   |
| Module types       | Capitalized snake case              | `Default,` `Equal`, `Monad`, `Parse`, `Show`   |
| Constructors       | Capitalized snake case              | `Ok`, `Empty_response`, `If_expression`        |
| Functions          | Lowercase snake case                | `say_hello`, `print`, `http_handler`, `show`   |
| Type initializer   | Lowercase snake case                | `Request.init`, `Array.with_capacity`          |
| Module initializer | Capitalized snake case              | `Map.Make`, `Functor.With_monad`               |




## Concepts

A concept is an interfaces (_.i.e._ a _module type_ in OCaml's jargon) that encodes a certain functionality a particular type has. Concepts can be used to abstract over behavior that types can have in common. For example the `Default` concept requires a type to provide a default value, like `0` for integers and `[]` for lists.

Alternative names: concepts, protocols, traits, classes


## Contributors

- Rafael Amaral ([@BrsaAmaral](https://github.com/BrsaAmaral))
- Nuno Cravino ([@ncravino](https://github.com/ncravino))

