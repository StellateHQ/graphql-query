# graphql_query

_Stupendously fast and easy GraphQL Query Language handling._

The **graphql_query** library follows two goals:

- To support a pleasant-to-use API for the GraphQL Query Language
- To be stupendously fast at processing GraphQL Query Language ASTs

In short, _surprise!_ The `graphql_query` crate while handling a part of GraphQL does
not aim to support full, server-side GraphQL execution or the GraphQL Schema Language.
Many parts of the server-side execution of requests are one-off operations. Parsing a schema
and using it are operations that could even be preprocessed using the reference GraphQL.js
implementation.

A harder focus is to optimize how individual GraphQL requests are handled and making it easier
to write complex code on top of an easy to use AST.
GraphQL throughput is highly important in ensuring that GraphQL doesn't fall behind any other
solutions, which don't feature a rich query language.
On top, having an AST and library that's sufficiently easy to use and contains enough primitives
and utilities at the same time as valuing performance is something that's harder to do when
focusing on building a full GraphQL server.

As such, this library focuses on just processing GraphQL queries for the purpose of
intermediary GraphQL layers, which operate inbetween GraphQL clients and GraphQL servers.

[A good place to start learning more about this crate is the `ast` module...](src/ast/mod.rs)

## Sample Code

The library currently consists of utilities to parse, print, validate, visit, and transform
GraphQL Query Language ASTs.

```rust
use graphql_query::{ast::*, validate::*};

let ctx = ASTContext::new();
let ast = Document::parse(&ctx, "{ field }").unwrap();

DefaultRules::validate(&ctx, &ast).unwrap()

let output = ast.print();
```

## Performance

_How far along is this library in achieving that?_ Right on the mark.

> Benchmarks performed on an M1 Macbook Pro

**Parsing**

```
test graphql_ast_parse_graphql_query   ... bench:       2,886 ns/iter (+/- 130)
test graphql_ast_parse_graphql_parser  ... bench:      25,122 ns/iter (+/- 1,711)
test graphql_ast_parse_apollo_parser   ... bench:      36,242 ns/iter (+/- 1,062)
```

**Printing**

```
test graphql_ast_print_graphql_query  ... bench:       1,082 ns/iter (+/- 79)
test graphql_ast_print_gql_parser     ... bench:       1,137 ns/iter (+/- 48)
test graphql_ast_print_apollo_parser  ... bench:      20,861 ns/iter (+/- 518)
```

**Others**

```
test graphql_ast_fold                 ... bench:       8,466 ns/iter (+/- 768)
test graphql_ast_validate             ... bench:       1,504 ns/iter (+/- 46)
test graphql_load_introspection       ... bench:      90,265 ns/iter (+/- 4,899)
```

## What's not here

- An SDL implementation
- Schema-aware validation rules, but entirely possible with our schema_reference
- The `execute`/`collectFields` algorithm - purposefully as we focussed on the execution language
