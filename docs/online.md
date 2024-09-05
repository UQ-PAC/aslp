# Online partial evaluation

## Background

Online partial evaluation traverses the specification,
closely mirroring concrete evaluation,
and specialises expressions as it encounters them.
It does this "online" by recording,
for each variable,
whether that variable is a concrete, symbolic, or unknown value
(in order of simplification potential).
Concrete values are simple literals which can almost always be immediately evaluated
in expressions.
Pure symbolic expressions are pure transformations of a combination
of concrete and symbolic/unknown values.
For example, this includes "(x + 1) - 1" and this can be used to simplify
using algebraic properties of integers and bitvectors.
The last type, unknown, is any computation which is opaque at partial-evaluation time.
Most often, this is register and memory accesses.
When used in an expression, this will emit code to perform the computation and store
it into a read-only variable, transforming it into a pure symbolic expression.

## Implementation

Within ASLp, the partial evaluation features are implemented by three files
with different responsibilities.
These are: symbolic.ml, dis.ml, and transforms.ml.
This is also the order from lower-level concepts (expressions)
to higher-level ones (statements and programs).

### Symbolic

In the implementation, this begins at the [symbolic.ml](/libASL/symbolic.ml)
file which decribes expression-level symbolic operations.
Here, the type of `sym` is defined simply as either
a concrete value or a pure symbolic expression
(the concept of "unknown" values lives outside this file).
Essentially, this file implements symbolic analogues
of concrete value computations ([value.ml][/libASL/value.ml])
and primitive operations ([primops.ml][/libASL/primops.ml]).
In doing so, it performs the expression-level simplification whenever
the structure of the pure expression allows. Some notable instances are:
- identities of bitwise "and" and "or" and integer "mul", "add", and "sub",
- slices of slices,
- (sign/zero) extension of extensions.

### Dis

[dis.ml](/libASL/dis.ml) performs the recursive traversal of the ASL AST,
calling functions from symbolic.ml as needed.
This file is responsible for inter-statement and inter-procedural effects,
including the collection of the residual program.

The structure in this file is designed to mirror that of the interpreter's
evaluation methods ([eval.ml](/libASL/eval.ml)).
For instance, there are corresponding functions for expressions:
```ocaml
val dis_expr : AST.l -> AST.expr -> sym rws
val eval_expr : AST.l -> Env.t -> AST.expr -> value
```
Here, the _'a rws_ is reader-writer-state monad:
```
type 'a rws = Eval.Env.t -> LocalEnv.t -> ('a, LocalEnv.t, stmt list)
```
which implements an abstraction for:
- a read-only view of globals and constants ("eval env"),
- a mutable state with a mapping of variables to their concrete value or pure symbolic expression ("local env"), and
- a write-only list of statements making up the residual program.
This mechanism is used in the code inside the `let@` and `let+` syntaxes.

### Transforms


## Grammar

## Supporting

- asl_ast (auto-generated)
- monads
- asl\_utils
- utils

