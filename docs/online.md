# Online partial evaluation

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

The overall flow within ASLp is:
- symbolic
- dis
- transforms

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


