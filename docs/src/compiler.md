# Dolang's compiler

The second main component of Dolang is a compiler that leverages its [Symbolic
manipulation](@ref) routines to produce efficient Julia functions to evaluate
equations and systems of equations.

## `FunctionFactory`

The compiler mainly operates through one main type, the `FunctionFactory`.

```@docs
FunctionFactory
```

## `make_function`

Once an instance of `FunctionFactory` is created, a single function is used to
compile Julia a function with various methods for evaluating different orders of
derivative of the factory's expressions. This function is named `make_function` and is called

```@docs
make_function(::FunctionFactory)
```

There is also a convenience method of `make_function` that takes built-in Julia
objects as inputs, constructs the `FunctionFactory` for you, then calls the
above method:

```@docs
make_function(::Vector{Expr}, ::AbstractVector, ::AbstractVector)
```
