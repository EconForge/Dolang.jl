# Examples

Let's show some examples of how use Dolang to compile simple Julia functions.

## Basic example

First, let's construct some equations, arguments, and parameters

```@example ff1
using Dolang
eqs = [
    :(foo(0) = log(a(0))+b(0)/x(-1)),
    :(bar(0) = c(1)+u*d(1))
]
args = [(:a, -1), (:a, 0), (:b, 0), (:c, 0), (:c, 1), (:d, 1)]
params = [:u]
defs = Dict(:x=>:(a(0)/(1-c(1))))
targets = [(:foo, 0), (:bar, 0)]

ff = FunctionFactory(
    eqs, args, params, targets=targets, defs=defs, funname=:myfun
)
```

Now that we have our `FunctionFactory` we can generate some code (warning, the
generated code is *not* pretty):

```@example ff1
code = make_function(ff)
print(code) # hide
```

In order to actually call methods of `myfun` we need to call `eval` on our `code`:

```@example ff1
eval(code)
```

And now we can call it

```@example ff1
V = [1.5, 2.5, 1.0, 2.0, 3.0, 1.1]
p = [0.5]
myfun(V, p)
```

We can call the vectorized version:

```@example ff1
myfun([V V]', p)
```

Or the mutating one:

```@example ff1
out = zeros(2)
myfun!(out, V, p)
out
```

We can evaluate the Jacobian (first derivative matrix)

```@example ff1
myfun(Der{1}, V, p)
```

... or the Hessian

```@example ff1
myfun(Der{2}, V, p)
```

... or higher order derivatives

```@example ff1
myfun(Der{3}, V, p)
```

```@example ff1
myfun(Der{10}, V, p)
```

Note that the Hessian is returned in as a `SparseMatrixCSC` where the columns
are ordered with the first variable increasing faster. In this example, the
`(1, 4)` element of the Hessian will be the second partial derivative of the
first equation (the `1`) with fourth and first variables in `args`.

For all higher order derivatives the return value is a
`Vector{Dict{NTuple{N,Int}, Float64}}`, where `N` is the order of derivative.
The keys of each dict will be non-increasing so mixed partials are only
computed and stored once. Notice that for the order `3` derivatives an entry
for `(1, 1, 3)` appears, but not for `(1, 3, 1)` or `(3, 1, 1)`. The user of
these routines is required to handle the [equality of
partials](https://calculus.subwiki.org/wiki/Clairaut%27s_theorem_on_equality_of_mixed_partials).

## With `dispatch`

Now lets consider an example that leverages the `dispatch` argument. In this
case we will use the convenience method for make function with signature:
[`make_function(::Vector{Expr}, ::AbstractVector, ::AbstractVector)`](@ref). We
also will not show the compiled code

```@example ff2
using Dolang  # hide
eqs = [
    :(sin(x(0)) + exp(2*x(1))),
    :(y(0) / (2 * (1 - β))),
]
# NOTE: Arguments can be pre-normalized and contain unicode
variables = [(:x, 0), (:y, 0), :_x__1_, :β]  
to_diff = 1:3
code = make_function(eqs, variables, to_diff, dispatch=Int, name=:my_int_fun)
eval(code)
```

Let's check the methods of `my_int_fun` and `my_int_fun!`:

```@example ff2
methods(my_int_fun)
```
```@example ff2
methods(my_int_fun!)
```

Notice that because we set the `dispatch` argument to `Int`, all methods of
either the mutating or allocating function require that an `Int` is passed to
direct dispatch.

Let's try calling our function

```julia
V = [π, 5.0, 0.8]
p = [0.98]
my_int_fun(V, p)  # doesn't work
```

The above fails with

```
ERROR: MethodError: no method matching my_int_fun(::Array{Float64,1}, ::Array{Float64,1})
```

Now if we call that method where the first argument is an *instance* of `Int`,
it will work

```@example ff2
V = [π, 5.0, 0.8]  # hide
p = [0.98]  # hide
my_int_fun(1, V, p)
```

The actual integer we pass doesn't matter

```@example ff2
my_int_fun(10_000_001, V, p)
```

We can still evaluate derivatives

```@example ff2
my_int_fun(Der{1}, 1, V, p)
```

Notice that the order of derivative comes first, then the dispatch argument,
then all other function arguments.

Higher order derivatives also work

```@example ff2
my_int_fun(Der{2}, 10, V, p)
```

```@example ff2
my_int_fun(Der{5}, 10, V, p)
```

```@example ff2
my_int_fun(Der{10}, 10, V, p)
```

## With grouped `args`

Finally we show one more example of how to use an associative mapping for the
`FunctionFactory` `args` field to produce a function with grouped arguments.

```@example ff3
using Dolang  # hide
eqs = [
    :(sin(x(0)) + exp(2*x(1))),
    :(y(0) / (2 * (1 - β))),
]
args = Dict(
    :a => [(:x, 0), (:y, 0)],
    :b => [(:x, 1)]
)
params = [:β]
ff = FunctionFactory(eqs, args, params, funname=:grouped_args)
code = make_function(ff)
eval(code)
```

Let's take a look at the methods for this function

```@example ff3
methods(grouped_args)
```

Notice that each of theses routines have arguments for `a`, `b`, and `p`
instead of just the `V` and `p` we saw in previous examples.

When we evaluate these methods we need to be sure that `a` has two elements,
`b` has one, and `p` has one:

```@example ff3
a = [π, 5.0]
b = [0.8]
p = [0.98]
grouped_args(a, b, p)
```

Notice we can evaluate a method where `a` and `b` are vectorized. Here `p` will
be repeated as necessary

```@example ff3
grouped_args([a a+1]', [b b+1]', p)
```

We can also evaluate a partially vectorized version of the function where only
`a` is a matrix and `b` is a vector. In this case both `b` and `p` will be
repeated

```@example ff3
grouped_args([a a+1]', b, p)
```

Non-allocating versions of the function are also available

```@example ff3
out = zeros(2)
grouped_args!(out, a, b, p)
@show @allocated grouped_args!(out, a, b, p)
@show out
```

As of today (2017-06-13) derivative code does not work for functions with
grouped args. Trying to evaluate a derivative will result in an error that
looks like this:

```
julia> grouped_args(Der{1}, a, b, p)
ERROR: MethodError: no method matching equation_block(::Dolang.FunctionFactory{Dict{Symbol,Array{Tuple{Symbol,Int64},1}},Array{Symbol,1},Dict{Symbol,Any},DataType}, ::Type{Dolang.Der{1}})
```
