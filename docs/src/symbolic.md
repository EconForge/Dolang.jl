# Symbolic manipulation

```@meta
DocTestSetup = quote
    using Dolang
end
```

One of Dolang's key features is providing an _extensible_ and _consistent_ set
of routines for doing operations on symbolic objects. We'll discuss each of
these routines in turn

```@contents
Pages = ["symbolic.md"]
Depth = 2
```

## `stringify`

The `stringify` function converts expressions and symbols into Dolang's internal
representation. There are various methods for this function:

```@docs
stringify(::Symbol)
```

**Examples**

```jldoctest
julia> stringify(:c)
:_c_

julia> stringify(:_c)
:__c_

julia> stringify(:_c_)
:_c_

julia> stringify(:x_ijk)
:_x_ijk_

julia> stringify(:x_ijk_)
:_x_ijk__

julia> stringify(:_x_ijk_)
:_x_ijk_
```

```@docs
stringify(::Number)
```

**Examples**

```jldoctest
julia> stringify(-1)
-1

julia> stringify(0)
0

julia> stringify(1)
1
```in

```@docs
stringify(::Symbol, ::Integer)
```

**Examples**

```jldoctest
julia> stringify(:x, 0)
:_x__0_

julia> stringify(:x, 1)
:_x__1_

julia> stringify(:x, -1)
:_x_m1_

julia> stringify(:x, -100)
:_x_m100_

julia> stringify("x", 0)
:_x__0_

julia> stringify("x", 1)
:_x__1_

julia> stringify("x", -1)
:_x_m1_

julia> stringify("x", -100)
:_x_m100_
```

```@docs
stringify(::Tuple{Symbol,Integer})
```

**Examples**

```jldoctest
julia> stringify((:x, 0))
:_x__0_

julia> stringify((:x, 1))
:_x__1_

julia> stringify((:x, -1))
:_x_m1_

julia> stringify((:x, -100))
:_x_m100_
```

```@docs
stringify(::Expr)
```

**Examples**

```jldoctest
julia> stringify(:(a(1) - b - c(2) + d(-1)))
:(((_a__1_ - _b_) - _c__2_) + _d_m1_)

julia> stringify(:(sin(x)))
:(sin(_x_))

julia> stringify(:(sin(x(0))))
:(sin(_x__0_))

julia> stringify(:(dot(x, y(1))))
:(dot(_x_, _y__1_))

julia> stringify(:(beta * c(0)/c(1) * (alpha*y(1)/k(1) * (1-mu(1)) + 1 - delta_k) - 1))
:(((_beta_ * _c__0_) / _c__1_) * ((((_alpha_ * _y__1_) / _k__1_) * (1 - _mu__1_) + 1) - _delta_k_) - 1)

julia> stringify(:(x = log(y(-1))); targets=[:x])  # with targets
:(_x_ = log(_y_m1_))

julia> stringify(:(x = log(y(-1))))  # without targets
:(log(_y_m1_) - _x_)
```

```@docs
stringify(::String)
```

**Examples**: see above for more

```jldoctest
julia> stringify("x = log(y(-1))"; targets=[:x])  # with targets
:(_x_ = log(_y_m1_))

julia> stringify("x = log(y(-1))")  # without targets
:(log(_y_m1_) - _x_)
```

```@docs
stringify(::Vector{Expr})
```

**Examples**:

```jldoctest
julia> stringify([:(sin(x(0))), :(dot(x, y(1))), :(x = log(y(-1)))])
quote
    sin(_x__0_)
    dot(_x_, _y__1_)
    log(_y_m1_) - _x_
end

julia> stringify([:(sin(x(0))), :(dot(x, y(1))), :(x = log(y(-1)))], targets=[:x])
quote
    sin(_x__0_)
    dot(_x_, _y__1_)
    _x_ = log(_y_m1_)
end
```

## `time_shift`

`time_shift` shifts an expression by a specified number of time periods. As
with stringify, there are many methods for this function, which will will
describe one at a time.

```@docs
time_shift(::Expr, ::Integer, ::Set{Symbol}, ::AbstractDict)
time_shift(::Expr, ::Integer)
```

**Examples**

```jldoctest
julia> defs = Dict(:a=>:(b(-1)/c));

julia> defs2 = Dict(:a=>:(b(-1)/c(0)));

julia> funcs = [:foobar];

julia> shift = 1;

julia> time_shift(:(a+b(1) + c), shift)
:(a + b(2) + c)

julia> time_shift(:(a+b(1) + c(0)), shift)
:(a + b(2) + c(1))

julia> time_shift(:(a+b(1) + c), shift, defs=defs)
:(b(0) / c + b(2) + c)

julia> time_shift(:(a+b(1) + c(0)), shift, defs=defs)
:(b(0) / c + b(2) + c(1))

julia> time_shift(:(a+b(1) + c(0)), shift, defs=defs2)
:(b(0) / c(1) + b(2) + c(1))

julia> time_shift(:(a+b(1) + foobar(c)), shift, functions=funcs)
:(a + b(2) + foobar(c))

julia> time_shift(:(a+b(1) + foobar(c)), shift, defs=defs, functions=funcs)
:(b(0) / c + b(2) + foobar(c))

julia> shift = -1;

julia> time_shift(:(a+b(1) + c), shift)
:(a + b(0) + c)

julia> time_shift(:(a+b(1) + c(0)), shift)
:(a + b(0) + c(-1))

julia> time_shift(:(a+b(1) + c), shift, defs=defs)
:(b(-2) / c + b(0) + c)

julia> time_shift(:(a+b(1) + c(0)), shift, defs=defs)
:(b(-2) / c + b(0) + c(-1))

julia> time_shift(:(a+b(1) + c(0)), shift, defs=defs2)
:(b(-2) / c(-1) + b(0) + c(-1))

julia> time_shift(:(a+b(1) + foobar(c)), shift, functions=funcs)
:(a + b(0) + foobar(c))

julia> time_shift(:(a+b(1) + foobar(c)), shift, defs=defs, functions=funcs)
:(b(-2) / c + b(0) + foobar(c))
```

```@docs
time_shift(::Symbol, ::Integer, ::Set{Symbol}, ::AbstractDict)
```

**Examples**

```jldoctest
julia> defs = Dict(:a=>:(b(-1)/c));

julia> defs2 = Dict(:a=>:(b(-1)/c(0)));

julia> shift = 1;

julia> funcs = Set([:foobar]);

julia> time_shift(:a, shift, funcs, Dict())
:a

julia> time_shift(:a, shift, funcs, defs)
:(b(0) / c)

julia> time_shift(:a, shift, funcs, defs2)
:(b(0) / c(1))

julia> time_shift(:b, shift, funcs, defs)
:b

julia> shift = -1;

julia> time_shift(:a, shift, funcs, Dict())
:a

julia> time_shift(:a, shift, funcs, defs)
:(b(-2) / c)

julia> time_shift(:a, shift, funcs, defs2)
:(b(-2) / c(-1))

julia> time_shift(:b, shift, funcs, defs)
:b
```

```@docs
time_shift(::Number)
```

**Examples**

```jldoctest
julia> time_shift(1)
1

julia> time_shift(2)
2

julia> time_shift(-1)
-1

julia> time_shift(-2)
-2
```

## `steady_state`

The `steady_state` function will set the period for all "timed" variables to 0.

```@docs
steady_state(::Symbol, ::Set{Symbol}, ::AbstractDict)
```

```jldoctest
julia> defs = Dict(:a=>:(b(-1)/c + d), :d=>:(exp(b(0))));

julia> steady_state(:c, Set{Symbol}(), defs)
:c

julia> steady_state(:d, Set{Symbol}(), defs)
:(exp(b))

julia> steady_state(:a, Set{Symbol}(), defs)  # recursive def resolution
:(b / c + exp(b))

julia> steady_state(1, Set{Symbol}(), defs)
1

julia> steady_state(-1, Set{Symbol}(), defs)
-1
```

**Examples**


```@docs
steady_state(::Expr, ::Set{Symbol}, ::AbstractDict)
steady_state(::Expr)
```

**Examples**

```jldoctest
julia> steady_state(:(a+b(1) + c))
:(a + b + c)

julia> steady_state(:(a+b(1) + c))
:(a + b + c)

julia> steady_state(:(a+b(1) + c), defs=Dict(:a=>:(b(-1)/c)))
:(b / c + b + c)

julia> steady_state(:(a+b(1)+c+foobar(c)))
ERROR: Dolang.UnknownFunctionError(:foobar, "Unknown function foobar")

julia> steady_state(:(a+b(1) + foobar(c)), functions=[:foobar])
:(a + b + foobar(c))
```

## `list_symbols`

`list_symbols` will walk an expression and determine which symbols are used as
potentially time varying *variables* and which symbols are used as static
*parameters*.

```@docs
list_symbols!(out, ::Expr, ::Set{Symbol})
list_symbols(::Expr)
```

**Examples**

```jldoctest
julia> list_symbols(:(a + b(1) + c))
Dict{Symbol,Any} with 2 entries:
  :variables  => Set(Tuple{Symbol,Int64}[(:b, 1)])
  :parameters => Set(Symbol[:a, :c])

julia> list_symbols(:(a + b(1) + c + b(0)))
Dict{Symbol,Any} with 2 entries:
  :variables  => Set(Tuple{Symbol,Int64}[(:b, 1), (:b, 0)])
  :parameters => Set(Symbol[:a, :c])

julia> list_symbols(:(a + b(1) + c + b(0) + foobar(x)))
ERROR: Dolang.UnknownFunctionError(:foobar, "Unknown function foobar")

julia> list_symbols(:(a + b(1) + c + b(0) + foobar(x)), functions=Set([:foobar]))
Dict{Symbol,Any} with 2 entries:
  :variables  => Set(Tuple{Symbol,Int64}[(:b, 1), (:b, 0)])
  :parameters => Set(Symbol[:a, :c, :x])
```

## `list_variables`

`list_variables` will return the `:variables` key that results from calling
`list_symbols`. The type of the returned object will be a `Set` of
`Tuple{Symbol,Int}`, where each item describes the symbol that appeared and the
corresponding date.

```@docs
list_variables(:Any)
```

**Examples**

```jldoctest
julia> list_variables(:(a + b(1) + c))
Set(Tuple{Symbol,Int64}[(:b, 1)])

julia> list_variables(:(a + b(1) + c + b(0)))
Set(Tuple{Symbol,Int64}[(:b, 1), (:b, 0)])

julia> list_variables(:(a + b(1) + c + b(0) + foobar(x)))
ERROR: Dolang.UnknownFunctionError(:foobar, "Unknown function foobar")

julia> list_variables(:(a + b(1) + c + b(0) + foobar(x)), functions=Set([:foobar]))
Set(Tuple{Symbol,Int64}[(:b, 1), (:b, 0)])
```

## `list_parameters`

`list_parameters` will return the `:parameters` key that results from calling
`list_symbols`. The returned object will be `Set{Symbol}` where each item
represents the symbol that appeared without a date.

```@docs
list_parameters(::Any)
```

**Examples**

```jldoctest
julia> list_variables(:(a + b(1) + c + b(0) + foobar(x)), functions=Set([:foobar]))
Set(Tuple{Symbol,Int64}[(:b, 1), (:b, 0)])

julia> list_parameters(:(a + b(1) + c))
Set(Symbol[:a, :c])

julia> list_parameters(:(a + b(1) + c + b(0)))
Set(Symbol[:a, :c])

julia> list_parameters(:(a + b(1) + c + b(0) + foobar(x)))
ERROR: Dolang.UnknownFunctionError(:foobar, "Unknown function foobar")

julia> list_parameters(:(a + b(1) + c + b(0) + foobar(x)), functions=Set([:foobar]))
Set(Symbol[:a, :c, :x])
```

## `subs`

`subs` will replace a symbol or expression with a different value, where the
value has type `Union{Symbol,Expr,Number}`

The first method of this function is very specific, and matches only a
particular pattern:

```@docs
subs(::Union{Expr,Symbol,Number}, from, ::Union{Symbol,Expr,Number}, ::Set{Symbol})
```

**Examples**

```jldoctest
julia> subs(:(a + b(1) + c), :a, :(b(-1)/c + d), Set{Symbol}())
:((b(-1) / c + d) + b(1) + c)

julia> subs(:(a + b(1) + c), :d, :(b(-1)/c + d), Set{Symbol}())
:(a + b(1) + c)
```

```@docs
subs(::Expr, ::AbstractDict, ::Set{Symbol})
subs(::Expr, ::AbstractDict)
```

**Examples**

```jldoctest
julia> ex = :(a + b);

julia> d = Dict(:b => :(c/a), :c => :(2a));

julia> subs(ex, d)  # subs is not recursive -- c is not replaced
:(a + c / a)
```

## `csubs`

`csubs` is closely related to `subs`, but is more Clever and applies
substitutions recursively.

```@docs
csubs(::Expr, ::AbstractDict, ::Set{Symbol})
csubs(::Expr, ::AbstractDict)
```

**Examples**

```jldoctest
julia> ex = :(a + b);

julia> d = Dict(:b => :(c/a), :c => :(2a));

julia> csubs(ex, d)  # csubs is recursive -- c is replaced
:(a + (2a) / a)

julia> d1 = Dict(:monty=> :python, :run=>:faster, :eat=>:more);

julia> csubs(:(monty(run + eat, eat)), d1)
:(python(faster + more, more))

julia> csubs(:(a + b(0) + b(1)), Dict(:b => :(c(0) + d(1))))
:(a + (c(0) + d(1)) + (c(1) + d(2)))

julia> csubs(:(a + b(0) + b(1)), Dict((:b, 1) => :(c(0) + d(1))))
:(a + b(0) + (c(0) + d(1)))

julia> csubs(:(a + b(0) + b(1)), Dict(:b => :(c + d(1))))
:(a + (c + d(1)) + (c + d(2)))

julia> csubs(:(a + b(0) + b(1)), Dict((:b, 1) => :(c + d(1))))
:(a + b(0) + (c + d(1)))

julia> d = Dict((:b, 0) => :(c + d(1)), (:b, 1) => :(c(100) + d(2)));

julia> csubs(:(a + b + b(1)), d)
:(a + (c + d(1)) + (c(100) + d(2)))
```
