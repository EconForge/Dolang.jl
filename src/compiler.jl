# --------- #
# Utilities #
# --------- #

_is_not_line(x) = true
_is_not_line(::LineNumberNode) = false
_is_not_line(ex::Expr) = ex.head != :line

const DISPATCH_ARG = gensym(:dispatch)

_filter_lines!(x) = x

function _filter_lines!(ex::Expr)
    filter!(_is_not_line, ex.args)  # filter these args
    map(_filter_lines!, ex.args)    # then map over any nested Expr
    ex
end

_filter_lines(ex::Expr) = _filter_lines!(deepcopy(ex))

"Convert lhs = rhs to rhs - lhs. Used in computation of derivatives"
_rhs_only(ex::Expr) =
    ex.head == :(=) ? Expr(:call, :(-), ex.args[2], ex.args[1]) : ex

function _unpack_expr(names::Vector, rhs::Symbol)
    _args = [:($(normalize(names[i])) = Dolang._unpack_var($rhs, $i))
             for i in 1:length(names)]
    out = Expr(:block); out.args = _args; out
end

function _unpack_expr(d::Associative, _discard)
    _args = vcat([_unpack_expr(v, k) for (k, v) in d]...)
    out = Expr(:block); out.args = _args; out
end

"returns an expression `:(lhs[i] = rhs)`"
_assign_var_expr(lhs, rhs, i) = :(Dolang._assign_var($lhs, $rhs, $i))

# ----------------- #
# Expression Blocks #
# ----------------- #

#=
Each function is composed of multiple blocks. Each of these blocks is
associated with a function that can be overloaded to customize behavior

For a function that allocates memory for the output and returns the alloacted
array, we have the following blocks (in this order):

1. `allocate_block`: Allocates memory to hold the output of the evaluated
equations. Memory is bound to a variable named `out`
2. `param_block`: Unpacks items from the `params` field
3. `arg_block`: Unpacks items from the `arg` field
4. `equation_block`: uses the now locally defined variables from params and
args to evaluate the equations
5. `return_block`: Simply returns `out`

For a mutating function that populates a pre-allocated array with the value
of the function at specified values for the args and params we have:

1. `sizecheck_block`: Checks that the size of the `out` argument that was
passed into the function is conformable with the input args and parameters and
the equations.
2. `param_block`: Unpacks items from the `params` field
3. `arg_block`: Unpacks items from the `arg` field
4. `equation_block`: uses the now locally defined variables from params and
args to evaluate the equations
5. `return_block`: Simply returns `out`

In both cases steps 2-5 are the same and are called the `body_block`

The `allocate_block`, `size_checkblock`, and `equation_block` can all depend
on the order of derivative to be computed. For that reason, the corresponding
functions all have the signature `func{n}(::FunctionFactory, ::TDer{n})`. To
implement the body of a function higher order derivatives, you only need to
provide methods for these functions. Also, each of them has the second argument
defaulting to `Der{0}`, so calling `func(ff)` will return the 0th order
derivative (or level) version of that block.

=#

"Expression that allocates memory for variable `out` in non-mutating version"
allocate_block(ff::FunctionFactory, ::TDer{0}=Der{0}) =
    :(out = Dolang._allocate_out(eltype($(arg_names(ff)[1])), $(length(ff.eqs)),
                                 $(arg_names(ff)...)))

"Expression that checks the size of `out` in mutating version"
function sizecheck_block(ff::FunctionFactory, ::TDer{0}=Der{0})
    ex = quote
        expected_size = Dolang._output_size($(length(ff.eqs)), $(arg_names(ff)...))
        if size(out) != expected_size
            msg = "Expected out to be size $(expected_size), found $(size(out))"
            throw(DimensionMismatch(msg))
        end
    end
    _filter_lines!(ex)
    ex
end

param_block(ff::FunctionFactory, vec::Symbol=:p) =
    _unpack_expr(ff.params, vec)

arg_block(ff::FunctionFactory, vec::Symbol=:V) = _unpack_expr(ff.args, vec)

"Evaluates main expressions in a function group and fills `out` with results"
function equation_block(ff::FunctionFactory, ::TDer{0}=Der{0})
    n_expr = length(ff.eqs)
    func_block = Expr(:block)

    if isempty(ff.targets)
        # no targets, just set out = parsed_expr
        assignments = map((rhs, i) -> _assign_var_expr(:out, rhs, i),
                          ff.eqs, 1:n_expr)
        func_block.args = assignments
    else
        # otherwise, need to parse the targets, evaluate them, and then set
        # elements of out equal to the targets
        parsed_targets = map(normalize, ff.targets)
        assignments = map((rhs, i) -> _assign_var_expr(:out, rhs, i),
                          parsed_targets, 1:n_expr)
        func_block.args = vcat(ff.eqs, assignments)
    end

    return func_block
end

function body_block{n}(ff::FunctionFactory, ::TDer{n})
    _args = [param_block(ff), arg_block(ff), equation_block(ff, Der{n}),
             :(return out)]
    out = Expr(:block); out.args = _args; out
end

# ------------------- #
# Function Signatures #
# ------------------- #

#=
In addition to the function blocks dicussed above, we also need to know the
signature of each function so it can be defined.

The signature of the generated function for `ff::FunctionFactory` has the
following structure:

`ff.funname([DERIVATIVE], [DISPATCH], arg_names(ff)..., param_names(ff)...)`

Let's take it once piece at a time:

- `ff.funname` is the provided function name
- `DERIVATIVE` has the form `::Type{Dolang.Der{N}}`, where `N` is meant to
specify the order(s) of the derivative to be evaluated. This allows you to use
the same function name, but control which order of derivative is evaluated by
passing `Der{N}` as the first argument to `ff.funname`. If `N == 0`, this
section of the signature is skipped.
- `DISPATCH` has the form `::Type{ff.dispatch}` where `ff.dispatch` should be
a Julia `DataType`. This is used to create many methods for same function (i.e.
mulitple versions of the function with the same name), but have them be
distinguishable to the Julia compiler. See example usage to see how it works.
By default `ff.dispatch` is set to `Dolang.SkipArg`. When
`ff.dispatch == SkipArg`, the compiler completely skips the `[DISPATCH]`
section of the signature
- `arg_names(ff)...` is simply the name of the arguments from `ff.args`. If
`ff.args` is a `Vector` (more specifically a `Dolang.FlatArgs`), then this will
be `[:V]`. If `ff.args` is some `Associative` structure, then this will be the
keys of that structure.
- `param_names(ff)` is the same as `arg_names(ff)`, but applied to the
`ff.params` field

We also need a signature for the mutating version of the signature. This has
the structure

`ff.funname!([DERIVATIVE], [DISPATCH], out, arg_names(ff)..., param_names(ff)...)`

Everything is the same as above, except that `ff.funname!` is now the original
function name with `!` appended to it and there is an additional `out`
argument. This is the array that should be filled with the evaluated equations
and always comes _after_ arguments that drive dispatch (`DERIVATIVE` and
`DISPATCH`), but _before_ args and params.

=#

arg_names{T1<:FlatArgs}(::FunctionFactory{T1}) = [:V]

# WARNING: This function will only work in a reliable way if the keys are
#          guaranteed to come out in the same order each time. To ensure this
#          we reccomend using an instance of `OrderedDict` from the
#          DataStructures.jl package instead of a `Dict` from Base.
arg_names{T1<:GroupedArgs}(ff::FunctionFactory{T1}) =
    collect(keys(ff.args))::Vector{Symbol}

param_names{T1,T2<:FlatParams}(::FunctionFactory{T1,T2}) = [:p]

param_names{T1,T2<:GroupedParams}(ff::FunctionFactory{T1,T2}) =
    collect(keys(ff.params))::Vector{Symbol}

_extra_args{n}(ff::FunctionFactory, d::TDer{n}) =
    ff.dispatch == SkipArg ? Any[:(::Dolang.TDer{$n})] :
                                [:(::Dolang.TDer{$n}),
                                 :($(DISPATCH_ARG)::$(ff.dispatch))]

"Method signature for non-mutating version of the function"
function signature{n}(ff::FunctionFactory, d::TDer{n}=Der{0})
    func_args = vcat(ff.funname, _extra_args(ff, d),
                     arg_names(ff), param_names(ff))

    out = Expr(:call)
    out.args = func_args
    out
end

"Method signature for mutating version of the function"
function signature!{n}(ff::FunctionFactory, d::TDer{n}=Der{0})
    sig = signature(ff, d)

    # convert name to `!` version and insert `out` as first argument
    sig.args[1] = Symbol(sig.args[1], "!")
    insert!(sig.args, length(_extra_args(ff, d))+2, :out)
    sig
end

# ---------------- #
# First derivative #
# ---------------- #

# first we need a couple of helper methods

function _jacobian_expr_mat(ff::FunctionFactory{FlatArgs})
    # NOTE: I'm starting with the easy version, where I just differentiate
    #       with respect to all arguments in the order they were given. It
    #       would be better if I went though `ff.incidence.by_var` and only
    #       made columns for variables that appear in the equations
    args = ff.args
    neq = length(ff.eqs)
    nvar = nargs(ff)

    exprs = Array(Union{Symbol,Expr,Number}, neq, nvar)
    fill!(exprs, 0)

    non_zero = 0
    for i_eq = 1:neq
        eq = _rhs_only(ff.eqs[i_eq])
        eq_prepped = prep_deriv(eq)
        eq_incidence = ff.incidence.by_eq[i_eq]

        for i_var in 1:nvar
            v, shift = args[i_var]

            if haskey(eq_incidence, v) && in(shift, eq_incidence[v])
                non_zero += 1
                my_deriv = deriv(eq_prepped, normalize((v, shift)))
                exprs[i_eq, i_var] = post_deriv(my_deriv)
            end
        end
    end
    exprs, non_zero
end

_output_size(ff::FunctionFactory, ::TDer{1}) =
    (length(ff.eqs), nargs(ff))

# Now fill in FunctionFactory API
function allocate_block{n}(ff::FunctionFactory, d::TDer{n})
    expected_size = _output_size(ff, d)
    :(out = zeros(Float64, $(expected_size)))
end

function sizecheck_block{n}(ff::FunctionFactory, d::TDer{n})
    expected_size = _output_size(ff, d)
    ex = quote
        if size(out) != $expected_size
            msg = "Expected out to be size $($(expected_size)), found $(size(out))"
            throw(DimensionMismatch(msg))
        end
        # populate with zeros, because we assume everything is zeroed and
        # only fill in non-zero elements
        fill!(zero(eltype), out)
    end
    _filter_lines!(ex)
    ex
end

function equation_block(ff::FunctionFactory{FlatArgs}, ::TDer{1})
    expr_mat, non_zero = _jacobian_expr_mat(ff)
    neq = size(expr_mat, 1)
    nvar = size(expr_mat, 2)

    # construct expressions that define the body of this function.
    # we need neq*nvar of them
    expr_args = Array(Expr, non_zero)

    # To do this we use linear indexing tricks to access `out` and `expr_mat`.
    # Note the offset on the index to expr_args also (needed because allocating)
    # is the first expression in the block
    ix = 0
    for ii in eachindex(expr_mat)
        if expr_mat[ii] != 0
            expr_args[ix+=1] = :(out[$(ii)] = $(expr_mat[ii]))
        end
    end

    out = Expr(:block)
    out.args = expr_args
    out
end

# ----------------- #
# Second derivative #
# ----------------- #

# NOTE: allocations for the hessian are done in in the equation_block because
#       it requires us to know the number of non-zero hessian terms, which we
#       only know after we have constructed them.
allocate_block(ff::FunctionFactory, ::TDer{2}) = nothing
sizecheck_block(ff::FunctionFactory, ::TDer{2}) = nothing

function _hessian_exprs(ff::FunctionFactory{FlatArgs})
    # NOTE: I'm starting with the easy version, where I just differentiate
    #       with respect to all arguments in the order they were given. It
    #       would be better if I went though `ff.incidence.by_var` and only
    #       made columns for variables that appear in the equations
    neq = length(ff.eqs)
    nvar = nargs(ff)

    # To do this we use linear indexing tricks to access `out` and `expr_mat`.
    # Note the offset on the index to expr_args also (needed because allocating)
    # is the first expression in the block
    terms = Array(Tuple{Int,Tuple{Int,Int},Union{Expr,Symbol,Number}},0)
    for i_eq in 1:neq
        ex = _rhs_only(ff.eqs[i_eq])
        eq_prepped = prep_deriv(ex)
        eq_incidence = ff.incidence.by_eq[i_eq]

        for i_v1 in 1:nvar
            v1, shift1 = ff.args[i_v1]

            if haskey(eq_incidence, v1) && in(shift1, eq_incidence[v1])
                diff_v1 = deriv(eq_prepped, normalize((v1, shift1)))

                for i_v2 in i_v1:nvar
                    v2, shift2 = ff.args[i_v2]

                    if haskey(eq_incidence, v2) && in(shift2, eq_incidence[v2])
                        diff_v1v2 = deriv(diff_v1, normalize((v2, shift2)))

                        # might still be zero if terms were independent
                        if diff_v1v2 != 0
                            push!(terms, (i_eq, (i_v1, i_v2), post_deriv(diff_v1v2)))
                        end
                    end
                end
            end
        end
    end

    return terms
end

# Ordering of hessian is H[eq, (v1,v2)]
function equation_block(ff::FunctionFactory{FlatArgs}, ::TDer{2})
    exprs = _hessian_exprs(ff)
    n_expr = length(exprs)
    nvar = nargs(ff)

    n_terms = 0
    for e in exprs
        # if indices are the same, term is for (v[i], v[i]), so it only appears
        # one time. Otherwise, we need to account for symmetry and have two
        # terms
        n_terms += e[2][1] == e[2][2] ? 1 : 2
    end

    # create expressions that define `_val_i`, which holds the numerical value
    # associated with the `i`th expression
    val_exprs = [Expr(:(=), Symbol("_val_$(i)"), exprs[i][3]) for i in 1:n_expr]
    vals = Expr(:block)
    vals.args = val_exprs

    # create expressions that fill in the correct elements of i, j, v based
    # on the data in `vals` and the indices in `exprs`
    pop_exprs = Array(Expr, n_terms)
    ix = 0
    for i_expr in 1:n_expr
        i_eq, (i_v1, i_v2), _ = exprs[i_expr]

        # we definitely need to fill the `i_eq, (i_v1, i_v2)` element
        ix += 1

        # value of j
        j = sub2ind((nvar, nvar), i_v1, i_v2)
        pop_exprs[ix] = Expr(:block,
            :(setindex!(i, $(i_eq), $(ix))),
            :(setindex!(j, $(j), $(ix))),
            :(setindex!(v, $(Symbol("_val_$(i_expr)")), $(ix)))
        )

        if i_v1 != i_v2
            # here we also need to fill the symmetric off diagonal element
            ix += 1
            j2 = sub2ind((nvar, nvar), i_v2, i_v1)
            pop_exprs[ix] = Expr(:block,
                :(setindex!(i, $(i_eq), $(ix))),
                :(setindex!(j, $(j2), $(ix))),
                :(setindex!(v, $(Symbol("_val_$(i_expr)")), $(ix)))
            )
        end
    end

    # gather these assignments into a block
    populate = Expr(:block)
    populate.args = pop_exprs

    # finally construct the whole blocks
    out = quote
        i = Array(Int, $(n_terms))
        j = Array(Int, $(n_terms))
        v = Array(Float64, $(n_terms))

        # include vals
        $vals

        # populate i, j, v with vals
        $populate

        # construct sparse matrix from i, j, v and return it
        return sparse(i, j, v, $(length(ff.eqs)), $(nvar*nvar))
    end

    _filter_lines!(out)

end

# we don't support non-allocating method for Hessians
build_function!(ff::FunctionFactory, ::TDer{2}) =
    error("Non-allocating Hessians not supported")

# -------------------------- #
# Putting functions together #
# -------------------------- #

func_body{n}(ff::FunctionFactory, d::TDer{n}) =
    Expr(:block, allocate_block(ff, d), body_block(ff, d))

func_body!{n}(ff::FunctionFactory, d::TDer{n}) =
    Expr(:block, sizecheck_block(ff, d), body_block(ff, d))

function _build_function{n}(ff::FunctionFactory, d::TDer{n},
                            sig_func::Function, body_func::Function)
    Expr(:function, sig_func(ff, d), body_func(ff, d))
end

function _build_function(ff::FunctionFactory, d::TDer{0}, sig_func::Function,
                         body_func::Function)
    body = body_func(ff, d)
    sig = sig_func(ff, d)
    no_der_sig = deepcopy(sig)
    splice!(no_der_sig.args, 2)
    Expr(:block,
         Expr(:function, sig, body),
         Expr(:function, no_der_sig, body)
         )
end

# NOTE: we could easily allocate and then call the mutating version, but we
#       don't do that because then we get overhead for allocating _and_
#       for checking the size of out
"Build allocating version of the method"
build_function{n}(ff::FunctionFactory, d::TDer{n}) =
    _build_function(ff, d, signature, func_body)

"Build non-allocating version of the method"
build_function!{n}(ff::FunctionFactory, d::TDer{n}) =
    _build_function(ff, d, signature!, func_body!)

function build_vectorized_function(ff::FunctionFactory{FlatArgs}, d::TDer{0})
    sig = signature(ff, d)
    # need to adjust second to last arg to be type AbstractMatrix
    sig.args[length(sig.args)-1] = :(V::AbstractMatrix)
    sig

    # use signature to figure out how to call non-vectorized version within the
    # loop
    row_i_sig = signature(ff, d)
    row_i_sig.args[2] = :($(d))
    row_i_sig.args[length(sig.args)-1] = :(V[_row, :])

    body = Expr(:block,
        allocate_block(ff, d),
        :(nrow = size(V, 1)),
        Expr(:for, :(_row = 1:nrow),
            Expr(:block,
            :(out[_row, :] = $(row_i_sig))
            )
        ),
        :(return out)
    )

    no_der_sig = deepcopy(sig)

    splice!(no_der_sig.args, 2)

    Expr(:block,
         Expr(:function, sig, body),
         Expr(:function, no_der_sig, body)
         )
end

# -------- #
# User API #
# -------- #
# This is the main method that does the work.
function make_method{n}(d::TDer{n}, ff::FunctionFactory; mutating::Bool=true,
                        allocating::Bool=true)

    out = Expr(:block)
    mutating && push!(out.args, build_function!(ff, d))
    allocating && push!(out.args, build_function(ff, d))
    out
end

# accept derivative order(s) as keyword argument
function make_method(ff::FunctionFactory;
                     mutating::Bool=true,
                     allocating::Bool=true,
                     orders=0)
    out = Expr(:block)
    for i in orders
        out_i = make_method(Der{i}, ff; mutating=mutating, allocating=allocating)
        append!(out.args, out_i.args)
    end
    out
end

# Method without `dispatch` argument and with orders as kwarg
function make_method(eqs::Vector{Expr},
                     arguments::ArgType,
                     params::ParamType;
                     targets::Vector{Symbol}=Symbol[],
                     defs::Associative=Dict(),
                     funname::Symbol=gensym(:anonymous),
                     mutating::Bool=true,
                     allocating::Bool=true,
                     orders=0)
    ff = FunctionFactory(eqs, arguments, params,
                         targets=targets, defs=defs, funname=funname)

    make_method(ff; mutating=mutating, allocating=allocating, orders=orders)
end

# Method with `dispatch` argument and with orders as kwarg
function make_method{T}(::Type{T}, eqs::Vector{Expr},
                        arguments::ArgType,
                        params::ParamType;
                        targets::Vector{Symbol}=Symbol[],
                        defs::Associative=Dict(),
                        funname::Symbol=gensym(:anonymous),
                        mutating::Bool=true,
                        allocating::Bool=true, orders=0)
    ff = FunctionFactory(T, eqs, arguments, params,
                         targets=targets, defs=defs, funname=funname)

    make_method(ff; mutating=mutating, allocating=allocating, orders=orders)
end
