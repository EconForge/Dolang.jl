# ------------------------------------- #
# Helper methods used in generated code #
# ------------------------------------- #

# inlined function to extract a single variable. If `x` is a vector then
# extract a single element. If `x` is a Matrix then extract one column of the
# matrix
@inline _unpack_var(x::AbstractVector, i::Integer) = x[i]
@inline _unpack_var(x::AbstractMatrix, i::Integer, ::Type(COrder)) = view(x, :, i)
@inline _unpack_var(x::AbstractMatrix, i::Integer, ::Type(FOrder)) = view(x, i, :)
@inline _unpack_var(x::AbstractMatrix, i::Integer) = view(x, :, i)

# inlined function to extract a single observations of a vector fo variables.
@inline _unpack_obs(x::AbstractMatrix, i::Integer, ::Type(COrder)) = view(x, i, :)
@inline _unpack_obs(x::AbstractMatrix, i::Integer, ::Type(FOrder)) = view(x, :, i)
@inline _unpack_obs(x::AbstractMatrix, i::Integer) = view(x, i, :)
@inline _unpack_obs(x::AbstractVector, i::Integer) = x

# similar to _unpack_var, but instead assigns one element of a vector or one
# column of a matrix
@inline _assign_var(lhs::AbstractVector, rhs::Number, i) = setindex!(lhs, rhs, i)
@inline _assign_var(lhs::AbstractMatrix, rhs::AbstractVector, i) = setindex!(lhs, rhs, :, i)
@inline _assign_var(lhs::AbstractMatrix, rhs::AbstractVector, i, ::Type(COrder)) = setindex!(lhs, rhs, :, i)
@inline _assign_var(lhs::AbstractMatrix, rhs::AbstractVector, i, ::Type(FOrder)) = setindex!(lhs, rhs, i, :)

# determine the size of the output variable, given the number of expressions
# in the equation and all the input arguments
_output_size(n_expr::Int, args::AbstractVector...) = (n_expr,)

_output_size(n_expr::Int, arg::AbstractMatrix) = (size(arg, 1), n_expr)

function _output_size(n_expr::Int, args...)
    n_row = 0
    # get maximum number of rows among matrix arguments
    for a in args
        if isa(a, AbstractMatrix)
            nr = size(a, 1)
            if n_row > 0
                # we already found another matrix argument, now we can enforce
                # that all matrix arguments have conformable shapes
                if nr != n_row
                    msg = string("Unconformable argument sizes. For vectorized",
                                 " Core.evaluation all matrix arguments must have ",
                                 "the same number of rows.")
                    throw(DimensionMismatch(msg))
                end
            else
                # we need to update n_row
                n_row = nr
            end
        end
    end
    (n_row, n_expr)
end

_output_size(n_expr::Int, ::Type(COrder), args...)  = _output_size(n_expr::Int, args...)

function _output_size(n_expr::Int, order::Type(FOrder), args...)
    s = _output_size(n_expr::Int, order::Type(FOrder), args...)
    (s[2],s[1])
end



# Allocate an array of eltype `T` for `n_expr` variables that corresponds
# to input arguments `args...`
_allocate_out(T::Type, n_expr::Int, args::AbstractVector...) = Array{T}(n_expr)

_allocate_out(T::Type, order::Union{Type(COrder),Type(FOrder)}, n_expr::Int, arg::AbstractMatrix) =
    Array{T}(undef, _output_size(n_expr, order, arg))

_allocate_out(T::Type, n_expr::Int, arg::AbstractMatrix) =
        Array{T}(undef, _output_size(n_expr, arg))

function _allocate_out(T::Type, n_expr::Int, args...)
    Array{T}(undef, _output_size(n_expr, args...))
end

function _allocate_out(T::Type, order::Union{Type(COrder),Type(FOrder)}, n_expr::Int, args...)
    Array{T}(undef, _output_size(n_expr, order, args...))
end

## Triangular solver

_expr_or_number(x::Union{AbstractString,Symbol,Expr}) = _to_expr(x)
_expr_or_number(x::Number) = x

inf_to_Inf(x::Number) = x
inf_to_Inf(x::Symbol) = x in (:inf, :Inf) ? Inf : x
inf_to_Inf(ex::Expr) = Expr(ex.head, map(inf_to_Inf, ex.args)...)

_to_Float64(x::Real) = convert(Float64, x)
_to_Float64(x::AbstractArray) = map(Float64, x)

function solution_order(d::OrderedDict, it::IncidenceTable, pre_solved::Vector{Symbol}=Symbol[])
    # unpack some data
    vars = collect(keys(d))
    n = length(d)

    # allocate
    out = zeros(Int, length(d))

    # Start with indices for equations that are purely numerical
    front = setdiff(1:n, keys(it.by_eq))
    out[front] = 1:length(front)
    solved = vcat(pre_solved, vars[front])
    to_solve = deepcopy(it.by_eq)

    # now start stepping through equations
    ix = length(front)
    for _junk in 2:n+1
        for (eq, eq_vars) in to_solve
            can_solve = true
            for (var, dates) in eq_vars
                if !in(var, solved)
                    can_solve = false
                    break
                end
            end
            if can_solve
                out[eq] = ix+=1
                push!(solved, vars[eq])
                pop!(to_solve, eq)
            end
        end
    end

    !isempty(to_solve) && error("Not triangular system")

    return sortperm(out)
end


function solution_order(_d::AbstractDict, pre_solved::Vector{Symbol}=Symbol[])
    d = OrderedDict(_d)
    it = Dolang.IncidenceTable(collect(values(d)))
    solution_order(d, it, pre_solved)
end

solve_triangular_system(d::AbstractDict) = solve_triangular_system(OrderedDict(d))

function solve_triangular_system(d::OrderedDict)
    sol_order = solution_order(d)

    # extract expressions and variable names in proper order
    nms = collect(keys(d))[sol_order]
    exprs = collect(values(d))[sol_order]

    # build expression to Core.evaluate system in correct order
    to_Core.eval = Expr(:block)
    to_Core.eval.args = [:($(i[1])=$(i[2])) for i in zip(nms, exprs)]

    # add one line to return a tuple of all data
    ret = Expr(:tuple); ret.args = nms

    # now Core.evaluate and get data
    data = Core.eval(Dolang, :(let
                        $to_Core.eval;
                        $ret
                        end))

    OrderedDict{Symbol,Real}(zip(nms, data))
end

mutable struct TriangularSystemException <: Exception
    missing
end

"""
Solves triangular system specified by incidence dictionary.

```
system = Dict(0=>[1,2], 1=>[], 2=>[1] )
solve_dependency(system)
```

or

```
system = Dict(:x=>[:y,:z], :y=>[], :z=>[:y] )
solve_dependency(system)
```

Optionally, one can add specify which subset of variables to solve  so that unrequired variables will be ommited in the solution. In

```
system = Dict(:x=>[:y,:z], :y=>[], :z=>[:y], :p=>[:x,y,z] )
solve_dependency(system, [:x,:y,:z])

```

the answer is the same as before since `:p` is not needed to
define the values of `[:x,:y,:z]`.
"""
function solve_dependencies(deps::AbstractDict{T,Set{T}}, unknowns=nothing) where T
    solution = []
    if unknowns == nothing
        needed = Set(keys(deps))
    else
        needed = Set(unknowns)
    end
    while length(needed)>0
        tt0 = (length(needed), length(solution))
        if length(needed)==0
            return solution
        else
            for k in needed
                # check whether k is solved
                if k in solution
                    pop!(needed, k)
                elseif issubset(deps[k], solution)
                    push!(solution, k)
                else
                    needed = union(deps[k], needed)
                end
            end
            tt = (length(needed), length(solution))
            if tt == tt0
                mis = join([string(e) for e in needed], ", ")
                exc = TriangularSystemException(mis)
                throw(exc)
            end
        end
    end
    return solution
end


function get_dependencies(defs::AbstractDict{T,U}) where T where U
    deps = OrderedDict{Any,Set{Any}}()
    for (k,v) in (defs)
        ii = intersect( Set(( collect( values( Dolang.list_symbols(v) ))... ,)), Set(keys(defs)))
        # ii = intersect( Set(union( collect( values( Dolang.list_symbols(v) ))... )), Set(keys(defs)))
        ij = Set(ii)
        deps[k] = ij
    end
    deps
end

function reorder_triangular_block(defs::AbstractDict{T,U}) where T where U
    deps = get_dependencies(defs)
    sol = Dolang.solve_dependencies(deps)
    return OrderedDict((k,defs[k]) for k in sol)
end

"""Solves definitions blocks

Keys are timed variables in canonical form (e.g. `(:v,0)`) at date t=0.
Values are expressions, possibly referencing key variables at different dates.
The system is recursively solved for the unknowns, by default the keys.
"""
function solve_definitions(defs::AbstractDict{Tuple{Symbol, Int}, <:SymExpr}, unknowns=keys(defs))
    # defs should map timed-vars to expressions.
    defs = deepcopy(defs)
    for (v,t) in collect(keys(defs))
        # t should always be 0
        @assert t==0
        for shift in (-1,1)
            defs[(v,shift)] = Dolang.time_shift(defs[(v,t)], shift)
        end
    end
    deps = Dolang.get_dependencies(defs)
    solution = Dolang.solve_dependencies(deps, unknowns)
    reordered = OrderedDict()
    for k in solution
        reordered[k] = defs[k]
    end
    return reordered
end
