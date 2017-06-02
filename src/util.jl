# ------------------------------------- #
# Helper methods used in generated code #
# ------------------------------------- #

# inlined function to extract a single variable. If `x` is a vector then
# extract a single element. If `x` is a Matrix then extract one column of the
# matrix
@inline _unpack_var(x::AbstractVector, i::Integer) = x[i]

@inline _unpack_var(x::AbstractMatrix, i::Integer) = view(x, :, i)

# similar to _unpack_var, but instead assigns one element of a vector or one
# column of a matrix
@inline _assign_var(lhs::AbstractVector, rhs::Number, i) = setindex!(lhs, rhs, i)

@inline _assign_var(lhs::AbstractMatrix, rhs::AbstractVector, i) = setindex!(lhs, rhs, :, i)

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
                                 " evaluation all matrix arguments must have ",
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

# Allocate an array of eltype `T` for `n_expr` variables that corresponds
# to input arguments `args...`
_allocate_out(T::Type, n_expr::Int, args::AbstractVector...) = Array{T}(n_expr)

_allocate_out(T::Type, n_expr::Int, arg::AbstractMatrix) =
    Array{T}(_output_size(n_expr, arg))

function _allocate_out(T::Type, n_expr::Int, args...)
    sz = _output_size(n_expr, args...)
    Array{T}(sz[1], sz[2])
end

## Triangular solver

_expr_or_number(x::Union{AbstractString,Symbol,Expr}) = _to_expr(x)
_expr_or_number(x::Number) = x

inf_to_Inf(x::Number) = x
inf_to_Inf(x::Symbol) = x in (:inf, :Inf) ? Inf : x
inf_to_Inf(ex::Expr) = Expr(ex.head, map(inf_to_Inf, ex.args)...)

_to_Float64(x::Real) = convert(Float64, x)
_to_Float64(x::AbstractArray) = map(Float64, x)

function solution_order(d::OrderedDict, it::IncidenceTable)
    # unpack some data
    vars = collect(keys(d))
    n = length(d)

    # allocate
    out = zeros(Int, length(d))

    # Start with indices for equations that are purely numerical
    front = setdiff(1:n, keys(it.by_eq))
    out[front] = 1:length(front)
    solved = vars[front]
    to_solve = deepcopy(it.by_eq)

    # now start stepping through equations
    ix = length(front)
    for _junk in 2:n
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


function solution_order(_d::Associative)
    d = OrderedDict(_d)
    it = Dolang.IncidenceTable(collect(values(d)))
    solution_order(d, it)
end

solve_triangular_system(d::Associative) = solve_triangular_system(OrderedDict(d))

function solve_triangular_system(d::OrderedDict)
    sol_order = solution_order(d)

    # extract expressions and variable names in proper order
    nms = collect(keys(d))[sol_order]
    exprs = collect(values(d))[sol_order]

    # build expression to evaluate system in correct order
    to_eval = Expr(:block)
    to_eval.args = [:($(i[1])=$(i[2])) for i in zip(nms, exprs)]

    # add one line to return a tuple of all data
    ret = Expr(:tuple); ret.args = nms

    # now evaluate and get data
    data = eval(Dolang, :(let
                        $to_eval;
                        $ret
                        end))

    OrderedDict{Symbol,Real}(zip(nms, data))
end
