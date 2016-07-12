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
_allocate_out(T::Type, n_expr::Int, args::AbstractVector...) = Array(T, n_expr)

_allocate_out(T::Type, n_expr::Int, arg::AbstractMatrix) =
    Array(T, _output_size(n_expr, arg))

function _allocate_out(T::Type, n_expr::Int, args...)
    sz = _output_size(n_expr, args...)
    Array(T, sz[1], sz[2])
end
