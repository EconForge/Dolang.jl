# finds the longest number of observations in arguments like
# x::Svector{2}, y::Vector{SVector{2}}, z::SVector{3}
# (in this case, length of y)
# slow and fails shamelessly if only SVectors are supplied
function _getsize(arrays::Union{AbstractArray,<:SVector}...)
# function _getsize(arrays::Union{AbstractVector,Adjoint,<:SVector}...)
    vectors_length = Int[length(a) for a in arrays if ~isa(a, SVector)]
    @assert length(vectors_length)>0
    maximum(vectors_length)
end

@inline _getobs(x::AbstractArray, i::Int) = x[i]
# @inline _getobs(x::Union{AbstractVector,Adjoint}, i::Int) = x[i]
@inline _getobs(x::SVector, i::Int) = x

# constructs expression SVector(a,b,c) from [:a,:b,:c]
_sym_sarray(v::AbstractVector{Symbol}) = Expr(:call,:SVector, v...)
_sym_sarray(v::AbstractMatrix{Symbol}) = Expr(:call, :(SMatrix{$(size(v)...)}), v...)

list_syms(eq::Expr) = list_symbols(eq).parameters
list_syms(eq::Symbol) = [eq]
list_syms(eq::Number) = Symbol[]

diff_symbol(k::Symbol, j::Symbol) = Symbol("∂", k, "_∂", j)


function add_derivatives(all_eqs, gradient_components)

    new_eqs = OrderedDict{Symbol, Union{Expr, Symbol, Int64, Float64}}()
    for g in gradient_components
        k = diff_symbol(g,g)
        new_eqs[k] = 1
    end

    for (sym, eq) in all_eqs

        new_eqs[sym] = eq
        # symbols the equation depends on
        deps = Dolang.list_parameters(eq)

        for θ in gradient_components

            deq_terms = []

            for a in deps

                # compute dy/da * da/dθ

                if a == θ
                    push!(deq_terms, ( deriv(eq, a), 1) )
                elseif !(a in keys(new_eqs))  # I should keep a list of defined variables
                    # nothing to do
                else
                    da_dθ = diff_symbol(a, θ)
                    if da_dθ in keys(new_eqs) # otherwise it is 0
                        dy_da = diff_symbol(sym, a)
                        if !(dy_da in keys(new_eqs))
                            new_eqs[dy_da] = deriv(eq, a)
                        end
                        push!(deq_terms, (dy_da, da_dθ) )
                    end
                end
            end
            if length(deq_terms)>0
                if length(deq_terms)==1
                    rhs = Expr(:call, :*, deq_terms[1]...)
                else
                    rhs = Expr(:call, :+, [Expr(:call, :*, a, b) for (a,b) in deq_terms]...)
                end
                new_eqs[diff_symbol(sym, θ)] = rhs
            end
        end
    end

    return new_eqs

end

# function add_derivatives(dd::OrderedDict, jac_args::Vector{Symbol})
#     # given a list of equations:
#     # x=p+a
#     # y=p+x
#     # and a list of symbols to differetiate with
#     # [:a, :p]
#     # produces a new list of equations with derivatives:
#     # x=p+a
#     # ∂x_∂a = 1
#     # ∂x_∂p = 1
#     # y=p+x
#     # ∂y_∂a = ..
#     # ∂y_∂p = ...

#     diff_eqs = OrderedDict{Symbol, SymExpr}()
#     for (var, eq) in dd
#         diff_eqs[var] = eq
#         deps = list_syms(eq) # list of variables eq depends on

#         for k in [kk for kk in deps if !(kk in jac_args)]
#             for l in jac_args
#                 dv = diff_symbol(var, k)
#                 ddv = diff_symbol(k, l)
#                 cdv = diff_symbol(var, l)
#                 if haskey(diff_eqs, ddv)
#                     if !haskey(diff_eqs, dv)
#                         deq = Dolang.deriv(eq, k)
#                         diff_eqs[dv] = deq
#                     end
#                     if cdv in keys(diff_eqs)
#                         diff_eqs[cdv] = :($(diff_eqs[cdv])+$dv*$ddv)
#                     else
#                         diff_eqs[cdv] = :($dv*$ddv)
#                     end
#                 end
#             end
#         end
#         for k in [kk for kk in deps if kk in jac_args]
#             dv = diff_symbol(var, k)
#             deq = Dolang.deriv(eq, k)
#             diff_eqs[dv] = deq
#         end

#     end

#     return diff_eqs

# end


function clean_unused(code)

    lines = code.args[2].args

    uses = Vector{Symbol}[]
    defs = Symbol[]

    for l in lines
        if l.head==:(=)
        rhs = l.args[2] 
        tt = []
        MacroTools.postwalk(x-> ( ((x isa(Symbol))  ) ? push!(tt, x) : nothing ) , rhs)
        push!(uses, tt)
        push!(defs, l.args[1])
        end

    end


    newlines = [:(return res_)]
    useful = [ :(res_) ]
    for j=1:length(lines)-1
        i = length(lines) - j 
        if occursin("oo", string(defs[i])) || (defs[i] ∈ useful)
            push!(newlines, lines[i])
            append!(useful, uses[i])
        end
    end

    newcode = deepcopy(code)
    reverse!(newlines)
    newcode.args[2].args = newlines
    return newcode

end


"""
Create a non allocating kernel from the function factory.

`fff`: assumed to be a `FlatFunctionFactory` object with empty preamble.
`diff`: index of variables to differentiate with or list of indices of variables positions.

The generated kernel looks like (diff=[0, 1])
```
function myfun(x::SVector{1, Float64}, y::SVector{3, Float64}, z::SVector{2, Float64}, p::SVector{1, Float64})
    _a_m1_ = x[1]
    _a__0_ = y[1]
    _b__0_ = y[2]
    _c__0_ = y[3]
    _c__1_ = z[1]
    _d__1_ = z[2]
    _u_ = p[1]
    _foo__0_ = log(_a__0_) + _b__0_ / (_a_m1_ / (1 - _c__0_))
    _bar__0_ = _c__1_ + _u_ * _d__1_
    d__foo__0__d__a_m1_ = (-(1 / (1 - _c__0_)) * _b__0_) / (_a_m1_ / (1 - _c__0_)) ^ 2
    d__bar__0__d__a_m1_ = 0
    oo_0_ = SVector(_foo__0_, _bar__0_)
    oo_1_ = SMatrix{2, 1}(d__foo__0__d__a_m1_, d__bar__0__d__a_m1_)
    res_ = (oo_0_, oo_1_)
    return res_
end
```

If diff is a scalar, the result of the kernel is a static vector (or a static matrix).
If diff is a list, the result is a tuple.
"""
function gen_kernel2(fff::FlatFunctionFactory, diff::Union{Int, Vector{Int}}; funname=fff.funname, arguments=fff.arguments, dispatch=nothing)

    targets = [keys(fff.equations)...]
    equations = [values(fff.equations)...]
    # names of symbols to output
    output_names = []
    for d in diff
        if d == 0
            push!(output_names, targets)
        else
            diff_args = collect(values(fff.arguments))[d]
            p = length(targets)
            q = length(diff_args)
            mat = Matrix{Symbol}(undef, p, q)
            for i in 1:p
                for j in 1:q
                    mat[i, j] = diff_symbol(targets[i], diff_args[j])
                end
            end
            push!(output_names, mat)
        end
    end


    argnames = collect(keys(arguments))

    all_eqs = cat(values(fff.preamble)..., equations, dims=1)
    all_args = cat(values(fff.arguments)..., dims=1)
    if maximum(diff)>0
        jac_args = cat([collect(values(fff.arguments))[i] for i in diff if i!=0]..., dims=1)
    else
        jac_args = []
    end
    # jac_args = Symbol.(jac_args) # strange type of output can by Any[]
    jac_args = Symbol[Symbol(e) for e in jac_args]

    # concatenate preamble and equations (doesn't make much sense...)
    dd = OrderedDict()
    for (k, v) in (fff.preamble)
        dd[k] = v
    end
    for (target, eq) in zip(targets, equations)
        dd[target] = eq
    end
    # compute all equations to write
    diff_eqs = add_derivatives(dd, jac_args)
    for out in output_names
        for k in out
            if !(haskey(diff_eqs, k))
                diff_eqs[k] = 0.0
            end
        end
    end

    diff_eqs = reorder_triangular_block(diff_eqs)

    # create function block
    code = []

    push!(code, :(T=getprecision(model)))

    for (k, args) in zip(argnames, values(arguments))
        for (i, a) in enumerate(args)
            push!(code, :($a = ($k)[$i]))
        end
    end
    for (k, v) in diff_eqs
        push!(code, :($k=$v))
    end

    return_args = []
    for (d, names) in enumerate(output_names)
        outname = Symbol("oo_", d, "_")
        @show names
        push!(code, :($outname = $(_sym_sarray(names))))
        push!(return_args, outname)
    end

    # this is to make inserting the resulting code in another function easier
    if typeof(diff) <: Int
        push!(code, :(res_ = $(return_args[1])))
        push!(code, :(return res_))
    else
        push!(code, :(res_ = $(Expr(:tuple, return_args...))))
        push!(code, :(return res_))
    end

    cast_scalars = u->MacroTools.postwalk(x -> ( (x==:Inf) | ((x isa Real)&(!(x isa Integer))) ) ? :(convert(T,$x)) : x, u)

    code = cast_scalars.(code)
    # now we construct the function
    # typed_args = [:($k::SVector{$(length(v)), Float64}) for (k, v) in arguments]

    typed_args = [:($k::SVector{$(length(v))}) for (k, v) in arguments]
    # typed_args[end]= :(p::P)

    # if !(dispatch isa Nothing)
    #     prepend!(typed_args, :(::$dispatch))
    # end

    # n_p = length(arguments[[keys(arguments)...][end]][2])

    typed_args = [:(model::$dispatch), typed_args...]
    
    # fun_args = Expr(:call, funname, typed_args...)
    fun_args = :(($funname)($(typed_args...)))
    ncode = Expr(:function, fun_args, Expr(:block, code...))

    return clean_unused(ncode)

end


"""
Create a non allocating kernel from the function factory.

`fff`: assumed to be a `FlatFunctionFactory` object with empty preamble.
`diff`: index of variables to differentiate with or list of indices of variables positions.

The generated kernel looks like (diff=[0, 1])
```
function myfun(x::SVector{1, Float64}, y::SVector{3, Float64}, z::SVector{2, Float64}, p::SVector{1, Float64})
    _a_m1_ = x[1]
    _a__0_ = y[1]
    _b__0_ = y[2]
    _c__0_ = y[3]
    _c__1_ = z[1]
    _d__1_ = z[2]
    _u_ = p[1]
    _foo__0_ = log(_a__0_) + _b__0_ / (_a_m1_ / (1 - _c__0_))
    _bar__0_ = _c__1_ + _u_ * _d__1_
    d__foo__0__d__a_m1_ = (-(1 / (1 - _c__0_)) * _b__0_) / (_a_m1_ / (1 - _c__0_)) ^ 2
    d__bar__0__d__a_m1_ = 0
    oo_0_ = SVector(_foo__0_, _bar__0_)
    oo_1_ = SMatrix{2, 1}(d__foo__0__d__a_m1_, d__bar__0__d__a_m1_)
    res_ = (oo_0_, oo_1_)
    return res_
end
```

If diff is a scalar, the result of the kernel is a static vector (or a static matrix).
If diff is a list, the result is a tuple.
"""
function gen_kernel(fff::FlatFunctionFactory, diff::Vector{Int}; funname=fff.funname, arguments=fff.arguments)

    targets = [keys(fff.equations)...]
    equations = [values(fff.equations)...]
    # names of symbols to output
    output_names = []
    for d in diff
        if d == 0
            push!(output_names, targets)
        else
            diff_args = collect(values(fff.arguments))[d]
            p = length(targets)
            q = length(diff_args)
            mat = Matrix{Symbol}(undef, p, q)
            for i in 1:p
                for j in 1:q
                    mat[i, j] = diff_symbol(targets[i], diff_args[j])
                end
            end
            push!(output_names, mat)
        end
    end


    argnames = collect(keys(arguments))

    all_eqs = cat(values(fff.preamble)..., equations, dims=1)
    all_args = cat(values(fff.arguments)..., dims=1)
    if maximum(diff)>0
        jac_args = cat([collect(values(fff.arguments))[i] for i in diff if i!=0]..., dims=1)
    else
        jac_args = []
    end
    # jac_args = Symbol.(jac_args) # strange type of output can by Any[]
    jac_args = Symbol[Symbol(e) for e in jac_args]

    # concatenate preamble and equations (doesn't make much sense...)
    dd = OrderedDict()
    for (k, v) in (fff.preamble)
        dd[k] = v
    end
    for (target, eq) in zip(targets, equations)
        dd[target] = eq
    end
    # compute all equations to write
    diff_eqs = add_derivatives(dd, jac_args)
    for out in output_names
        for k in out
            if !(haskey(diff_eqs, k))
                diff_eqs[k] = 0.0
            end
        end
    end

    diff_eqs = reorder_triangular_block(diff_eqs)

    # create function block
    code = []
    for (k, args) in zip(argnames, values(arguments))
        for (i, a) in enumerate(args)
            push!(code, :($a = ($k)[$i]))
        end
    end
    for (k, v) in diff_eqs
        push!(code, :($k=$v))
    end

    return_args = []
    for (d, names) in enumerate(output_names)
        outname = Symbol("oo_", d, "_")
        push!(code, :($outname = $(_sym_sarray(names))))
        push!(return_args, outname)
    end

    # this is to make inserting the resulting code in another function easier
    push!(code, :(res_ = $(Expr(:tuple, return_args...))))
    push!(code, :(return res_))

    # now we construct the function
    typed_args = [:($k::SVector{$(length(v)), Float64}) for (k, v) in arguments]
    fun_args = Expr(:call, funname, typed_args...)

    ncode = Expr(:function, fun_args, Expr(:block, code...))

    return clean_unused(ncode)

end


# NOTE: for small arrays (probably <= 10 elements) the splatting below is
#       faster than `reinterpret(SVector{length(v), Float64}, v)`
to_SA(v::AbstractVector{Float64}) = SVector(v...)
to_SA(v::AbstractMatrix{Float64}) = copy(reshape(reinterpret(SVector{size(v, 2),Float64}, vec(copy(v'))), (size(v, 1),)))

function from_SA(v::AbstractVector{SMatrix{p,q,Float64,k}}) where p where q where k
    copy(permutedims(reshape(reinterpret(Float64, vec(v)), (p, q, size(v, 1))), [3, 1, 2]))
end
from_SA(v::AbstractVector{SVector{d,Float64}}) where d  = copy(reshape(reinterpret(Float64, vec(v)), (d, size(v, 1)))')
from_SA(v::SVector{d,Float64}) where d = Vector(v)

# that one is a bit risky as from_SA(to_SA(mat)) != mat
from_SA(v::SMatrix{p,q,Float64,k}) where p where q where k = Matrix(v)
from_SA(v::Tuple) = tuple([from_SA(e) for e in v]...)


"""
Creates a vectorized function, which can accept points or list-of-points as
arguments.

`fff`: assumed to be a `FlatFunctionFactory` object with empty preamble.
`diff`: index of variables to differentiate with or list of indices of variables positions.
`out`: optional preallocated output

If at least one of the arguments is a list of points, the result is a list of
points (or a tuple mad of lists of points). In this case preallocated
structures can be passed as `out`.
"""
function gen_gufun(fff::FlatFunctionFactory, to_diff::Union{Array{Int}, Int};   
    funname=fff.funname)

    if to_diff isa Int
        diff = [to_diff]::Array{Int}
    else
        diff = to_diff::Array{Int}
    end

    # Generate the code for the kernel
    # change arguments in the kernel to avoid clashes
    arguments_ = OrderedDict{Symbol,Vector{Symbol}}(
        (Symbol(k, "_"), v) for (k, v) in fff.arguments
    )
    kernel_code = gen_kernel(fff, diff; funname=:kernel, arguments=arguments_)
    # remove function def and return statement
    kernel_code_stripped = kernel_code.args[2].args[1:end-1]

    args = [keys(fff.arguments)...]
    args_scalar = [keys(arguments_)...]
    args_out = [Symbol("out_", i) for i in 1:length(diff)]
    args_length = [length(v) for v in values(fff.arguments)]

    targets = [keys(fff.equations)...]

    out_types = []
    p = length(targets)
    for d in diff
        if d == 0
            push!(out_types, :(SVector{$p,Float64}))
        else
            q = args_length[d]
            push!(out_types, :(SMatrix{$p,$q,Float64,$(p*q)}))
        end
    end

    code = quote
        function $funname($(args...),out=nothing)

            @fastmath begin 
            # @inline $kernel_code # unpure...
            # revisit in the future

            ### if all arguments are 1d.vectors we do vector things
            # if they are static
            if (($(args...)),) isa Tuple{$([:(SVector) for i=1:length(args)]...)}
                # ret = kernel($(args...))
                # return $(to_diff isa Int? :(ret[1]) :  :(ret) )
                $([:($_a = $a) for (_a, a) in zip(args_scalar, args)]...)
                $(kernel_code_stripped...)
                return $(to_diff isa Int ? :(res_[1]) :  :(res_) )
            end

            # if all arguments are array vectors
            if (($(args...)),) isa Tuple{$([:(AbstractVector{Float64}) for i=1:length(args)]...)}
                 # oo = kernel( $( [ :(SVector( $(e)...)) for e in args]...) )
                 $([:($_a = $a) for (_a, a) in zip(args_scalar, args)]...)
                 $(kernel_code_stripped...)
                 ret = ( $( [:(Array(res_[$i])) for i=1:length(args_out)]...),)
                 return $(to_diff isa Int ? :(ret[1]) :  :(ret) )
             end
            # if arguments are array vectors and one of them is not a vector (plausibly a matrix then...)
            hackish = ( (($(args...)),) isa Tuple{$([:(AbstractArray{Float64}) for i=1:length(args)]...)} )
            if hackish
                $([:($a = Dolang.to_SA($a)) for a in args]...)
            end


            N = Dolang._getsize($(args...))::Int

            if isa(out, Nothing)
                $([:($a=zeros($t, N)) for (a, t) in zip(args_out, out_types)]...)
            else
                $([:($a=out[$i]::Vector{$t}) for (i,(a, t)) in enumerate(zip(args_out, out_types))]...)
            end

            @inbounds @simd for n=1:N
                $([:($a_ = Dolang._getobs($a, n)) for (a_, a) in zip(args_scalar, args)]...)
                # res_ = kernel($(args_scalar...))
                $(kernel_code_stripped...)
                $([:($a[n] = res_[$i]) for (i, a) in enumerate(args_out)]...)
            end

            ret = ($(args_out...),)

            if hackish
                ret = Dolang.from_SA(ret)
            end

            return $(to_diff isa Int ? :(ret[1]) :  :(ret) )
        end

        end
    end

    return code

end

# We now produce the code for generated functions.
# The generated functions have a signature like:
# genfun((Val(1),Val(2)), x, y, z, p, out=)
# where the first argument denotes the arguments to differentiate with (0, is zero-order diff)

_get_nums(::Union{Val{n}, Type{Val{n}}, Type{Type{Val{n}}}}) where n = n
_get_nums(t::Type{<:Tuple}) = [_get_nums(i) for i in getfield(t, 3)]
_get_oorders(x::Array{Int,0}) = x[1]
_get_oorders(x::Union{Tuple,<:Array{Int}}) = x


function gen_generated_kernel(fff::FlatFunctionFactory)

    funname = fff.funname
    meta_code = quote
        @generated function $funname(orders, x, y, z, p, out=nothing)
            fff = $(fff) # this is amazing !
            oorders = Dolang._get_nums(orders) # convert into tuples
            code = Dolang.gen_kernel(fff, oorders)
            code.args[2]
        end
    end
end


function gen_generated_gufun(fff::FlatFunctionFactory; funname=fff.funname, dispatch=Nothing)

    args = collect(keys(fff.arguments))
    dispatch_argtype = Meta.parse(string(dispatch))
    dispatch_arg = dispatch == Nothing ? [] : [:(::$(dispatch_argtype))]
    meta_code = quote
        # basic fun for compat
        @generated function $funname($(dispatch_arg...), $(args...), out=nothing)
            fff = $(fff) # this is amazing !
            code = Dolang.gen_gufun(fff, 0)
            code.args[2].args[2]
        end
        @generated function $funname($(dispatch_arg...), orders::Tuple, $(args...), out=nothing)
            fff = $(fff) # this is amazing !
            oorders = Dolang._get_nums(orders) # convert into tuples
            code = Dolang.gen_gufun(fff, oorders)
            code.args[2].args[2]
        end

        @generated function $funname($(dispatch_arg...), orders::Union{Val,Type{<:Val}}, $(args...), out=nothing)
            fff = $(fff) # this is amazing !
            oorders = _get_oorders(collect(Dolang._get_nums(orders)))
            code = Dolang.gen_gufun(fff, oorders)
            code.args[2].args[2]
        end
    end
end
