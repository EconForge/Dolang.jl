# finds the longest number of observations in arguments like
# x::Svector{2}, y::Vector{SVector{2}}, z::SVector{3}
# (in this case, length of y)
# slow and fails shamelessly if only SVectors are supplied
function _getsize(arrays::Union{Vector,<:SVector}...)
    vectors_length = Int[length(a) for a in arrays if isa(a,Vector)]
    @assert length(vectors_length)>0
    maximum(vectors_length)
end
@inline _getobs(x::Vector{<:SVector},i::Int) = x[i]
@inline _getobs(x::SVector,i::Int) = x

# constructs expression SVector(a,b,c) from [:a,:b,:c]
function _sym_sarray(v::Vector{Symbol})
    Expr(:call,:SVector, v...)
end
function _sym_sarray(v::Matrix{Symbol})
    p,q = size(v)
    Expr(:call,:(SMatrix{$p,$q}), v[:]...)
end

_get_first(x) = x[1]

list_syms(eq) = Dolang.list_symbols(eq)[:parameters]

diff_symbol(k::Symbol,j::Symbol) = Symbol(string("∂",k,"_∂",j))

function add_derivatives(dd::OrderedDict, jac_args::Vector{Symbol})
    # given a list of equations:
    # x=p+a
    # y=p+x
    # and a list of symbols to differetiate with
    # [:a, :p]
    # produces a new list of equations with derivatives:
    # x=p+a
    # ∂x_∂a = 1
    # ∂x_∂p = 1
    # y=p+x
    # ∂y_∂a = ..
    # ∂y_∂p = ...

    diff_eqs = OrderedDict{Symbol, Union{Expr, <:Number, Symbol}}()
    for (var,eq) in dd
        diff_eqs[var] = eq
        deps = list_syms(eq) # list of variables eq depends on
        for k in deps
            if k in jac_args
                dv = diff_symbol(var, k)
                deq = Dolang.deriv(eq, k)
                diff_eqs[dv] = deq
            else
                for l in jac_args
                    dv = diff_symbol(var, k)
                    ddv = diff_symbol(k, l)
                    cdv = diff_symbol(var, l)
                    if ddv in keys(diff_eqs)
                        if !(dv in keys(diff_eqs))
                            deq = Dolang.deriv(eq, k)
                            diff_eqs[dv] = deq
                        end
                        diff_eqs[cdv] = :($dv*$ddv)
                    end
                end
            end
        end
    end

    return diff_eqs

end

"""
Create a non allocating kernel from the function factory.

`fff`: assumed to be a `FlatFunctionFactory` object with empty preamble.
`diff`: index of variables to differentiate with or list of indices of variables positions.

The generated kernel looks like (diff=[0,1])
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
    oo_0 = SVector(_foo__0_, _bar__0_)
    oo_1 = SMatrix{2, 1}(d__foo__0__d__a_m1_, d__bar__0__d__a_m1_)
    res_ = (oo_0, oo_1)
    return res_
end
```

If diff is a scalar, the result of the kernel is a static vector (or a static matrix).
If diff is a list, the result is a tuple.
"""
function gen_kernel(fff::FlatFunctionFactory, diff::Vector{Int}; funname=fff.funname, arguments=fff.arguments)

    targets = fff.targets

    # names of symbols to output
    output_names = []
    for d in diff
        if d == 0
            push!(output_names, targets)
        else
            diff_args = collect(values(fff.arguments))[d]
            p = length(fff.targets)
            q = length(diff_args)
            mat = Matrix{Symbol}(p,q)
            for i=1:p
                for j=1:q
                    mat[i,j] = diff_symbol(targets[i], diff_args[j])
                end
            end
            push!(output_names, mat)
        end
    end


    argnames = collect(keys(arguments))

    all_eqs = cat(1,values(fff.preamble)...,fff.equations)
    all_args = cat(1,values(fff.arguments)...)
    jac_args = cat(1,[collect(values(fff.arguments))[i] for i in diff if i!=0]...)


    # concatenate preamble and equations (doesn't make much sense...)
    dd = deepcopy(fff.preamble)
    for (i,eq) in enumerate(fff.equations)
        dd[fff.targets[i]] = eq
    end

    # compute all equations to write
    diff_eqs = add_derivatives(dd, jac_args)
    for out in output_names
        for k in out[:]
            if !(k in keys(diff_eqs))
                diff_eqs[k] = 0.0
            end
        end
    end

    # create function block
    code = []
    for (k,args) in zip(argnames, values(arguments))
        for (i,a) in enumerate(args)
            push!(code, :($a = ($k)[$i]))
        end
    end
    for (k,v) in diff_eqs
            push!(code, :($k=$v))
    end

    return_args = []
    for (d,names) in enumerate(output_names)
        outname = Symbol(string("oo_",d))
        push!(code, :($outname = $(_sym_sarray(names))))
        push!(return_args, outname)
    end

    # this is to make inserting the resulting code in another function easier
    push!(code, :(res_=$(Expr(:tuple, return_args...))))
    push!(code, :(return res_))

    # now we construct the function
    typed_args = [:($k::SVector{$(length(v)), Float64}) for (k,v) in arguments]
    fun_args = Expr(:call, funname, typed_args...)

    Expr(:function, fun_args, Expr(:block, code...))

end


"""
Creates a vectorized function, which can accept points or list-of-points as arguments.

`fff``: assumed to be a `FlatFunctionFactory` object with empty preamble.
`diff`: index of variables to differentiate with or list of indices of variables positions.
`out`: optional preallocated output

If at least one of the arguments is a list of points, the result is a list of points (or a tuple mad of lists of points).
In this case preallocated structures can be passed as `out`.
"""
function gen_gufun(fff::FlatFunctionFactory, to_diff::Union{Vector{Int}, Int};
    funname=fff.funname)

    if to_diff isa Int
        diff = [to_diff]::Array{Int}
    else
        diff = to_diff::Array{Int}
    end

    # Generate the code for the kernel
    # change arguments in the kernel to avoid clashes
    arguments_ = OrderedDict{Symbol,Vector{Symbol}}(
                    (Symbol(string(k,"_")),v) for (k,v) in fff.arguments
                )
    kernel_code = gen_kernel(fff, diff; funname=:kernel, arguments=arguments_)
    # remove function def and return statement
    kernel_code_stripped = kernel_code.args[2].args[1:end-1]


    args = [keys(fff.arguments)...]
    args_scalar = [keys(arguments_)...]
    args_out = [Symbol(string("out_",i)) for i in 1:length(diff)]
    args_length = [length(v) for v in values(fff.arguments)]

    out_types = []
    p = length(fff.targets)
    for d in diff
        if d==0
            push!(out_types, :(SVector{$p,Float64}))
        else
            q = args_length[d]
            push!(out_types, :(SMatrix{$p,$q,Float64,$(p*q)}))
        end
    end

    code = quote
        function $funname($(args...),out=nothing)

            # @inline $kernel_code # unpure...
            # revisit in the future

            ### if all arguments are 1d.vectors we do vector things
            # if they are static
            if (($(args...)),) isa Tuple{$([:(SVector) for i=1:length(args)]...)}
                # ret = kernel($(args...))
                # return $(to_diff isa Int? :(ret[1]) :  :(ret) )
                $([:($_a=$a) for (_a,a) in zip(args_scalar,args)]...)
                $(kernel_code_stripped...)
                return $(to_diff isa Int? :(res_[1]) :  :(res_) )
            end
            # if they are arrays
            if (($(args...)),) isa Tuple{$([:(Vector{Float64}) for i=1:length(args)]...)}
                # oo = kernel( $( [ :(SVector( $(e)...)) for e in args]...) )
                $([:($_a=$a) for (_a,a) in zip(args_scalar,args)]...)
                $(kernel_code_stripped...)
                ret = ( $( [:(Array(res_[$i])) for i=1:length(args_out)]...),)
                return $(to_diff isa Int? :(ret[1]) :  :(ret) )

            end

            N = _getsize($(args...))::Int

            if isa(out,Void)
                $([:($a=zeros($t,N)) for (a,t) in zip(args_out,out_types)]...)
            else
                $([:($a=out[$i]::Vector{$t}) for (i,(a,t)) in enumerate(zip(args_out,out_types))]...)
            end

            @inbounds @simd for n=1:N
                $([:($a_=_getobs($a,n)) for (a_,a) in zip(args_scalar,args)]...)
                # res_ = kernel($(args_scalar...))
                $(kernel_code_stripped...)
                $([:($a[n] = res_[$i]) for (i,a) in enumerate(args_out)]...)
            end

            ret = ($(args_out...),)
            return $(to_diff isa Int? :(ret[1]) :  :(ret) )
        end
    end

    return code

end

# We now produce the code for generated functions.
# The generated functions have a signature like:
# genfun((Val(1),Val(2)), x,y,z,p, out=)
# where the first argument denotes the arguments to differentiate with (0, is zero-order diff)

function _get_nums(t)
    # reads deritative types
    # return (2,3,4) for Tuple{Val{2},Val{3},Val{4}}
    n = length(fieldnames(t)) # inconsistent with what follows!
    svec = getfield(t, 3)
    res = zeros(Int,n)
    for i=1:n
        res[i] = svec[i].parameters[1]
    end
    return res
end



function gen_generated_kernel(fff::FlatFunctionFactory)

    funname = fff.funname
    meta_code = quote
        @generated function $funname(orders, x, y, z, p, out=nothing)
            fff = $(fff) # this is amazing !
            oorders = _get_nums(orders) # convert into tuples
            code = gen_kernel(fff, oorders)
            code.args[2]
        end
    end
end


function gen_generated_gufun(fff::FlatFunctionFactory; funname=fff.funname, dispatch=nothing)

    args = collect(keys(fff.arguments))
    dispatch_argument = parse(string(dispatch))
    meta_code = quote
        @generated function $funname($((dispatch==nothing?[]:[:(::$dispatch_argument)])...), orders,  $(args...), out=nothing)
            fff = $(fff) # this is amazing !
            oorders = _get_nums(orders) # convert into tuples
            code = gen_gufun(fff, oorders)
            code.args[2].args[2]
        end
    end
end
