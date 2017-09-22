function solve_dependency(dd::Dict{T,Set{T}}) where T # is hashable
    solved = T[]
    deps = deepcopy(dd)
    p = length(deps)
    it = 0
    while length(deps)>0 && it<=p
        it+=1
        for (k,dep) in deps
            if length(dep)==0
                push!(solved,k)
                pop!(deps,k)
                for (l,ldeps) in deps
                    if k in ldeps
                        pop!(ldeps,k)
                    end
                end
            end
        end
    end
    if it==p+1
        throw("Non triangular system")
    end
    return solved
end


immutable FlatFunctionFactory
        # normalized equations
        equations::Vector{Expr}
        # list of group of (normalized) variables
        arguments::OrderedDict{Symbol, Vector{Symbol}}
        # list of assigned variables
        targets::Vector{Symbol}
        # preamble: definitions
        preamble::OrderedDict{Symbol, Expr}
        # name of function
        funname::Symbol
end

function FlatFunctionFactory(ff::FunctionFactory)

    equations = Expr[]
    for eq in ff.eqs
        # we remove lhs if it is there
        if eq.head == :(=)
            ee = eq.args[2]
        else
            ee = eq
        end
        push!(equations, ee)
    end

    arguments = OrderedDict{Symbol, Vector{Symbol}}()
    # we assume silently :p is not given as an argument in ff
    if isa(ff.args, OrderedDict)
        for k in keys(ff.args)
            arguments[k] = [Dolang.normalize(e) for e in ff.args[k]]
        end
    else
        arguments[:x] = [Dolang.normalize(e) for e in ff.args]
    end
    arguments[:p] = [Dolang.normalize(p) for p in ff.params]

    if length(ff.targets)==0
        targets = [Symbol(string("outv_",i)) for i=1:length(ff.eqs)]
    else
        targets = ff.targets
    end

    npreamble = Dict{Symbol, Expr}()
    for k in keys(ff.defs)
        npreamble[Dolang.normalize((k,0))] = Dolang.normalize(ff.defs[k])
    end
    unknown = [keys(npreamble)...]
    depends  = [Set(intersect(Dolang.list_symbols(eq)[:parameters], unknown)) for eq in values(npreamble)]
    deps = Dict(zip(unknown,depends))
    order = solve_dependency(deps)
    preamble = OrderedDict{Symbol, Expr}()
    for k in order
        preamble[k] = npreamble[k]
    end

    # return equations, arguments, targets, preamble, funname
    FlatFunctionFactory(equations, arguments, targets, preamble, ff.funname)

end

using StaticArrays

# slow and fails shamelessly if only Vectors are supplied
function _getsize(arrays::Union{Vector,<:SVector}...)
    vectors_length = Int[length(a) for a in arrays if isa(a,Vector)]
    @assert length(vectors_length)>0
    maximum(vectors_length)
end
@inline _getobs(x::Vector{<:SVector},i::Int) = x[i]
@inline _getobs(x::SVector,i::Int) = x


function sym_to_sarray(v::Vector{Symbol})
    Expr(:call,:SVector, v...)
end

function sym_to_sarray(v::Matrix{Symbol})
    p,q = size(v)
    Expr(:call,:(SMatrix{$p,$q}), v[:]...)
end

get_first(x) = x[1]

function gen_kernel(fff::FlatFunctionFactory, diff::Vector{Int}; funname=fff.funname, arguments=fff.arguments)
    # diff indices of vars to differentiate
    # 0 for residuals, i for i-th argument

    # prepare equations to write
    outputs = OrderedDict()
    targets = OrderedDict()
    if 0 in diff
        outputs[0] = collect(zip(fff.targets, fff.equations))
    end
    p = length(fff.targets)

    argnames = collect(keys(arguments))
    for d in filter(x->x!=0, diff)
        argname = argnames[d]
        vars = arguments[argname]
        q = length(vars)
        jacexpr = Matrix(p, q)
        for i=1:p
            v_out = fff.targets[i]
            for j=1:q

                expr = Dolang.deriv(fff.equations[i], vars[j])
                sym = Symbol(string("d_",v_out,"_d_",vars[j]))
                jacexpr[i,j] = (sym, expr)
            end
        end
        outputs[d] = jacexpr
    end

    # create function block
    code = []
    for (k,args) in zip(argnames, values(arguments))
        for (i,a) in enumerate(args)
            push!(code, :($a = ($k)[$i]))
        end
    end
    for (k,v) in fff.preamble
        push!(code, :($k = $v))
    end
    for (d,out) in outputs
        for k in out[:]
            push!(code, :($(k[1])=$(k[2])))
        end
    end
    return_args = []
    for (d,out) in outputs
        names = get_first.(out)
        outname = Symbol(string("oo_",d))
        push!(code, :($outname = $(sym_to_sarray(names))))
        push!(return_args, outname)
    end

    push!(code, :(res_=$(Expr(:tuple, return_args...))))
    push!(code, :(return res_))

    # now we construct the function

    # not clear whether we really want to specify staticarrays here
    # it makes everything uselessly painful
    # typed_args = [keys(fff.arguments)...]
    typed_args = [:($k::SVector{$(length(v)), Float64}) for (k,v) in arguments]
    fun_args = Expr(:call, funname, typed_args...)

    Expr(:function, fun_args, Expr(:block, code...))

end

function gen_gufun(fff::FlatFunctionFactory, to_diff::Union{Vector{Int}, Int})

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

    funname = fff.funname

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

            ### if all arguments are 1d.vectors we do vector things
            #
            # if (($(args...)),) isa Tuple{$([:(SVector) for i=1:length(args)]...)}
            #     ret = kernel($(args...))
            #     # $kernel_code_stripped
            #     return $(to_diff isa Int? :(ret[1]) :  :(ret) )
            # end
            # if (($(args...)),) isa Tuple{$([:(Vector{Float64}) for i=1:length(args)]...)}
            #     oo = kernel( $( [ :(SVector( $(e)...)) for e in args]...) )
            #     ret = ( $( [:(Array(oo[$i])) for i=1:length(args_out)]...),)
            #     return $(to_diff isa Int? :(ret[1]) :  :(ret) )
            # end

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


#### The following is




function get_nums(t)
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
            oorders = get_nums(orders) # convert into tuples
            code = gen_kernel(fff, oorders)
            code.args[2]
        end
    end
end


function gen_generated_gufun(fff::FlatFunctionFactory)

    funname = fff.funname

    meta_code = quote
        @generated function $funname(orders, x,y,z,p)
            fff = $(fff) # this is amazing !
            # oorders = get_nums(orders) # convert into tuples
            oorders =[0,1]
            code = gen_gufun(fff, oorders)
            code.args[2]
        end
    end
    println(meta_code)
    meta_code
end
