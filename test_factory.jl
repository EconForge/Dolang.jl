using StaticArrays
using Dolang, DataStructures, Base.Test

eqs = [:(foo(0) = log(a(0))+b(0)/x(-1)), :(bar(0) = c(1)+u*d(1))]
args = [(:a, -1), (:a, 0), (:b, 0), (:c, 0), (:c, 1), (:d, 1)]
params = [:u]
defs = Dict(:x=>:(a(0)/(1-c(1))))
targets = [(:foo, 0), (:bar, 0)]
funname = :myfun

const flat_args = [(:a, 0), (:b, 1), (:c, -1)]
const grouped_args = OrderedDict(:x=>[(:a, -1),], :y=>[(:a, 0), (:b, 0), (:c, 0)], :z=>[(:c, 1), (:d, 1)])
const flat_params = [:beta, :delta]
const grouped_params = Dict(:p => [:u])

args2 = vcat(args, targets)::Vector{Tuple{Symbol,Int}}


ff = Dolang.FunctionFactory(eqs, args, params, targets=targets, defs=defs,
                            funname=funname)
fff = Dolang.FlatFunctionFactory(ff)



ff2 = Dolang.FunctionFactory(eqs, grouped_args, params, targets=targets, defs=defs,
                            funname=funname)
fff = Dolang.FlatFunctionFactory(ff2)

using StaticArrays


code = Dolang.gen_kernel(fff,[0,1,2]) # residual then derivative w.r.t. first and second
println(code)
code = Dolang.gen_kernel(fff,[0])
println(code)



# we need a module to evaluate the function in
module TestMe
    import Dolang: _get_size, _getobs
    using StaticArrays
end
import TestMe

eval(TestMe,
    quote

        # slow and fails shamelessly if only Vectors are supplied
        function _getsize(arrays::Union{Vector,<:SVector}...)
            vectors_length = Int[length(a) for a in arrays if isa(a,Vector)]
            @assert length(vectors_length)>0
            maximum(vectors_length)
        end
        @inline _getobs(x::Vector{<:SVector},i::Int) = x[i]
        @inline _getobs(x::SVector,i::Int) = x

    end
)


eval(TestMe,
    quote

        # slow and fails shamelessly if only Vectors are supplied
        function _getsize(arrays::Union{Vector,<:SVector}...)
            vectors_length = Int[length(a) for a in arrays if isa(a,Vector)]
            @assert length(vectors_length)>0
            maximum(vectors_length)
        end
        @inline _getobs(x::Vector{<:SVector},i::Int) = x[i]
        @inline _getobs(x::SVector,i::Int) = x

    end
)




code = Dolang.gen_gufun(fff, [1,2,3])
print(code)


myfun = eval(TestMe, code)


#
x = @SVector [1.0]
y = @SVector [2.0, 0.5, 0.3]
z = @SVector [0.4, 0.2]
p = @SVector [0.1]
# res = TestMe.myfun(x,y,z,p);

# now vector
N=10000000
# and test speed
x_vec = reinterpret(SVector{1,Float64}, 1+rand(1,N), (N,))
y_vec = reinterpret(SVector{3,Float64}, rand(3,N), (N,))
z_vec = reinterpret(SVector{2,Float64}, rand(2,N), (N,))
p_vec = reinterpret(SVector{1,Float64}, rand(1,N), (N,))
out_vec = reinterpret(SVector{2,Float64}, rand(2,N), (N,))


xx = Vector(x)
yy = Vector(y)
zz = Vector(z)
pp = Vector(p)
TestMe.myfun(xx, yy, zz, pp)

TestMe.myfun(x_vec, y_vec, z_vec, p_vec)



function test_perf(x, y, z, p, out, k=10)

    for i in 1:k
        TestMe.myfun(x, y, z, p, out)
    end
end

print(code)
@time test_perf(x, y, z_vec, p, (out_vec,), k=100);


xx = Vector(x)
yy = Vector(y)
zz = Vector(z)
pp = Vector(p)
TestMe.myfun(xx, yy, zz, pp)

TestMe.myfun(x, y, z, p)

@time



@time TestMe.myfun( x_vec, y_vec, z_vec, p_vec)

TestMe.myfun!(out_vec, x, y_vec, z_vec, p)
out2 = TestMe.myfun(x, y_vec, z_vec, p)
@assert ( maximum( [maximum(e) for e in (out_vec-out2)] ) )<1e-8

print("Static vectors")
@time TestMe.myfun!(out_vec, x_vec, y_vec, z_vec, p)


# compare with old version

code_old = Dolang.make_function(ff2)
my_fun, my_fun! = eval(Dolang, code_old)

x_vec   = 1+rand(N,1)
y_vec   = rand(N,3)
z_vec   = rand(N,2)
p_vec   = rand(N,1)
out_vec = rand(N,2)

my_fun!(out_vec, x_vec, y_vec, z_vec, p)
print("Old version")

import Dolang: Der, TDer
# @time my_fun(Der{1}, x_vec, y_vec, z_vec, p)


@time my_fun!(out_vec, x_vec, y_vec, z_vec, p)
@time my_fun!(out_vec, x_vec, y_vec, z_vec, p)
