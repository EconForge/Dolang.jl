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


code = Dolang.gen_fun(fff,[0,1,2]) # residual then derivative w.r.t. first and second
println(code)
code = Dolang.gen_fun(fff,[0])
println(code)

# we need a module to evaluate the function in
module TestMe
    using StaticArrays
end
import TestMe
fun = eval(TestMe, code)


x = @SVector [1.0]
y = @SVector [2.0, 0.5, 0.3]
z = @SVector [0.4, 0.2]
p = @SVector [0.1]
res = TestMe.myfun(x,y,z,p);


# now vectorize the function
eval(TestMe,
    quote
        function _getsize(arrays::Union{Vector,<:SVector}...)
            vectors = [a for a in arrays if isa(a,Vector)]
            @assert length(vectors)>0
            max([length(v) for v in vectors]...)
        end
        _getobs(x::Vector{<:SVector},i::Int) = x[i]
        _getobs(x::SVector,i::Int) = x
        function myfun!(out::Vector{SVector{2,Float64}},x,y,z,p)
            N = size(out,1)
            @inbounds @simd for n=1:N
                x_ = _getobs(x,n)
                y_ = _getobs(y,n)
                z_ = _getobs(z,n)
                p_ = _getobs(p,n)
                _res = (myfun(x_,y_,z_,p_))
                out[n] = _res[1]
                # out[n] = (fun(x,y,z,p_))[1]
            end
        end
        function myfun(arrays...)
            N = _getsize(arrays...)
            out = zeros(SVector{2,Float64}, N)
            myfun!(out,arrays...)
            return out
        end
    end
)

# and test speed

N = 1000000
x_vec = reinterpret(SVector{1,Float64}, 1+rand(1,N), (N,))
y_vec = reinterpret(SVector{3,Float64}, rand(3,N), (N,))
z_vec = reinterpret(SVector{2,Float64}, rand(2,N), (N,))
p_vec = reinterpret(SVector{1,Float64}, rand(1,N), (N,))
out_vec = reinterpret(SVector{2,Float64}, rand(2,N), (N,))

isa(out_vec[1], AbstractVector)



TestMe.myfun!(out_vec, x_vec, y_vec, z_vec, p)
TestMe.myfun!(out_vec, x_vec, y_vec, z_vec, p_vec)

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
@time my_fun!(out_vec, x_vec, y_vec, z_vec, p)
