@testset "minilanguage" begin


import Dolang: Language, construct_object, add_language_elements!, eval_node
import Dolang: FromGreek, ToGreek
import Dolang: LangInvalidCall, LangUnknownTag, ParsingError, yaml_node_from_file
# the two objects replace  imports from Dolo

struct MvNormal
    mu::Vector{Float64}
    Sigma::Matrix{Float64}
end

struct Cartesian
    a::Vector{Float64}
    b::Vector{Float64}
    orders::Vector{Int64}
end

# we create a mini-language with two keywords:
# UNormal(μ=0.0, σ=1.0) # keyword object with greek arguments
# Cartesian # positional


UNormal(;μ::Float64=0.0,σ::Float64=1.0) = MvNormal([μ], σ*ones(1,1))
minilang = Language(Dict())
add_language_elements!(minilang, Dict(
    "!Cartesian"=>Cartesian,
    "!UNormal"=>UNormal
))

obj = construct_object(minilang, "!UNormal", Dict(:σ=>0.1, :μ=> 0.001))
@assert typeof(obj) <: MvNormal

obj = construct_object(minilang, "!Cartesian", ([0.0], [1.0], [20]))
@assert typeof(obj) <: Cartesian

# UNormal doesn't take positional arguments:
@test_throws LangInvalidCall construct_object(minilang, "!UNormal", (0.1, 0.001))
@test_throws LangInvalidCall construct_object(minilang, "!Cartesian", ([0.0], [1.0]) )
@test_throws LangUnknownTag construct_object(minilang, "!NonExisting", ([0.0], [1.0], [20]) )


##

calibration = Dict(:a=>0.2, :b=>0.5)

# use test file

data = yaml_node_from_file("minilanguage.yaml", minilang)

# evaluate terminal nodes directly
res = eval_node( data["equations"][1], calibration )
@assert res == 0.7

# if expression is not recognized or calibration not given, expression is returned as such
res = eval_node( data["equations"][2], calibration )
@assert res=="a + b + c"

# one can evaluate a yaml list directly
res = eval_node( data["equations"], calibration)
@assert res[1] == 0.7
@assert res[2] == "a + b + c"
@assert res[3] == 8
@assert res[4] == 9.0 # we return same types as parsed by yaml


# this raises a parsing exception
@test_throws ParsingError{LangUnknownTag} eval_node(data["exogenous"], calibration)
@test_throws ParsingError{LangUnknownTag} eval_node(data["shocks"], calibration)

# evaluate typed section
gg = eval_node(data["grid"], calibration, minilang)
@test typeof(gg) <: Cartesian
gg = eval_node(data["exogenous"], calibration, minilang)
@test typeof(gg) <: MvNormal

# test greek tolerance

gg = eval_node(data["calibration"], calibration, minilang)
@test tuple(sort([keys(gg)...])... ) == (:mu, :β, :λ, :σ)

gg = eval_node(data["calibration"], calibration, minilang, FromGreek())
@test tuple(sort([keys(gg)...])... ) == ( :beta, :lambda, :mu, :sigma)


@test_throws ParsingError{LangInvalidCall} eval_node(data["exogenous2"], calibration, minilang)
@test_throws ParsingError{LangInvalidCall} eval_node(data["exogenous2"], calibration, minilang, FromGreek())

# that one works because object !
gg = eval_node(data["exogenous2"], calibration, minilang, ToGreek())
typeof(gg) <: MvNormal

end
