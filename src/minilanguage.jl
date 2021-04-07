import YAML


# convenience extensions of YAML library
import Base.getindex
import Base.keys
import Base.values

Base.length(d::YAML.SequenceNode) = length(d.value)
Base.getindex(d::YAML.SequenceNode, k::Integer) = d.value[k]
Base.iterate(d::YAML.SequenceNode, state=1)  = state>length(d) ? nothing : (d.value[state], state+1)
Base.keys(s::YAML.MappingNode) =  [e[1].value for e=s.value]
Base.values(s::YAML.MappingNode) =  [e[2].value for e=s.value]
Base.getindex(d::YAML.MappingNode, s::AbstractString) = d.value[findfirst(isequal(s),keys(d))][2]
Base.iterate(d::YAML.MappingNode, state=1)  = state>length(d) ? nothing : ((d.value[state][1].value=>d.value[state][2]), state+1)
Base.getindex(d::YAML.MappingNode, s::Symbol) = d[string(s)]
Base.length(d::YAML.MappingNode) = length(d.value)

const symbols_from_greek = Dict(
    :μ=>:mu,
    :σ=>:sigma,
    :Σ=>:Sigma,
    :ρ=>:rho,
    :λ=>:lambda,
    :β=>:beta,
    :δ=>:delta
)
const symbols_to_greek = Dict(v=>k for (k,v) in symbols_from_greek)

#
#  Language object: essentially a table from tag to a constructor
#

struct Language
    objects::Dict{String, Any}
end

Language() = Language(Dict{String, Any}())

function add_language_element!(lang, fun, tag::AbstractString)
    lang.objects[tag] = fun
end
function add_language_element!(lang, fun, tags::Vector{AbstractString})
    for tag in tags
        add_language_element!(lang, fun, tag)
    end
end
function add_language_elements!(lang, elts::AbstractDict{<:AbstractString, <:Any})
    for (k,v) in elts
        add_language_element!(lang, v, k)
    end
end

####
#### Functions to import yaml tree (with locations and tags)
####

const yaml_basis_tags = [
    "tag:yaml.org,2002:null",
    "tag:yaml.org,2002:bool",
    "tag:yaml.org,2002:int",
    "tag:yaml.org,2002:float",
    "tag:yaml.org,2002:binary",
    "tag:yaml.org,2002:timestamp",
    "tag:yaml.org,2002:omap",
    "tag:yaml.org,2002:pairs",
    "tag:yaml.org,2002:set",
    "tag:yaml.org,2002:str",
    "tag:yaml.org,2002:seq",
    "tag:yaml.org,2002:map",
]

# with upcoming version of YAML.jl
function yaml_node_from_string(txt::AbstractString)
    cons = YAML.Constructor()
    YAML.add_multi_constructor!((c,s,m)->m, cons, "tag:yaml.org")
    YAML.add_multi_constructor!((c,s,m)->m, cons, "!")
    data = YAML.load(txt, cons)
    return data
end

# function yaml_node_from_string(txt::AbstractString, minilanguage::Language=Language())
#     # Didn't find how to access the top node more easily.
#     #
#     yml_types = Dict{String,Function}()
#     for tag in yaml_basis_tags
#         yml_types[tag] = (c,n)->n
#     end
#     for tag in keys(minilanguage.objects)
#         yml_types[tag] = (c,n)->n
#     end
#     return YAML.load(txt, yml_types)
# end

function yaml_node_from_file(fn::AbstractString, minilanguage::Language=Language())
    txt = open(f->read(f,String), fn)
    txt = replace(txt, "\r"=>"")
    return yaml_node_from_string(txt) # , minilanguage)
end

##
# Function to evaluate string from calibration
#
# TODO: not safe and hazardous error policy
##

function eval_string(s::AbstractVector{Expr}, d:: AbstractDict{Symbol, <:Number})

        # extract expressions and variable names in proper order
        nms = collect(keys(d))
        exprs = collect(values(d))

        # build expression to Core.evaluate system in correct order
        to_eval = Expr(:block)
        to_eval.args = [:($(i[1])=$(i[2])) for i in zip(nms, exprs)]

        # add one line to return a tuple of all data
        ret = Expr(:tuple); ret.args = s

        # now Core.evaluate and get data
        expr = :(
            let
                $to_eval
                $ret
            end
        )
        data = eval(expr)
end

eval_string(s::Expr, d::AbstractDict{Symbol, <:Number}) = eval_string([s], d)[1]

function eval_string(s::AbstractString, d::AbstractDict{Symbol, <:Number}=Dict{Symbol, Float64}())
        expr = Meta.parse(s)
        eval_string(expr, d)
end

###
# Some errors potentially occuring when parsing yaml structure
##

struct Position
    l1::Int
    c1::Int
    l2::Int
    c2::Int
end

abstract type LanguageError <: Exception end

struct ParsingError{T<:LanguageError} <: Exception
    pos::Position
    error::T
end

function ParsingError(node::YAML.Node, error)
    ms, em = node.start_mark, node.end_mark
    l1, c1 = Int(ms.line), Int(ms.column)
    l2, c2 = Int(em.line), Int(em.column)
    pos = Position(l1, c1, l2, c2)
    return ParsingError(pos, error)
end


struct LangInvalidCall <: LanguageError
    tag::String
    err::MethodError
end

struct LangUnknownTag <: LanguageError
    tag::String
end

function message(e::LangInvalidCall)
    s = IOBuffer()
    showerror(s, e.err)
    msg = String(take!(s))
    return msg
end
message(e::LangUnknownTag) = "Unknown tag: \"$(e.tag)\""

Base.showerror(io::IO, e::LanguageError) = print(io, message(e))
Base.showerror(io::IO, e::ParsingError) = print(io, "Language Error: $(e.pos.l1), $(e.pos.l2): $(message(e.error)))")

#
# construct object from tag and arguments
#

function construct_object(lang, tag, args::AbstractDict{<:Any,<:Any})
    if !( tag in keys(lang.objects))
        throw(LangUnknownTag(tag))
    end
    constructor = lang.objects[tag]
    try
        return constructor(;args...)
    catch err
        throw( LangInvalidCall(tag, err) )
    end
end

function construct_object(lang, tag, args)
    if !(tag in keys(lang.objects))
        throw(LangUnknownTag(tag))
    end
    constructor = lang.objects[tag]
    try
        return constructor(args...)
    catch err
        throw( LangInvalidCall(tag, err) )
    end
end

#
# option types
# maybe this is not needed anymore thanks to constant propagation

abstract type GreekTolerance end
struct FromGreek<:GreekTolerance
    # greek characters are automatically converted to alphanumerical
end
struct ToGreek<:GreekTolerance
    # alphanumerical characters are automatically converted to greek
end
struct NoGreek<:GreekTolerance
    # no conversion
end

###
# evaluate yaml structures
#


# scalar: text, float, int, ...
function eval_node(s::YAML.ScalarNode, calibration::AbstractDict{Symbol, <:Number}, minilang::Language=Language(), greek_tol::GreekTolerance=NoGreek())
    v = s.value
    tag = s.tag
    if tag == "tag:yaml.org,2002:str"
        return try
            eval_string(v, calibration)
        catch err
            # TODO: fix behaviour when type is not recognized
            v
        end
    elseif tag == "tag:yaml.org,2002:int"
        return parse(Int, v)
    elseif tag == "tag:yaml.org,2002:float"
        return parse(Float64, v)
    end
end



# sequences
function eval_node(node::YAML.SequenceNode, calibration::AbstractDict{Symbol, <:Number}, minilang::Language=Language(), greek_tol::GreekTolerance=NoGreek())
    children = [eval_node(ch, calibration, minilang, greek_tol) for ch in node]
    tag = node.tag
    if tag == "tag:yaml.org,2002:seq"
        if typeof(children) <: Vector{Vector{Float64}}
            res = vcat([x' for x in children]...)
            println(res)
            return res
        else
            return children
        end
    else # custom object with positional arguments
        childs = tuple(children...)
        try
            return construct_object(minilang, tag, childs)
        catch err
            throw(ParsingError(node, err))
        end
    end
end

# map
function eval_node(node::YAML.MappingNode, calibration::AbstractDict{Symbol, <:Number}, minilang::Language=Language(), greek_tol::GreekTolerance=NoGreek())
    d = Dict()
    for i=1:length(node)
        k = Symbol(node.value[i][1].value)
        if typeof(greek_tol)<:FromGreek
            if haskey(symbols_from_greek, k)
                k = symbols_from_greek[k]
            end
        elseif typeof(greek_tol)<:ToGreek
            if haskey(symbols_to_greek, k)
                k = symbols_to_greek[k]
            end
        end
        v = eval_node(node.value[i][2], calibration, minilang, greek_tol)
        d[k] = v
    end
    if node.tag == "tag:yaml.org,2002:map"
        return d
    else # custom object with positional arguments
        try
            return construct_object(minilang, node.tag, d)
        catch err
            throw(ParsingError(node, err))
        end
    end
end
