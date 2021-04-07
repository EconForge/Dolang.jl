using Lerche

grammar_path = joinpath(dirname(pathof(@__MODULE__)), "grammar.lark")

txt = read(open(grammar_path), String)
const parser = Lark(txt, 
    parser="lalr", start="formula");

const parser_equation = Lark(txt, 
    parser="lalr", start="equation");

const parser_assignment = Lark(txt, 
    parser="lalr", start="assignment");

    const parser_equation_block = Lark(txt, 
    parser="lalr", start="equation_block");

const parser_assignment_block = Lark(txt, 
    parser="lalr", start="assignment_block");



LTree = Union{Tree, Token}

# sanitize: 
# ensure that all variables have a date
# maybe we should convert numbers into actual numbers here ?

struct Sanitizer <: Transformer 
    variables::Vector{String}
end

Sanitizer() = Sanitizer(String[])

@rule variable(sanitizer::Sanitizer, children) = length(children)==1 ? 
    Tree("variable", Union{Tree,Token}[children[1], Tree("date", [Token("SIGNED_INT2", "0")])]) : 
    Tree("variable", children)

@rule symbol(sanitizer::Sanitizer, children) = (children[1].value in sanitizer.variables) ?
    Tree("variable", Union{Tree,Token}[Tree("name", [Token("NAME", children[1].value )]), Tree("date", [Token("SIGNED_INT2", "0")])]) : 
    Tree("symbol", children)

sanitize(expr::LTree; variables=AbstractString[])::LTree = Lerche.transform(Sanitizer(variables), expr)


struct TimeShifter <: Transformer
    Δt::Int64
end

@rule variable(ts::TimeShifter, children) = begin
    t0 = parse(Int, children[2].children[1].value)
    t1 = t0 + ts.Δt
    t1_s = string(t1)
    name = children[1].children[1].value
    Tree("variable", Union{Tree,Token}[Tree("name", [Token("NAME", name)]), Tree("date", [Token("SIGNED_INT2", t1_s)])])
end 

time_shift(expr::LTree, Δt::Int64)::LTree = Lerche.transform(TimeShifter(Δt), expr)


struct SteadyState <: Transformer
end

@rule variable(t::SteadyState, children) = begin
    name = children[1].children[1].value
    Tree("variable", Union{Tree,Token}[Tree("name", [Token("NAME", name)]), Tree("date", [Token("SIGNED_INT2", "0")])])
end 

steady_state(expr::LTree)::LTree = Lerche.transform(SteadyState(), expr)



import Dolang
import Dolang: stringify


## Converts an expression into a an expression, where all variables are replaced
## by their equation name


# struct Stringifier <: Transformer end

# stringify(variable::String, date::Int) = String(stringify(Symbol(variable), date))
# stringify(::typeof(Symbol), variable::String, date::Int) = (stringify(Symbol(variable), date))

# stringify(variable::String) = String(stringify(Symbol(variable)))
# stringify(::typeof(Symbol), variable::String) = (stringify(Symbol(variable)))

# @rule variable(t::Stringifier, tree) =  stringify(Symbol, tree[1].children[1].value, parse(Int,tree[2].children[1].value))
# @rule symbol(t::Stringifier, tree) =  stringify(Symbol,tree[1].value)


# stringify(expr::LTree) = Lerche.transform(Stringifier(), expr)

# converts to julia expression tree
struct Converter <: Transformer 
    stringify::Bool
end

Converter() = Converter(false)

@rule variable(t::Converter, tree) = begin
    s = Symbol(tree[1].children[1].value)
    d = parse(Int,tree[2].children[1].value)
    if t.stringify
        stringify(s, d)
    else
        :($(s)[$(d)])
    end
end
@rule symbol(t::Converter, tree) = begin
    s = Symbol(tree[1].value)
    if t.stringify
        stringify(s)
    else
        s
    end
end

@rule call(t::Converter, children) =  Expr(:call, Symbol(children[1].value), children[2:end]...)
@rule add(t::Converter, children) =  Expr(:call, :+, children...)
@rule mul(t::Converter, children) =  Expr(:call, :*, children...)
@rule div(t::Converter, children) =  Expr(:call, :/, children...)
@rule pow(t::Converter, children) =  Expr(:call, :^, children...)
@rule neg(t::Converter, children) =  Expr(:call, :-, children...)
@rule sub(t::Converter, children) =  Expr(:call, :-, children...)
@rule number(t::Converter, children) =  parse(Float64, children[1].value)
@rule equality(t::Converter, children) =  :($(children[1]) == $(children[2]))

# this is not supposed to work for equation blocks.
convert(::typeof(Expr), expr::Tree; stringify=true) =  Lerche.transform(Converter(stringify), expr)
convert(::typeof(Expr), x::Number) = x
# convert(::typeof(Expr), s::Symbol) = s
# convert(::typeof(Expr), expr::Token) = expr.value

function parse_equation(s::String; variables=AbstractString[])::LTree
    expr = Lerche.parse(parser_equation, s)
    expr = sanitize(expr; variables=variables)
    return expr
end


function parse_assignment(s::String; variables=AbstractString[])::LTree
    expr = Lerche.parse(parser_assignment, s)
    expr = sanitize(expr; variables=variables)
    return expr
end

function parse_equation_block(s::String; variables=AbstractString[])::LTree
    expr = Lerche.parse(parser_equation_block, s)
    expr = sanitize(expr; variables=variables)
    return expr
end

function parse_assignment_block(s::String; variables=AbstractString[])::LTree
    expr = Lerche.parse(parser_assignment_block, s)
    expr = sanitize(expr; variables=variables)
    return expr
end
