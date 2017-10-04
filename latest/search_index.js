var documenterSearchIndex = {"docs": [

{
    "location": "index.html#",
    "page": "Dolang.jl",
    "title": "Dolang.jl",
    "category": "page",
    "text": ""
},

{
    "location": "index.html#Dolang.jl-1",
    "page": "Dolang.jl",
    "title": "Dolang.jl",
    "category": "section",
    "text": "CurrentModule = DolangDolang is an implementation of the dolo language and provides utilities for symbolic manipulation and efficient compilation of symbols and expressions into performant Julia functions.To read more about the available routines, read the Symbolic manipulation and Dolang's compiler chapters. To get started right away with using the Dolang compiler skip directly to the Examples."
},

{
    "location": "symbolic.html#",
    "page": "Symbolic manipulation",
    "title": "Symbolic manipulation",
    "category": "page",
    "text": ""
},

{
    "location": "symbolic.html#Symbolic-manipulation-1",
    "page": "Symbolic manipulation",
    "title": "Symbolic manipulation",
    "category": "section",
    "text": "DocTestSetup = quote\n    using Dolang\nendOne of Dolang's key features is providing an _extensible_ and _consistent_ set of routines for doing operations on symbolic objects. We'll discuss each of these routines in turnPages = [\"symbolic.md\"]\nDepth = 2"
},

{
    "location": "symbolic.html#normalize-1",
    "page": "Symbolic manipulation",
    "title": "normalize",
    "category": "section",
    "text": "The normalize function converts expressions and symbols into Dolang's internal representation. There are various methods for this function:normalize(::Symbol)Examplesjulia> normalize(:c)\n:_c_\n\njulia> normalize(:_c)\n:__c_\n\njulia> normalize(:_c_)\n:_c_\n\njulia> normalize(:x_ijk)\n:_x_ijk_\n\njulia> normalize(:x_ijk_)\n:_x_ijk__\n\njulia> normalize(:_x_ijk_)\n:_x_ijk_normalize(::Number)Examplesjulia> normalize(-1)\n-1\n\njulia> normalize(0)\n0\n\njulia> normalize(1)\n1innormalize(::Symbol, ::Integer)Examplesjulia> normalize(:x, 0)\n:_x__0_\n\njulia> normalize(:x, 1)\n:_x__1_\n\njulia> normalize(:x, -1)\n:_x_m1_\n\njulia> normalize(:x, -100)\n:_x_m100_\n\njulia> normalize(\"x\", 0)\n:_x__0_\n\njulia> normalize(\"x\", 1)\n:_x__1_\n\njulia> normalize(\"x\", -1)\n:_x_m1_\n\njulia> normalize(\"x\", -100)\n:_x_m100_normalize(::Tuple{Symbol,Integer})Examplesjulia> normalize((:x, 0))\n:_x__0_\n\njulia> normalize((:x, 1))\n:_x__1_\n\njulia> normalize((:x, -1))\n:_x_m1_\n\njulia> normalize((:x, -100))\n:_x_m100_normalize(::Expr)Examplesjulia> normalize(:(a(1) - b - c(2) + d(-1)))\n:(((_a__1_ - _b_) - _c__2_) + _d_m1_)\n\njulia> normalize(:(sin(x)))\n:(sin(_x_))\n\njulia> normalize(:(sin(x(0))))\n:(sin(_x__0_))\n\njulia> normalize(:(dot(x, y(1))))\n:(dot(_x_, _y__1_))\n\njulia> normalize(:(beta * c(0)/c(1) * (alpha*y(1)/k(1) * (1-mu(1)) + 1 - delta_k) - 1))\n:(((_beta_ * _c__0_) / _c__1_) * ((((_alpha_ * _y__1_) / _k__1_) * (1 - _mu__1_) + 1) - _delta_k_) - 1)\n\njulia> normalize(:(x = log(y(-1))); targets=[:x])  # with targets\n:(_x_ = log(_y_m1_))\n\njulia> normalize(:(x = log(y(-1))))  # without targets\n:(log(_y_m1_) - _x_)normalize(::String)Examples: see above for morejulia> normalize(\"x = log(y(-1))\"; targets=[:x])  # with targets\n:(_x_ = log(_y_m1_))\n\njulia> normalize(\"x = log(y(-1))\")  # without targets\n:(log(_y_m1_) - _x_)normalize(::Vector{Expr})Examples:julia> normalize([:(sin(x(0))), :(dot(x, y(1))), :(x = log(y(-1)))])\nquote\n    sin(_x__0_)\n    dot(_x_, _y__1_)\n    log(_y_m1_) - _x_\nend\n\njulia> normalize([:(sin(x(0))), :(dot(x, y(1))), :(x = log(y(-1)))], targets=[:x])\nquote\n    sin(_x__0_)\n    dot(_x_, _y__1_)\n    _x_ = log(_y_m1_)\nend"
},

{
    "location": "symbolic.html#Dolang.time_shift-Tuple{Expr,Integer,Set{Symbol},Associative}",
    "page": "Symbolic manipulation",
    "title": "Dolang.time_shift",
    "category": "Method",
    "text": "time_shift(ex::Expr, shift::Integer,\n           functions::Set{Symbol}=Set{Symbol}(),\n           defs::Associative=Dict())\n\nRecursively apply a time_shift to ex according to the following rules based on the form of ex (list below has the form \"contents of ex: return expr\"):\n\nvar(n::Integer): time_shift(var, args, shift + n, defs)\nf(other...): if f is in functions or is a known dolang funciton (see Dolang.DOLANG_FUNCTIONS) return f(map(i -> time_shift(i, args, shift, defs), other)), otherwise error\nAny other Expr: Return an Expr with the same head, with time_shift applied on each of ex.args\n\n\n\n"
},

{
    "location": "symbolic.html#Dolang.time_shift-Tuple{Expr,Integer}",
    "page": "Symbolic manipulation",
    "title": "Dolang.time_shift",
    "category": "Method",
    "text": "time_shift(ex::Expr, shift::Int=0;\n           functions::Union{Set{Symbol},Vector{Symbol}}=Vector{Symbol}(),\n           defs::Associative=Dict())\n\nVersion of time_shift where functions and defs are keyword arguments with default values.\n\n\n\n"
},

{
    "location": "symbolic.html#Dolang.time_shift-Tuple{Symbol,Integer,Set{Symbol},Associative}",
    "page": "Symbolic manipulation",
    "title": "Dolang.time_shift",
    "category": "Method",
    "text": "time_shift(s::Symbol, shift::Integer,\n           functions::Set{Symbol}=Set{Symbol}(),\n           defs::Associative=Dict())\n\nIf s is in defs, then return the shifted version of the definition\n\nOtherwise return s\n\n\n\n"
},

{
    "location": "symbolic.html#Dolang.time_shift-Tuple{Number}",
    "page": "Symbolic manipulation",
    "title": "Dolang.time_shift",
    "category": "Method",
    "text": "time_shift(x::Number, other...)\n\nReturn x for all values of other\n\n\n\n"
},

{
    "location": "symbolic.html#time_shift-1",
    "page": "Symbolic manipulation",
    "title": "time_shift",
    "category": "section",
    "text": "time_shift shifts an expression by a specified number of time periods. As with normalize, there are many methods for this function, which will will describe one at a time.time_shift(::Expr, ::Integer, ::Set{Symbol}, ::Associative)\ntime_shift(::Expr, ::Integer)Examplesjulia> defs = Dict(:a=>:(b(-1)/c));\n\njulia> defs2 = Dict(:a=>:(b(-1)/c(0)));\n\njulia> funcs = [:foobar];\n\njulia> shift = 1;\n\njulia> time_shift(:(a+b(1) + c), shift)\n:(a + b(2) + c)\n\njulia> time_shift(:(a+b(1) + c(0)), shift)\n:(a + b(2) + c(1))\n\njulia> time_shift(:(a+b(1) + c), shift, defs=defs)\n:(b(0) / c + b(2) + c)\n\njulia> time_shift(:(a+b(1) + c(0)), shift, defs=defs)\n:(b(0) / c + b(2) + c(1))\n\njulia> time_shift(:(a+b(1) + c(0)), shift, defs=defs2)\n:(b(0) / c(1) + b(2) + c(1))\n\njulia> time_shift(:(a+b(1) + foobar(c)), shift, functions=funcs)\n:(a + b(2) + foobar(c))\n\njulia> time_shift(:(a+b(1) + foobar(c)), shift, defs=defs, functions=funcs)\n:(b(0) / c + b(2) + foobar(c))\n\njulia> shift = -1;\n\njulia> time_shift(:(a+b(1) + c), shift)\n:(a + b(0) + c)\n\njulia> time_shift(:(a+b(1) + c(0)), shift)\n:(a + b(0) + c(-1))\n\njulia> time_shift(:(a+b(1) + c), shift, defs=defs)\n:(b(-2) / c + b(0) + c)\n\njulia> time_shift(:(a+b(1) + c(0)), shift, defs=defs)\n:(b(-2) / c + b(0) + c(-1))\n\njulia> time_shift(:(a+b(1) + c(0)), shift, defs=defs2)\n:(b(-2) / c(-1) + b(0) + c(-1))\n\njulia> time_shift(:(a+b(1) + foobar(c)), shift, functions=funcs)\n:(a + b(0) + foobar(c))\n\njulia> time_shift(:(a+b(1) + foobar(c)), shift, defs=defs, functions=funcs)\n:(b(-2) / c + b(0) + foobar(c))time_shift(::Symbol, ::Integer, ::Set{Symbol}, ::Associative)Examplesjulia> defs = Dict(:a=>:(b(-1)/c));\n\njulia> defs2 = Dict(:a=>:(b(-1)/c(0)));\n\njulia> shift = 1;\n\njulia> funcs = Set([:foobar]);\n\njulia> time_shift(:a, shift, funcs, Dict())\n:a\n\njulia> time_shift(:a, shift, funcs, defs)\n:(b(0) / c)\n\njulia> time_shift(:a, shift, funcs, defs2)\n:(b(0) / c(1))\n\njulia> time_shift(:b, shift, funcs, defs)\n:b\n\njulia> shift = -1;\n\njulia> time_shift(:a, shift, funcs, Dict())\n:a\n\njulia> time_shift(:a, shift, funcs, defs)\n:(b(-2) / c)\n\njulia> time_shift(:a, shift, funcs, defs2)\n:(b(-2) / c(-1))\n\njulia> time_shift(:b, shift, funcs, defs)\n:btime_shift(::Number)Examplesjulia> time_shift(1)\n1\n\njulia> time_shift(2)\n2\n\njulia> time_shift(-1)\n-1\n\njulia> time_shift(-2)\n-2"
},

{
    "location": "symbolic.html#Dolang.steady_state-Tuple{Symbol,Set{Symbol},Associative}",
    "page": "Symbolic manipulation",
    "title": "Dolang.steady_state",
    "category": "Method",
    "text": "steady_state(s, functions::Set{Symbol}, defs::Associative)\n\nIf s is not a key in defs, return the steady state version of the  s (essentially s(0)).\n\nOtherwise, return steady_state(defs[s], functions, defs)\n\n\n\n"
},

{
    "location": "symbolic.html#Dolang.steady_state-Tuple{Expr,Set{Symbol},Associative}",
    "page": "Symbolic manipulation",
    "title": "Dolang.steady_state",
    "category": "Method",
    "text": "steady_state(ex::Expr, functions::Set{Symbol}, defs::Associative)\n\nReturn the steady state version of ex, where all symbols in args always appear at time 0\n\n\n\n"
},

{
    "location": "symbolic.html#Dolang.steady_state-Tuple{Expr}",
    "page": "Symbolic manipulation",
    "title": "Dolang.steady_state",
    "category": "Method",
    "text": "steady_state(ex::Expr;\n             functions::Vector{Symbol}=Vector{Symbol}(),\n             defs::Associative=Dict())\n\nVersion of steady_state where functions and defs are keyword arguments with default values\n\n\n\n"
},

{
    "location": "symbolic.html#steady_state-1",
    "page": "Symbolic manipulation",
    "title": "steady_state",
    "category": "section",
    "text": "The steady_state function will set the period for all \"timed\" variables to 0.steady_state(::Symbol, ::Set{Symbol}, ::Associative)julia> defs = Dict(:a=>:(b(-1)/c + d), :d=>:(exp(b(0))));\n\njulia> steady_state(:c, Set{Symbol}(), defs)\n:c\n\njulia> steady_state(:d, Set{Symbol}(), defs)\n:(exp(b))\n\njulia> steady_state(:a, Set{Symbol}(), defs)  # recursive def resolution\n:(b / c + exp(b))\n\njulia> steady_state(1, Set{Symbol}(), defs)\n1\n\njulia> steady_state(-1, Set{Symbol}(), defs)\n-1Examplessteady_state(::Expr, ::Set{Symbol}, ::Associative)\nsteady_state(::Expr)Examplesjulia> steady_state(:(a+b(1) + c))\n:(a + b + c)\n\njulia> steady_state(:(a+b(1) + c))\n:(a + b + c)\n\njulia> steady_state(:(a+b(1) + c), defs=Dict(:a=>:(b(-1)/c)))\n:(b / c + b + c)\n\njulia> steady_state(:(a+b(1)+c+foobar(c)))\nERROR: Dolang.UnknownFunctionError(:foobar, \"Unknown function foobar\")\n\njulia> steady_state(:(a+b(1) + foobar(c)), functions=[:foobar])\n:(a + b + foobar(c))"
},

{
    "location": "symbolic.html#Dolang.list_symbols!-Tuple{Any,Expr,Set{Symbol}}",
    "page": "Symbolic manipulation",
    "title": "Dolang.list_symbols!",
    "category": "Method",
    "text": "list_symbols!(out, s::Expr, functions::Set{Symbol})\n\nWalk the expression and populate out according to the following rules for each type of subexpression encoutered:\n\ns::Symbol: add s to out[:parameters]\nx(i::Integer): Add (x, i) out out[:variables]\nAll other function calls: add any arguments to out according to above rules\nAnything else: do nothing.\n\n\n\n"
},

{
    "location": "symbolic.html#Dolang.list_symbols-Tuple{Expr}",
    "page": "Symbolic manipulation",
    "title": "Dolang.list_symbols",
    "category": "Method",
    "text": "list_symbols(::Expr;\n             functions::Union{Set{Symbol},Vector{Symbol}}=Set{Symbol}())\n\nConstruct an empty Dict{Symbol,Any} and call list_symbols! to populate it\n\n\n\n"
},

{
    "location": "symbolic.html#list_symbols-1",
    "page": "Symbolic manipulation",
    "title": "list_symbols",
    "category": "section",
    "text": "list_symbols will walk an expression and determine which symbols are used as potentially time varying variables and which symbols are used as static parameters.list_symbols!(out, ::Expr, ::Set{Symbol})\nlist_symbols(::Expr)Examplesjulia> list_symbols(:(a + b(1) + c))\nDict{Symbol,Any} with 2 entries:\n  :variables  => Set(Tuple{Symbol,Int64}[(:b, 1)])\n  :parameters => Set(Symbol[:a, :c])\n\njulia> list_symbols(:(a + b(1) + c + b(0)))\nDict{Symbol,Any} with 2 entries:\n  :variables  => Set(Tuple{Symbol,Int64}[(:b, 1), (:b, 0)])\n  :parameters => Set(Symbol[:a, :c])\n\njulia> list_symbols(:(a + b(1) + c + b(0) + foobar(x)))\nERROR: Dolang.UnknownFunctionError(:foobar, \"Unknown function foobar\")\n\njulia> list_symbols(:(a + b(1) + c + b(0) + foobar(x)), functions=Set([:foobar]))\nDict{Symbol,Any} with 2 entries:\n  :variables  => Set(Tuple{Symbol,Int64}[(:b, 1), (:b, 0)])\n  :parameters => Set(Symbol[:a, :c, :x])"
},

{
    "location": "symbolic.html#Dolang.list_variables-Tuple{Any}",
    "page": "Symbolic manipulation",
    "title": "Dolang.list_variables",
    "category": "Method",
    "text": "list_variables(\n    arg;\n    functions::Union{Set{Symbol},Vector{Symbol}}=Set{Symbol}()\n)\n\nReturn the :variables key that results from calling list_symbols. The type of the returned object will be a Set of Tuple{Symbol,Int}, where each item describes the symbol that appeared and the corresponding date.\n\n\n\n"
},

{
    "location": "symbolic.html#list_variables-1",
    "page": "Symbolic manipulation",
    "title": "list_variables",
    "category": "section",
    "text": "list_variables will return the :variables key that results from calling list_symbols. The type of the returned object will be a Set of Tuple{Symbol,Int}, where each item describes the symbol that appeared and the corresponding date.list_variables(:Any)Examplesjulia> list_variables(:(a + b(1) + c))\nSet(Tuple{Symbol,Int64}[(:b, 1)])\n\njulia> list_variables(:(a + b(1) + c + b(0)))\nSet(Tuple{Symbol,Int64}[(:b, 1), (:b, 0)])\n\njulia> list_variables(:(a + b(1) + c + b(0) + foobar(x)))\nERROR: Dolang.UnknownFunctionError(:foobar, \"Unknown function foobar\")\n\njulia> list_variables(:(a + b(1) + c + b(0) + foobar(x)), functions=Set([:foobar]))\nSet(Tuple{Symbol,Int64}[(:b, 1), (:b, 0)])"
},

{
    "location": "symbolic.html#Dolang.list_parameters-Tuple{Any}",
    "page": "Symbolic manipulation",
    "title": "Dolang.list_parameters",
    "category": "Method",
    "text": "list_parameters(\n    arg;\n    functions::Union{Set{Symbol},Vector{Symbol}}=Set{Symbol}()\n)\n\nReturn the :parameters key that results from calling list_symbols. The returned object will be Set{Symbol} where each item represents the symbol that appeared without a date.\n\n\n\n"
},

{
    "location": "symbolic.html#list_parameters-1",
    "page": "Symbolic manipulation",
    "title": "list_parameters",
    "category": "section",
    "text": "list_parameters will return the :parameters key that results from calling list_symbols. The returned object will be Set{Symbol} where each item represents the symbol that appeared without a date.list_parameters(::Any)Examplesjulia> list_variables(:(a + b(1) + c + b(0) + foobar(x)), functions=Set([:foobar]))\nSet(Tuple{Symbol,Int64}[(:b, 1), (:b, 0)])\n\njulia> list_parameters(:(a + b(1) + c))\nSet(Symbol[:a, :c])\n\njulia> list_parameters(:(a + b(1) + c + b(0)))\nSet(Symbol[:a, :c])\n\njulia> list_parameters(:(a + b(1) + c + b(0) + foobar(x)))\nERROR: Dolang.UnknownFunctionError(:foobar, \"Unknown function foobar\")\n\njulia> list_parameters(:(a + b(1) + c + b(0) + foobar(x)), functions=Set([:foobar]))\nSet(Symbol[:a, :c, :x])"
},

{
    "location": "symbolic.html#Dolang.subs-Tuple{Union{Expr, Number, Symbol},Any,Union{Expr, Number, Symbol},Set{Symbol}}",
    "page": "Symbolic manipulation",
    "title": "Dolang.subs",
    "category": "Method",
    "text": "subs(ex::Union{Expr,Symbol,Number}, from, to::Union{Symbol,Expr,Number}, funcs::Set{Symbol})\n\nApply a substituion where all occurances of from in ex are replaced by to.\n\nNote that to replace something like x(1) from must be the canonical form for that expression: (:x, 1)\n\n\n\n"
},

{
    "location": "symbolic.html#Dolang.subs-Tuple{Expr,Associative,Set{Symbol}}",
    "page": "Symbolic manipulation",
    "title": "Dolang.subs",
    "category": "Method",
    "text": "subs(ex::Union{Expr,Symbol,Number}, d::Associative,\n     variables::Set{Symbol},\n     funcs::Set{Symbol})\n\nApply substituions to ex so that all keys in d are replaced by their values\n\nNote that the keys of d should be the canonical form of variables you wish to substitute. For example, to replace x(1) with b/c you need to have the entry (:x, 1) => :(b/c) in d.\n\nThe one exception to this rule is that a key :k is treated the same as (:k, 0).\n\n\n\n"
},

{
    "location": "symbolic.html#Dolang.subs-Tuple{Expr,Associative}",
    "page": "Symbolic manipulation",
    "title": "Dolang.subs",
    "category": "Method",
    "text": "subs(ex::Union{Expr,Symbol,Number}, d::Associative;\n     variables::Set{Symbol},\n     functions::Set{Symbol})\n\nVerison of subs where variables and functions are keyword arguments with default values\n\n\n\n"
},

{
    "location": "symbolic.html#subs-1",
    "page": "Symbolic manipulation",
    "title": "subs",
    "category": "section",
    "text": "subs will replace a symbol or expression with a different value, where the value has type Union{Symbol,Expr,Number}The first method of this function is very specific, and matches only a particular pattern:subs(::Union{Expr,Symbol,Number}, from, ::Union{Symbol,Expr,Number}, ::Set{Symbol})Examplesjulia> subs(:(a + b(1) + c), :a, :(b(-1)/c + d), Set{Symbol}())\n:((b(-1) / c + d) + b(1) + c)\n\njulia> subs(:(a + b(1) + c), :d, :(b(-1)/c + d), Set{Symbol}())\n:(a + b(1) + c)subs(::Expr, ::Associative, ::Set{Symbol})\nsubs(::Expr, ::Associative)Examplesjulia> ex = :(a + b);\n\njulia> d = Dict(:b => :(c/a), :c => :(2a));\n\njulia> subs(ex, d)  # subs is not recursive -- c is not replaced\n:(a + c / a)"
},

{
    "location": "symbolic.html#Dolang.csubs-Tuple{Expr,Associative,Set{Symbol}}",
    "page": "Symbolic manipulation",
    "title": "Dolang.csubs",
    "category": "Method",
    "text": "csubs(ex::Union{Symbol,Expr,Number}, d::Associative,\n      variables::Set{Symbol}=Set{Symbol}(),\n      funcs::Set{Symbol}=Set{Symbol}())\n\nRecursively apply substitutions to ex such that all items that are a key in d are replaced by their associated values. Different from subs(x, d) in that definitions in d are allowed to depend on one another and will all be fully resolved here.\n\nExample\n\nex = :(a + b)\nd = Dict(:b => :(c/a), :c => :(2a))\nsubs(ex, d)  # returns :(a + c / a)\ncsubs(ex, d)  # returns :(a + (2a) / a)\n\n\n\n"
},

{
    "location": "symbolic.html#Dolang.csubs-Tuple{Expr,Associative}",
    "page": "Symbolic manipulation",
    "title": "Dolang.csubs",
    "category": "Method",
    "text": "csubs(ex::Union{Symbol,Expr,Number}, d::Associative;\n      functions::Union{Vector{Symbol},Set{Symbol}}=Set{Symbol}())\n\nVerison of csubs where variables and functions are keyword arguments.\n\n\n\n"
},

{
    "location": "symbolic.html#csubs-1",
    "page": "Symbolic manipulation",
    "title": "csubs",
    "category": "section",
    "text": "csubs is closely related to subs, but is more Clever and applies substitutions recursively.csubs(::Expr, ::Associative, ::Set{Symbol})\ncsubs(::Expr, ::Associative)Examplesjulia> ex = :(a + b);\n\njulia> d = Dict(:b => :(c/a), :c => :(2a));\n\njulia> csubs(ex, d)  # csubs is recursive -- c is replaced\n:(a + (2a) / a)\n\njulia> d1 = Dict(:monty=> :python, :run=>:faster, :eat=>:more);\n\njulia> csubs(:(monty(run + eat, eat)), d1)\n:(python(faster + more, more))\n\njulia> csubs(:(a + b(0) + b(1)), Dict(:b => :(c(0) + d(1))))\n:(a + (c(0) + d(1)) + (c(1) + d(2)))\n\njulia> csubs(:(a + b(0) + b(1)), Dict((:b, 1) => :(c(0) + d(1))))\n:(a + b(0) + (c(0) + d(1)))\n\njulia> csubs(:(a + b(0) + b(1)), Dict(:b => :(c + d(1))))\n:(a + (c + d(1)) + (c + d(2)))\n\njulia> csubs(:(a + b(0) + b(1)), Dict((:b, 1) => :(c + d(1))))\n:(a + b(0) + (c + d(1)))\n\njulia> d = Dict((:b, 0) => :(c + d(1)), (:b, 1) => :(c(100) + d(2)));\n\njulia> csubs(:(a + b + b(1)), d)\n:(a + (c + d(1)) + (c(100) + d(2)))"
},

{
    "location": "compiler.html#",
    "page": "Dolang's compiler",
    "title": "Dolang's compiler",
    "category": "page",
    "text": ""
},

{
    "location": "compiler.html#Dolang's-compiler-1",
    "page": "Dolang's compiler",
    "title": "Dolang's compiler",
    "category": "section",
    "text": "The second main component of Dolang is a compiler that leverages its Symbolic manipulation routines to produce efficient Julia functions to evaluate equations and systems of equations."
},

{
    "location": "compiler.html#Dolang.FunctionFactory",
    "page": "Dolang's compiler",
    "title": "Dolang.FunctionFactory",
    "category": "Type",
    "text": "FunctionFactory{T4}([dispatch::Type{T4}], eqs::Vector{Expr},\n                     args::ArgType, params::ParamType; targets=Symbol[],\n                     defs=Dict{Symbol,Any}(), funname::Symbol=:anon)\n\nConstruct a FunctionFactory that evaluates eqs using args and params.\n\nargs and params can either be flat Vector of Dolang symbols (not just julia Symbols), or an associative mapping from a grouped argument name, to a list of symbols in that group. See examples below:\n\n# if ...\nargs = Dict(:x => [(:a, 0), (:b, 0)], :X => [(:a, 1)])\nparams = [:beta, :delta]\n\n# compiled function would have arguments ...\n# funname(..., x, X, p)\n# where length(x) = 2, length(X) = 1, length(p) = 2\n\n# if ...\nargs = [(:a, 0), (:b, 0), (:a, 1)]\nparams = [:beta, :delta]\n\n# compiled function would have arguments ...\n# funname(..., V, p)\n# where length(V) = 3, length(p) = 2\n\nOptional function arguments have the following purposes:\n\nfunname: instruct the Dolang compiler that the compiled function should have a particular name\ntargets: If non-empty and the symbols listed in targets and eqs contains statements of the form lhs = rhs –\ndefs: Recursively substitute definitions into eqs (see csubs for more info)\ndispatch: If this argument is passed, then the Dolang compiler will generate code for a function whose first argument must be an instance of type T4. This can be used to compile functions with the same funname, but different behavior based on the type of dispatch. Note that the argument to FunctionFactory must be the name of a type, not an instance of a type (e.g. Float64 instead of 1.0), but when calling the compiled code you must pass an instance instead of the name of the type (e.g. funname(1.0, ...) not funname(Float64, ...))\n\n\n\n"
},

{
    "location": "compiler.html#FunctionFactory-1",
    "page": "Dolang's compiler",
    "title": "FunctionFactory",
    "category": "section",
    "text": "The compiler mainly operates through one main type, the FunctionFactory.FunctionFactory"
},

{
    "location": "compiler.html#Dolang.make_function-Tuple{Dolang.FunctionFactory}",
    "page": "Dolang's compiler",
    "title": "Dolang.make_function",
    "category": "Method",
    "text": "make_function(ff::FunctionFactory)\n\nCompile a function using data in ff; with methods for\n\nvarious order of derivative\nAllocating output arguments\nNon-allocating functions that mutate the input argument\n(partially-)Vectorized evaluation\n\nSee FunctionFactory for a description of how the fields of ff impact the generated code.\n\nIn non-vectorized evaluation, all function arguments should be vectors and will be unpacked into scalars according to ff.args and ff.params. If any argument is an  AbstractMatrix, then each column of the matrix is assumed to be multiple observations of a single variable. All matrix arguments must have the same number of rows. Let this number be n. Any arguments passed as vectors will be implicitly repeated n times and the function will be evaluated with these vectors and the n observations of each matrix argument.\n\nNote\n\nThe output will be an @generated function that can be evaluated at arbitrary order of analytical derivative – with derivative computation and function compilation happening at runtime upon the user's first request to evaluate that order derivative.\n\n\n\n"
},

{
    "location": "compiler.html#Dolang.make_function-Tuple{Array{Expr,1},AbstractArray{T,1} where T,AbstractArray{T,1} where T}",
    "page": "Dolang's compiler",
    "title": "Dolang.make_function",
    "category": "Method",
    "text": "make_function(\n    eqs::Vector{Expr}, variables::AbstractVector,\n    to_diff::AbstractVector=1:length(variables);\n    dispatch::DataType=SkipArg,\n    targets=Symbol[], name::Symbol=:anon,\n    defs=Dict()\n)\n\nCompile a Julia function by first constructing a FunctionFactory instance where\n\nargs is equal to variables[to_diff]\nparams is equal to variables[setdiff(to_diff, 1:length(variables))]\neqs, dispatch, defs, targets, and name are passed along to the FunctionFactory as arguments with the same name (except name, which becomes the funname argument to FunctionFactory)\n\nSee make_function(ff::FunctionFactory) for more details.\n\nThis method is less flexible than constructing the FunctionFactory by hand because you can only create that have one vector for arguments and one vector for symbols. Meaning you cannot construct an associative mapping for args or params that groups symbols together.\n\n\n\n"
},

{
    "location": "compiler.html#make_function-1",
    "page": "Dolang's compiler",
    "title": "make_function",
    "category": "section",
    "text": "Once an instance of FunctionFactory is created, a single function is used to compile Julia a function with various methods for evaluating different orders of derivative of the factory's expressions. This function is named make_function and is calledmake_function(::FunctionFactory)There is also a convenience method of make_function that takes built-in Julia objects as inputs, constructs the FunctionFactory for you, then calls the above method:make_function(::Vector{Expr}, ::AbstractVector, ::AbstractVector)"
},

{
    "location": "examples.html#",
    "page": "Examples",
    "title": "Examples",
    "category": "page",
    "text": ""
},

{
    "location": "examples.html#Examples-1",
    "page": "Examples",
    "title": "Examples",
    "category": "section",
    "text": "Let's show some examples of how use Dolang to compile simple Julia functions."
},

{
    "location": "examples.html#Basic-example-1",
    "page": "Examples",
    "title": "Basic example",
    "category": "section",
    "text": "First, let's construct some equations, arguments, and parametersusing Dolang\neqs = [\n    :(foo(0) = log(a(0))+b(0)/x(-1)),\n    :(bar(0) = c(1)+u*d(1))\n]\nargs = [(:a, -1), (:a, 0), (:b, 0), (:c, 0), (:c, 1), (:d, 1)]\nparams = [:u]\ndefs = Dict(:x=>:(a(0)/(1-c(1))))\ntargets = [(:foo, 0), (:bar, 0)]\n\nff = FunctionFactory(\n    eqs, args, params, targets=targets, defs=defs, funname=:myfun\n)Now that we have our FunctionFactory we can generate some code (warning, the generated code is not pretty):code = make_function(ff)\nprint(code) # hideIn order to actually call methods of myfun we need to call eval on our code:eval(code)And now we can call itV = [1.5, 2.5, 1.0, 2.0, 3.0, 1.1]\np = [0.5]\nmyfun(V, p)We can call the vectorized version:myfun([V V]', p)Or the mutating one:out = zeros(2)\nmyfun!(out, V, p)\noutWe can evaluate the Jacobian (first derivative matrix)myfun(Der{1}, V, p)... or the Hessianmyfun(Der{2}, V, p)... or higher order derivativesmyfun(Der{3}, V, p)myfun(Der{10}, V, p)Note that the Hessian is returned in as a SparseMatrixCSC where the columns are ordered with the first variable increasing faster. In this example, the (1, 4) element of the Hessian will be the second partial derivative of the first equation (the 1) with fourth and first variables in args.For all higher order derivatives the return value is a Vector{Dict{NTuple{N,Int}, Float64}}, where N is the order of derivative. The keys of each dict will be non-increasing so mixed partials are only computed and stored once. Notice that for the order 3 derivatives an entry for (1, 1, 3) appears, but not for (1, 3, 1) or (3, 1, 1). The user of these routines is required to handle the equality of partials."
},

{
    "location": "examples.html#With-dispatch-1",
    "page": "Examples",
    "title": "With dispatch",
    "category": "section",
    "text": "Now lets consider an example that leverages the dispatch argument. In this case we will use the convenience method for make function with signature: make_function(::Vector{Expr}, ::AbstractVector, ::AbstractVector). We also will not show the compiled codeusing Dolang  # hide\neqs = [\n    :(sin(x(0)) + exp(2*x(1))),\n    :(y(0) / (2 * (1 - β))),\n]\n# NOTE: Arguments can be pre-normalized and contain unicode\nvariables = [(:x, 0), (:y, 0), :_x__1_, :β]  \nto_diff = 1:3\ncode = make_function(eqs, variables, to_diff, dispatch=Int, name=:my_int_fun)\neval(code)Let's check the methods of my_int_fun and my_int_fun!:methods(my_int_fun)methods(my_int_fun!)Notice that because we set the dispatch argument to Int, all methods of either the mutating or allocating function require that an Int is passed to direct dispatch.Let's try calling our functionV = [π, 5.0, 0.8]\np = [0.98]\nmy_int_fun(V, p)  # doesn't workThe above fails withERROR: MethodError: no method matching my_int_fun(::Array{Float64,1}, ::Array{Float64,1})Now if we call that method where the first argument is an instance of Int, it will workV = [π, 5.0, 0.8]  # hide\np = [0.98]  # hide\nmy_int_fun(1, V, p)The actual integer we pass doesn't mattermy_int_fun(10_000_001, V, p)We can still evaluate derivativesmy_int_fun(Der{1}, 1, V, p)Notice that the order of derivative comes first, then the dispatch argument, then all other function arguments.Higher order derivatives also workmy_int_fun(Der{2}, 10, V, p)my_int_fun(Der{5}, 10, V, p)my_int_fun(Der{10}, 10, V, p)"
},

{
    "location": "examples.html#With-grouped-args-1",
    "page": "Examples",
    "title": "With grouped args",
    "category": "section",
    "text": "Finally we show one more example of how to use an associative mapping for the FunctionFactory args field to produce a function with grouped arguments.using Dolang  # hide\neqs = [\n    :(sin(x(0)) + exp(2*x(1))),\n    :(y(0) / (2 * (1 - β))),\n]\nargs = Dict(\n    :a => [(:x, 0), (:y, 0)],\n    :b => [(:x, 1)]\n)\nparams = [:β]\nff = FunctionFactory(eqs, args, params, funname=:grouped_args)\ncode = make_function(ff)\neval(code)Let's take a look at the methods for this functionmethods(grouped_args)Notice that each of theses routines have arguments for a, b, and p instead of just the V and p we saw in previous examples.When we evaluate these methods we need to be sure that a has two elements, b has one, and p has one:a = [π, 5.0]\nb = [0.8]\np = [0.98]\ngrouped_args(a, b, p)Notice we can evaluate a method where a and b are vectorized. Here p will be repeated as necessarygrouped_args([a a+1]', [b b+1]', p)We can also evaluate a partially vectorized version of the function where only a is a matrix and b is a vector. In this case both b and p will be repeatedgrouped_args([a a+1]', b, p)Non-allocating versions of the function are also availableout = zeros(2)\ngrouped_args!(out, a, b, p)\n@show @allocated grouped_args!(out, a, b, p)\n@show outAs of today (2017-06-13) only first order derivative code has been implemented for functions with grouped args. Trying to evaluate a derivative will result in an error that looks like this:grouped_args(Der{1}, a, b, p)Trying to evaluate any higher order derivative will result in an error that looks like this:julia> grouped_args(Der{2}, a, b, p)\nERROR: MethodError: no method matching equation_block(::Dolang.FunctionFactory{Dict{Symbol,Array{Tuple{Symbol,Int64},1}},Array{Symbol,1},Dict{Symbol,Any},DataType}, ::Type{Dolang.Der{2}})"
},

{
    "location": "dev/compiler.html#",
    "page": "Compiler internals",
    "title": "Compiler internals",
    "category": "page",
    "text": ""
},

{
    "location": "dev/compiler.html#Compiler-internals-1",
    "page": "Compiler internals",
    "title": "Compiler internals",
    "category": "section",
    "text": "NOTE: 2017-06-13: These docs might be slightly out of date. They should still serve as a helpful reference if you are totally lost when reading the code, but the best way to understand what is going on is to read the code in src/symbolic.jl and src/compiler.jlThese are some developer notes about the compiler inside Dolang.jlThe compiler operates through the FunctionFactory type. The fields of the type include the equations, symbols, and incidence tables for all equations."
},

{
    "location": "dev/compiler.html#Expression-Blocks-1",
    "page": "Compiler internals",
    "title": "Expression Blocks",
    "category": "section",
    "text": "Julia functions are generated by composing multiple blocks. Each of these blocks is associated with a function that can be overloaded to customize behavior.For a function that allocates memory for the output and returns the allocated array, we have the following blocks (in this order):allocate_block: Allocates memory to hold the output of the evaluatedequations. Memory is bound to a variable named outparam_block: Unpacks items from the params field\narg_block: Unpacks items from the arg field\nequation_block: uses the now locally defined variables from params andargs to evaluate the equationsreturn_block: Simply returns outFor a mutating function that populates a pre-allocated array with the value of the function at specified values for the args and params we have:sizecheck_block: Checks that the size of the out argument that waspassed into the function is conformable with the input args and parameters and the equations.param_block: Unpacks items from the params field\narg_block: Unpacks items from the arg field\nequation_block: uses the now locally defined variables from params andargs to evaluate the equationsreturn_block: Simply returns outIn both cases steps 2-5 are the same and are called the body_blockThe allocate_block, size_checkblock, and equation_block can all depend on the order of derivative to be computed. For that reason, the corresponding functions all have the signature func{n}(::FunctionFactory, ::TDer{n}). To implement the body of a function higher order derivatives, you only need to provide methods for these functions. Also, each of them has the second argument defaulting to Der{0}, so calling func(ff) will return the 0th order derivative (or level) version of that block."
},

{
    "location": "dev/compiler.html#Function-Signature-1",
    "page": "Compiler internals",
    "title": "Function Signature",
    "category": "section",
    "text": "In addition to the function blocks discussed above, we also need to know the signature of each function so it can be defined.The signature of the generated function for ff::FunctionFactory has the following structure:ff.funname([DERIVATIVE], [DISPATCH], arg_names(ff)..., param_names(ff)...)Let's take it once piece at a time:ff.funname is the provided function name\nDERIVATIVE has the form ::Type{Dolang.Der{N}}, where N is meant tospecify the order(s) of the derivative to be evaluated. This allows you to use the same function name, but control which order of derivative is evaluated by passing Der{N} as the first argument to ff.funname. If N == 0, this section of the signature is skipped.DISPATCH has the form ::Type{ff.dispatch} where ff.dispatch should bea Julia DataType. This is used to create many methods for same function (i.e. multiple versions of the function with the same name), but have them be distinguishable to the Julia compiler. See example usage to see how it works. By default ff.dispatch is set to Dolang.SkipArg. When ff.dispatch == SkipArg, the compiler completely skips the [DISPATCH] section of the signaturearg_names(ff)... is simply the name of the arguments from ff.args. Ifff.args is a Vector (more specifically a Dolang.FlatArgs), then this will be [:V]. If ff.args is some Associative structure, then this will be the keys of that structure.param_names(ff) is the same as arg_names(ff), but applied to theff.params fieldWe also need a signature for the mutating version of the signature. This has the structureff.funname!([DERIVATIVE], [DISPATCH], out, arg_names(ff)..., param_names(ff)...)Everything is the same as above, except that ff.funname! is now the original function name with ! appended to it and there is an additional out argument. This is the array that should be filled with the evaluated equations and always comes _after_ arguments that drive dispatch (DERIVATIVE and DISPATCH), but _before_ args and params."
},

{
    "location": "dev/compiler.html#Putting-it-together-1",
    "page": "Compiler internals",
    "title": "Putting it together",
    "category": "section",
    "text": "Once you have the signature and function body, putting them together is pretty simple.The build_function function will simply build Expr(:function, signature, body), using the signature and body routines from above.In a pun on the normal meaning of the ! suffix for Julia functions, build_function! will build a mutating version of the function following the rules outlined above."
},

]}
