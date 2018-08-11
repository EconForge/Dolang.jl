function subvar(sym, shift::Int)
    if shift == 0
        return sym
    elseif shift >= 1
        return Symbol(string(sym, "__f_", shift))
    else
        return Symbol(string(sym, "__b_", -shift))
    end
end

function walktree(tree, subvar, table)
#     if typeof(tree)==Symbol
#         return tree
    if typeof(tree)==Expr
        if (tree.head == :call) && (length(tree.args)==2)
            sym = tree.args[1]
            shift = tree.args[2]
            if (sym,shift) in keys(table)
                return subvar(sym,shift)
            else
                return Expr(tree.head, [walktree(e, subvar, table) for e in tree.args]...)
            end
        else
            return Expr(tree.head, [walktree(e, subvar, table) for e in tree.args]...)
        end
    else
        return tree
    end
end


function construct_table(symbols, api)
    d = Dict()
    for (symbol_group,shift) in api
        for ind in 1:length(symbols[symbol_group])
            variable = symbols[symbol_group][ind]
            if shift == 0
                t_symbol_group = symbol_group
            elseif shift == 1
                t_symbol_group = symbol(string(symbol_group,"_f"))
            elseif shift == -1
                t_symbol_group = symbol(string(symbol_group,"_b"))
            end
            d[(variable, shift)] = (t_symbol_group, ind)
        end
    end
    return d
end

function rewrite_function(funcode, symbols, api)

    new_args = Any[]
    for e in api
        if e[2] == 1
            push!(new_args, Symbol(string(e[1],:_f)))
        elseif e[2] == -1
            push!(new_args, Symbol(string(e[1],:_b)))
        else
            push!(new_args, e[1])
        end
    end

    # new_args = [e[1] for e in api]
    if length(funcode.args[1].args)>=2
        # println("Inserting")
        splice!(funcode.args[1].args,2:1, new_args)
    else
        append!(funcode.args[1].args,new_args)
    end

    funbody = funcode.args[2]
    table = construct_table(symbols, api)

    # # substitute_variable(expr) = walktree(expr, )

    for i = 1:length(funbody.args)
        funbody.args[i] = walktree(funbody.args[i], subvar, table)
    end

    new_definitions = Expr[]
    for ((symbol,tshift),(t_symbol_group,ind)) in construct_table(symbols, api)
        new_sym = (subvar(symbol, tshift))  # e__f_1
        new_ins = :($new_sym = $t_symbol_group[$ind])
        push!(new_definitions, new_ins)
    end

    prepend!(funbody.args, new_definitions)

    return funcode

end


macro quickdef(symbols, api, content)
    funcode = copy((content).args[2])
    new_funcode = rewrite_function(funcode, Core.eval(symbols), Core.eval(api))
    # if debug
        # println(new_funcode)
    # end
    Core.eval(new_funcode)
end

symbols = Dict(
    :states => [:a, :b],
    :controls => [:x, :y],
    :shocks => [:e],
    :parameters => [:p1]
)


recipes = Dict(
    :transition => [
        (:markov_states, -1),
        (:states, -1),
        (:controls, -1),
        (:markov_states, 0),
        (:parameters, 0),
#         (:states, 1),
    ],
    :lower_bound => [
        (:markov_states, 0),
        (:states, 0),
        (:parameters, 0),
    ],
    :upper_bound => [
        (:markov_states, -1),
        (:states, 0),
        (:parameters, 0),
    ],
    :arbitrage => [
        (:markov_states,0),
        (:states,0),
        (:controls, 0),
        (:markov_states,1),
        (:states,1),
        (:controls, 1),
        (:parameters, 0),
    ],
    :auxiliary => [
        (:markov_states,0),
        (:states,0),
        (:controls, 0),
        (:parameters, 0),
    ],
    :felicity => [
        (:markov_states, 0),
        (:states, 0),
        (:controls, 0),
        (:parameters, 0)
    ]
)



#api = [
#    (:states, -1),
#    (:controls, -1),
#    (:shocks, 0),
#    (:parameters, 0),
#    (:states, 1),
#]

#@quickdef symbols api begin
#function transition()
#        a(1) = a(-1) + b(-1)
#         return out
#end
#end
