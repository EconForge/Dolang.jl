"""
Maps from equation number to a Dict: variable -> Set(time_periods)
"""
immutable IncidenceTable
    by_eq::Dict{Int,Dict{Symbol,Set{Int}}}
    by_var::Dict{Symbol,Set{Int}}
    by_date::Dict{Int,Set{Symbol}}
end

IncidenceTable() = IncidenceTable(Dict(), Dict(), Dict())

function IncidenceTable(eqs::AbstractVector, skip::AbstractVector=Symbol[])
    # create incidence
    it = IncidenceTable()
    for (i, eq) in enumerate(eqs)
        visit!(it, eq, i, 0, skip)
    end
    it
end

function IncidenceTable(eq::Expr, skip::AbstractVector=Symbol[])
    it = IncidenceTable()
    visit!(it, eq, 1, 0, skip)
    it
end

Base.getindex(it::IncidenceTable, i::Int) = it.by_date[i]
Base.getindex(it::IncidenceTable, s::Symbol) = it.by_var[s]

function visit!(it::IncidenceTable, s::Symbol, n::Int, shift::Int,
                skip::AbstractVector=Symbol[])

    if s in skip
        return nothing
    end

    for_eq = get!(it.by_eq, n, Dict{Symbol,Set{Int}}())
    for_sym = get!(for_eq, s, Set{Int}())
    push!(for_sym, shift)

    # update by_var
    push!(get!(it.by_var, s, Set{Int}()), shift)

    # upate by_date
    push!(get!(it.by_date, shift, Set{Symbol}()), s)

    nothing
end

# don't worry about anything else
function visit!(it::IncidenceTable, s::Any, n::Int, shift::Int,
                skip::AbstractVector=Symbol[])
    nothing
end

function visit!(it::IncidenceTable, ex::Expr, n::Int, shift::Int,
                skip::AbstractVector=Symbol[])
    if is_time_shift(ex)
        var = ex.args[1]
        i = ex.args[2]
        visit!(it, var, n, i+shift, skip)
        return nothing
    end

    # don't visit the function name
    if ex.head == :call
        for an_arg in ex.args[2:end]
            visit!(it, an_arg, n, shift, skip)
        end
        return nothing
    end

    # Otherwise just visit everything
    for an_arg in ex.args
        visit!(it, an_arg, n, shift, skip)
    end
    nothing
end
