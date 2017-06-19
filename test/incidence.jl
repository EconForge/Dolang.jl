@testset " IncidenceTable" begin
    ex1 = :(foo = bing + bong)
    ex2 = :(cowboy = yee - haw! + foo)
    ex3 = :(x = foo(-3) + bing(1))
    exs = [ex1, ex2, ex3]

    # visiting symbols
    it = Dolang.IncidenceTable()
    Dolang.visit!(it, :x, 1, 0)
    want_eq = OrderedDict(1=>Dict(:x=>Set(0)))
    @test want_eq == it.by_eq
    @test Dict(:x=>Set(0)) == it.by_var

    Dolang.visit!(it, :x, 1, 1)
    want_eq[1][:x] = Set([0, 1])
    @test want_eq == it.by_eq
    @test Dict(:x=>Set([0, 1])) == it.by_var

    Dolang.visit!(it, :x, 2, 3)
    want_eq[1][:x] = Set([0, 1])
    want_eq[2] = Dict(:x=>Set(3))
    @test want_eq == it.by_eq
    @test Dict(:x=>Set([0, 1, 3])) == it.by_var

    Dolang.visit!(it, :y, 1, -1)
    want_eq[1][:y] = Set([-1])
    @test want_eq == it.by_eq
    @test Dict(:x=>Set([0, 1, 3]), :y=>Set([-1])) == it.by_var

    # visiting numbers shouldn't change anything
    for i in 1:100
        Dolang.visit!(it, i, 1, 1)
    end
    @test want_eq == it.by_eq
    @test Dict(:x=>Set([0, 1, 3]), :y=>Set([-1])) == it.by_var

    # reset `it` before working on expressions
    it = Dolang.IncidenceTable()
    want_eq = deepcopy(it.by_eq)
    want_bv = deepcopy(it.by_var)

    s0 = Set(0)
    Dolang.visit!(it, ex1, 1, 0)
    want_eq[1] = Dict(:foo=>s0, :bing=>s0, :bong=>s0)
    want_bv = deepcopy(want_eq[1])
    @test want_eq == it.by_eq
    @test want_bv == it.by_var

    Dolang.visit!(it, ex2, 2, 0)
    want_eq[2] = Dict(:cowboy=>s0, :yee=>s0, :haw! =>s0, :foo=>s0)
    # want_bv[:cowboy] =s0; want_bv[:yee] = s0; want_bv[:haw!] =s0
    merge!(want_bv, want_eq[2])
    @test want_eq == it.by_eq
    @test want_bv == it.by_var

    Dolang.visit!(it, ex3, 3, 0)
    want_eq[3] = Dict(:x=>s0, :foo=>Set([-3]), :bing=>Set(1))
    want_bv[:x] = s0
    want_bv[:foo] = Set([-3, 0])
    want_bv[:bing] = Set([0, 1])
    @test want_eq == it.by_eq
    @test want_bv == it.by_var

    Dolang.visit!(it, ex3, 4, 1)
    want_eq[4] = Dict(:x=>Set(1), :foo=>Set([-2]), :bing=>Set(2))
    want_bv[:x] = Set([0, 1])
    want_bv[:foo] = Set([-3, -2, 0])
    want_bv[:bing] = Set([0, 1, 2])
    @test want_eq == it.by_eq
    @test want_bv == it.by_var

    # using expressions
    it1 = Dolang.IncidenceTable(ex1)
    want = OrderedDict(1=>Dict(:foo=>Set(0), :bing=>Set(0),
                               :bong=>Set(0)))
    @test it1.by_eq == want

    want = Dict(:foo=>Set(0), :bing=>Set(0), :bong=>Set(0))
    @test it1.by_var == want

    # getindex
    @test it[1] == it.by_date[1]
    @test it[:foo] == it.by_var[:foo]
end

@testset "by_eq always created -- even for empty eqs" begin
    # ff is controls_lb for rbc_dtcc_iid.yaml
    args = DataStructures.OrderedDict(:m=>Tuple{Symbol,Int64}[(:e_z,
    0)],:s=>Tuple{Symbol,Int64}[(:z, 0), (:k, 0)])

    params = Symbol[:beta, :sigma, :eta, :chi, :delta, :alpha, :rho, :zbar, :sig_z]
    eqs = Expr[quote 0.0 end, quote 0.0 end]

    defs = DataStructures.OrderedDict(:w=>:(((1 - alpha) * y(0)) /
    n(0)),:rk=>:((alpha * y(0)) / k(0)),:y=>:(exp(z(0)) * k(0) ^ alpha * n(0) ^
    (1 - alpha)),:c=>:(y(0) - i(0)))

    ff = FunctionFactory(eqs, args, params, defs=defs)

    @test length(ff.incidence.by_eq) == 2
    @test haskey(ff.incidence.by_eq, 1)
    @test haskey(ff.incidence.by_eq, 2)
    @test length(ff.incidence.by_eq[1]) == 0
    @test length(ff.incidence.by_eq[2]) == 0
end
