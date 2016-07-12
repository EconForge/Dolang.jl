@testset "factory" begin

const flat_args = [(:a, 0), (:b, 1), (:c, -1)]
const grouped_args = Dict(:x=>[(:a, 0), (:c, -1)], :y=>[(:b, 1)])
const flat_params = [:beta, :delta]
const grouped_params = Dict(:p => [:beta, :delta])

@testset "convert Grouped" begin
    @test sort(Dolang.FlatArgs(grouped_args), by=_->_[1]) == flat_args
    @test sort(Dolang.FlatParams(grouped_params)) == flat_params
end

@testset "allowed_dates" begin
    out = OrderedDict()
    Dolang.allowed_dates!(flat_args, out)
    @test out == OrderedDict(:a=>Set([0]), :b=>Set([1]), :c=>Set([-1]))

    @test out == Dolang.allowed_dates(flat_args)
    @test out == Dolang.allowed_dates(grouped_args)
end

@testset "param_names" begin
    @test Dolang.param_names(flat_params) == flat_params
    @test Dolang.param_names(grouped_params) == flat_params
end

@testset "is_time_shift" begin
    @test Dolang.is_time_shift(:(sin(1)))
    @test Dolang.is_time_shift(:(x(2)))
    @test !Dolang.is_time_shift(:(a(b)))
    @test !Dolang.is_time_shift(:(a(1.0)))
    @test Dolang.is_time_shift(:(a(1)))
end

@testset "time_shift" begin
    #TODO: write tests with defs
    _ts = Dolang.time_shift
    defs = Dict()
    args = Symbol[_[1] for _ in flat_args]
    ex1 = :(a + x)
    ex2 = :(a(2) + x + b)

    @test _ts(:a, args, defs, 0) == :a_
    @test _ts(:a, args, defs, 1) == :a__1_
    @test _ts(:a, args, defs, -10) == :a_m10_

    # no shift, this gets parsed, hence the `.+`. Same below
    @test _ts(ex1, args, defs, 0) == :(a_ .+ x_)
    @test _ts(ex1, args, defs, 1) == :(a__1_ + x_)
    @test _ts(ex1, args, defs, -10) == :(a_m10_ + x_)

    @test _ts(ex2, args, defs, 0) == :(a__2_ .+ x_ .+ b_)
    @test _ts(ex2, args, defs, 1) == :(a__3_ + x_ + b__1_)
    @test _ts(ex2, args, defs, -10) == :(a_m8_ + x_ + b_m10_)
end

@testset "subs" begin
    d = Dict(:monty=> :python, :run=>:faster, :eat=>:more)
    @test Dolang.subs(:monty, d) == :python
    @test Dolang.subs(:Monty, d) == :Monty
    @test Dolang.subs(1.0, d) == 1.0

    want = :(python(faster + more, eats))
    @test Dolang.subs(:(monty(run + eat, eats)), d) == want
end

@testset "IncidenceTable" begin
    ex1 = :(foo = bing + bong)
    ex2 = :(cowboy = yee - haw! + foo)
    ex3 = :(x = foo(-3) + bing(1))
    exs = [ex1, ex2, ex3]

    # visiting symbols
    it = Dolang.IncidenceTable()
    Dolang.visit!(it, :x, 1, 0)
    want_t = OrderedDict(1=>Dict(:x=>Set([0])))
    @test want_t == it.t
    @test Dict(:x=>Set([0])) == it.by_var

    Dolang.visit!(it, :x, 1, 1)
    want_t[1][:x] = Set([0, 1])
    @test want_t == it.t
    @test Dict(:x=>Set([0, 1])) == it.by_var

    Dolang.visit!(it, :x, 2, 3)
    want_t[1][:x] = Set([0, 1])
    want_t[2] = Dict(:x=>Set([3]))
    @test want_t == it.t
    @test Dict(:x=>Set([0, 1, 3])) == it.by_var

    Dolang.visit!(it, :y, 1, -1)
    want_t[1][:y] = Set([-1])
    @test want_t == it.t
    @test Dict(:x=>Set([0, 1, 3]), :y=>Set([-1])) == it.by_var

    # visiting numbers shouldn't change anything
    for i in 1:100
        Dolang.visit!(it, i, 1, 1)
    end
    @test want_t == it.t
    @test Dict(:x=>Set([0, 1, 3]), :y=>Set([-1])) == it.by_var

    # reset `it` before working on expressions
    it = Dolang.IncidenceTable()
    want_t = deepcopy(it.t)
    want_bv = deepcopy(it.by_var)

    s0 = Set([0])
    Dolang.visit!(it, ex1, 1, 0)
    want_t[1] = Dict(:foo=>s0, :bing=>s0, :bong=>s0)
    want_bv = deepcopy(want_t[1])
    @test want_t == it.t
    @test want_bv == it.by_var

    Dolang.visit!(it, ex2, 2, 0)
    want_t[2] = Dict(:cowboy=>s0, :yee=>s0, :haw! =>s0, :foo=>s0)
    # want_bv[:cowboy] =s0; want_bv[:yee] = s0; want_bv[:haw!] =s0
    merge!(want_bv, want_t[2])
    @test want_t == it.t
    @test want_bv == it.by_var

    Dolang.visit!(it, ex3, 3, 0)
    want_t[3] = Dict(:x=>s0, :foo=>Set([-3]), :bing=>Set([1]))
    want_bv[:x] = s0
    want_bv[:foo] = Set([-3, 0])
    want_bv[:bing] = Set([0, 1])
    @test want_t == it.t
    @test want_bv == it.by_var

    Dolang.visit!(it, ex3, 4, 1)
    want_t[4] = Dict(:x=>Set([1]), :foo=>Set([-2]), :bing=>Set([2]))
    want_bv[:x] = Set([0, 1])
    want_bv[:foo] = Set([-3, -2, 0])
    want_bv[:bing] = Set([0, 1, 2])
    @test want_t == it.t
    @test want_bv == it.by_var

    # using expressions
    it1 = Dolang.IncidenceTable(ex1)
    want = OrderedDict(1=>Dict(:foo=>Set([0]), :bing=>Set([0]),
                               :bong=>Set([0])))
    @test it1.t == want

    want = Dict(:foo=>Set([0]), :bing=>Set([0]), :bong=>Set([0]))
    @test it1.by_var == want

    # getindex
    @test it[1] == it.t[1]
    @test it[:foo] == it.by_var[:foo]
end

@testset "Function Factory" begin
    # TODO: write these tests
end

end  # @testset "factory"
