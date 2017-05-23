@testset "factory" begin

const flat_args = [(:a, 0), (:b, 1), (:c, -1)]
const grouped_args = Dict(:x=>[(:a, 0), (:c, -1)], :y=>[(:b, 1)])
const flat_params = [:beta, :delta]
const grouped_params = Dict(:p => [:beta, :delta])

@testset " convert Grouped" begin
    @test sort(Dolang._to_flat(grouped_args), by=_junk->_junk[1]) == flat_args
    @test sort(Dolang._to_flat(grouped_params)) == flat_params
end

@testset " is_time_shift" begin
    @test Dolang.is_time_shift(:(sin(1)))
    @test Dolang.is_time_shift(:(x(2)))
    @test !Dolang.is_time_shift(:(a(b)))
    @test !Dolang.is_time_shift(:(a(1.0)))
    @test Dolang.is_time_shift(:(a(1)))
end

@testset " Function Factory" begin
    # TODO: test grouped argument style

    eqs = [:(foo(0) = log(a(0))+b(0)/x(-1)), :(bar(0) = c(1)+u*d(1))]
    args = [(:a, -1), (:a, 0), (:b, 0), (:c, 0), (:c, 1), (:d, 1)]
    args_grouped = Dict(
        :m => [(:a, -1), (:a, 0), (:b, 0), (:c, 0)],
        :M => [(:c, 1), (:d, 1)]
    )
    params = [:u]
    defs = Dict(:x=>:(a(0)/(1-c(1))))
    targets = [(:foo, 0), (:bar, 0)]
    funname = :myfun
    _FF = Dolang.FunctionFactory

    @testset "  constructors" begin
        # inner constructor directly
        ff1 = _FF{typeof(args),
                  typeof(params),
                  Dict{Symbol,Expr},
                  DataType}(eqs, args, params, targets, defs, funname,
                            Dolang.SkipArg)

        # First outer constructor
        ff2 = _FF(eqs, args, params, targets, defs, funname, Dolang.SkipArg)

        # kwarg outer constructor -- SkipArg default
        ff3 = _FF(eqs, args, params, targets=targets, defs=defs,
                  funname=funname)

        ff4 = _FF(Dolang.SkipArg, eqs, args, params, targets=targets,
                  defs=defs, funname=funname)

        @test ff2 == ff1
        @test ff3 == ff1
        @test ff4 == ff1

        # test with grouped args
        # inner constructor directly
        ffg1 = _FF{typeof(args_grouped),
                  typeof(params),
                  Dict{Symbol,Expr},
                  DataType}(eqs, args_grouped, params, targets, defs, funname,
                            Dolang.SkipArg)

        # First outer constructor
        ffg2 = _FF(eqs, args_grouped, params, targets, defs, funname, Dolang.SkipArg)

        # kwarg outer constructor -- SkipArg default
        ffg3 = _FF(eqs, args_grouped, params, targets=targets, defs=defs,
                  funname=funname)

        ffg4 = _FF(Dolang.SkipArg, eqs, args_grouped, params, targets=targets,
                  defs=defs, funname=funname)

        @test ffg2 == ffg1
        @test ffg3 == ffg1
        @test ffg4 == ffg1

    end

    @testset "  constructor behavior" begin
        _FF = _FF
        ff = _FF(eqs, args, params, targets=targets, defs=defs, funname=funname)

        # test that equations were normalized properly
        norm_eq1 = :(_foo__0_ = log(_a__0_) + _b__0_ / (_a_m1_ / (1 - _c__0_)))
        norm_eq2 = :(_bar__0_ = _c__1_ + _u_ * _d__1_)
        norm_eq = [norm_eq1, norm_eq2]
        @test ff.eqs == norm_eq

        # test that exceptions are thrown for unknown variables appearing
        # in the equations
        bad_eqs = vcat(eqs, :(whoami = a-b))::Vector{Expr}
        @test_throws(Dolang.NormalizeError,
                     _FF(bad_eqs, args, params, targets=targets, defs=defs,
                         funname=funname))

        # check content of exception
        ex = try
            _FF(bad_eqs, args, params, targets=targets, targets=targets,
                defs=defs)
           catch e
               e
           end

        @test ex.ex == :(whoami = a - b)

    end

    @testset " issue #18 (expand definitions in ff.incidence.by_eq)" begin
        ff = _FF(eqs, args, params, targets=targets, defs=defs, funname=funname)

        @test haskey(ff.incidence.by_eq[1], :a)
        @test haskey(ff.incidence.by_eq[1], :c)
        @test ff.incidence.by_eq[1][:a] == Set([-1, 0])
        @test ff.incidence.by_eq[1][:c] == Set([0])

    end

end

end  # @testset "factory"
