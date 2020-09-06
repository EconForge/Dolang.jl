@testset "util" begin
    x = [1, 2, 3]
    X = [1 0 0; 0 1 0; 0 0 1]

    @test Dolang._unpack_var(x, 1) == 1
    @test Dolang._unpack_var(x, 2) == 2
    @test Dolang._unpack_var(x, 3) == 3

    @test Dolang._unpack_var(X, 1) == [1, 0 ,0]
    @test Dolang._unpack_var(X, 2) == [0, 1 ,0]
    @test Dolang._unpack_var(X, 3) == [0, 0 ,1]

    Dolang._assign_var(x, 3, 1)
    Dolang._assign_var(x, 1, 2)
    Dolang._assign_var(x, 2, 3)
    @test x == [3, 1, 2]

    Dolang._assign_var(X, [0, 1 ,0], 3)
    Dolang._assign_var(X, [0, 0 ,1], 1)
    Dolang._assign_var(X, [1, 0 ,0], 2)
    @test X == [0 1 0; 0 0 1; 1 0 0]

    @test Dolang._output_size(4, x) == (4,)
    @test Dolang._output_size(4, x, x, x, x) == (4,)

    @test Dolang._output_size(4, X) == (3, 4)
    @test Dolang._output_size(4, X, x) == (3, 4)
    @test Dolang._output_size(4, x, X) == (3, 4)
    @test Dolang._output_size(4, X, x, X) == (3, 4)
    @test Dolang._output_size(4, x, X, x) == (3, 4)

    @test_throws DimensionMismatch Dolang._output_size(4, X, rand(4, 4))

    foo = Dolang._allocate_out(Int, 4, x)
    @test isa(foo, Vector{Int})
    @test size(foo) == (4,)

    foo = Dolang._allocate_out(Int, 4, x, x, x, x)
    @test isa(foo, Vector{Int})
    @test size(foo) == (4,)

    foo = Dolang._allocate_out(Int, 4, X)
    @test isa(foo, Matrix{Int})
    @test size(foo) == (3,4)

    foo = Dolang._allocate_out(Int, 4, X, x)
    @test isa(foo, Matrix{Int})
    @test size(foo) == (3,4)

    foo = Dolang._allocate_out(Int, 4, x, X)
    @test isa(foo, Matrix{Int})
    @test size(foo) == (3,4)

    foo = Dolang._allocate_out(Int, 4, x, X, x)
    @test isa(foo, Matrix{Int})
    @test size(foo) == (3,4)

    foo = Dolang._allocate_out(Int, 4, X, x, X)
    @test isa(foo, Matrix{Int})
    @test size(foo) == (3,4)

    @testset "_to_expr" begin
        @test Dolang._to_expr("foo") == Expr(:block, :foo)
        @test Dolang._to_expr(100) == Expr(:block, 100)
        @test Dolang._to_expr(:bar) == Expr(:block, :bar)
        @test Dolang._to_expr(:(x+y)) == :(x+y)
    end

    @testset "inf_to_Inf" begin
        x = rand()
        @test Dolang.inf_to_Inf(x) == x
        @test Dolang.inf_to_Inf(Inf) == Inf
        @test Dolang.inf_to_Inf(:inf) == Inf
        @test Dolang.inf_to_Inf(:(-inf)) == :(-$(Inf))
        @test Dolang.inf_to_Inf(:(x-inf)) == :(x-$(Inf))
    end

    @testset "solve_triangular_system" begin
        # very simple case
        d1 = Dict(:x => 1.0, :y => :(x+1))
        sol1 = OrderedDict{Symbol,Number}(:x => 1.0, :y => 2.0)
        @test Dolang.solve_triangular_system(d1) == sol1

        # fully specified numerical dict
        d2 = Dict(:x => 1.0, :y => 2.0)
        sol2 = OrderedDict{Symbol,Number}(:x => 1.0, :y => 2.0)
        @test Dolang.solve_triangular_system(d2) == sol2

        # unknown variable w
        d3 = Dict(:x => 1.0, :y => :(x+1), :z=> :(100*w))
        @test_throws ErrorException Dolang.solve_triangular_system(d3)

        # non-triangular system
        d4 = Dict(:x => :(y+1), :y => :(x-1))
        @test_throws ErrorException Dolang.solve_triangular_system(d4)
    end


end  # @testset "util"


@testset "triangular systems" begin

    @testset "solve_dependencies" begin
        system_ints = Dict{Int,Set{Int}}(
            1=>Set([2,3,4]),
            2=>Set([3,4]),
            3=>Set([4]),
            4=>Set([])
        )
        sol = Dolang.solve_dependencies(system_ints)
        @test sol==[4,3,2,1]

        system_syms = Dict{Any,Set{Any}}(
            :d=>Set([(:a,0),:b,:c]),
            :c=>Set([(:a,0),:b]),
            :b=>Set([(:a,0)]),
            (:a,0)=>Set([])
        )
        sol = Dolang.solve_dependencies(system_syms)
        @test sol==[(:a,0),:b,:c,:d]

        system_error = Dict{Any,Set{Any}}(
            :d=>Set([(:a,0),:b,:c]),
            :c=>Set([(:a,0),:b]),
            :b=>Set([(:a,0)]),
            (:a,0)=>Set([:d])
        )
        @test_throws Dolang.TriangularSystemException Dolang.solve_dependencies(system_error)
    end

    @testset "Reorder triangular block" begin
        eqs = Dict(
            (:k,0)=>:((1-delta)*k(-1)+i(-1)),
            (:y,0)=>:(A(0)*k(-1)^theta),
            (:A,0)=>:(rho*A(-1)+epsilon),
        )
        eqs_reordered = OrderedDict(
            (:A, 0) => :(rho * A(-1) + epsilon),
            (:y, 0) => :(A(0) * k(-1) ^ theta),
            (:k, 0) => :((1 - delta) * k(-1) + i(-1))
        )
        @test eqs_reordered == Dolang.reorder_triangular_block(eqs)
    end

    @testset "Solve definitions" begin

        defs = Dict(
            (:rho, 0) => :(c[t] / c[t-1]),
            (:V, 0)   => :(c[t] ^ (1 - gamma) / (1 - gamma)),
            (:c, 0)   => :(y[t] ^ theta - i[t]),
        )
        solved_defs = OrderedDict(
            (:V, 0)   => :(c[t] ^ (1 - gamma) / (1 - gamma)),
            (:c, 0)   => :(y[t] ^ theta - i[t]),
            (:c, -1)  => :(y[t-1] ^ theta - i[t-1]),
            (:rho, 0) => :(c[t] / c[t-1])
        )
        @test solved_defs == Dolang.solve_definitions(defs)

        solved_defs_reduced = OrderedDict(
            (:c, 1)   => :(y[t+1] ^ theta - i[t+1]),
            (:c, 0)  => :(y[t] ^ theta - i[t]),
            (:rho, 1) => :(c[t+1] / c[t])
        )

        @test solved_defs_reduced == Dolang.solve_definitions(defs, [(:rho,1)])

    end
end
