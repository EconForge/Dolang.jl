@testset "util" begin
    x = [1, 2, 3]
    X = eye(3)

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



end  # @testset "util"
