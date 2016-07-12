@testset "compiler" begin


    @testset "_unpack_expr" begin
        nms = [:a, :b, :c]
        have = Dolang._unpack_expr(nms, :V)
        @test have.head == :block
        @test have.args[1] == :(a_ = Dolang._unpack_var(V, 1))
        @test have.args[2] == :(b_ = Dolang._unpack_var(V, 2))
        @test have.args[3] == :(c_ = Dolang._unpack_var(V, 3))
    end
end
