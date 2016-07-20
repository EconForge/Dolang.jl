@testset "symbolic" begin

@testset "Dolang.normalize(::Union{Symbol,String}, Integer)" begin
    @test Dolang.normalize(:x, 0) == :_x_
    @test Dolang.normalize(:x, 1) == :_x__1_
    @test Dolang.normalize(:x, -1) == :_x_m1_
    @test Dolang.normalize(:x, -100) == :_x_m100_

    @test Dolang.normalize("x", 0) == :_x_
    @test Dolang.normalize("x", 1) == :_x__1_
    @test Dolang.normalize("x", -1) == :_x_m1_
    @test Dolang.normalize("x", -100) == :_x_m100_

    @test Dolang.normalize((:x, 0)) == :_x_
    @test Dolang.normalize((:x, 1)) == :_x__1_
    @test Dolang.normalize((:x, -1)) == :_x_m1_
    @test Dolang.normalize((:x, -100)) == :_x_m100_
end

@testset "Dolang.eq_expr" begin
    ex = :(z = x + y(1))
    @test Dolang.eq_expr(ex) == :(_x_ .+ _y__1_ .- _z_)
    @test Dolang.eq_expr(ex, [:z]) == :(_z_ = _x_ .+ _y__1_)
end

@testset "Dolang.normalize" begin
    @testset "numbers" begin
        for T in (Float16, Float32, Float64, Int8, Int16, Int32, Int64)
            x = rand(T)
            @test Dolang.normalize(x) == x
        end
    end

    @testset "symbols" begin
        for i=1:10
            s = gensym()
            @test Dolang.normalize(s) == Symbol("_", string(s), "_")
        end
    end

    @testset "x_(shift_Integer)" begin
        for i=1:10, T in (Int8, Int16, Int32, Int64)
            @test Dolang.normalize(string("x(", T(i), ")")) == Symbol("_x__$(i)_")
            @test Dolang.normalize(string("x(", T(-i), ")")) == Symbol("_x_m$(i)_")
        end

        # only add underscore to naems when shift is 0
        @test Dolang.normalize("x(0)") == :_x_
    end

    @testset "other function calls" begin
        @testset "one argument" begin
            @test Dolang.normalize("sin(x)") == :(sin(_x_))
            @test Dolang.normalize("sin(x(-1))") == :(sin(_x_m1_))
            @test Dolang.normalize("foobar(x(2))") == :(foobar(_x__2_))
        end

        @testset "two arguments" begin
            @test Dolang.normalize("dot(x, y(1))") == :(dot(_x_, _y__1_))
            @test Dolang.normalize("plot(x(-1), y)") == :(plot(_x_m1_, _y_))
            @test Dolang.normalize("bingbong(x(2), y)") == :(bingbong(_x__2_, _y_))
        end

        @testset "more args" begin
            for i=3:10
                ex = Expr(:call, :my_func, [:(x($j)) for j in 1:i]...)
                want = Expr(:call, :my_func, [Symbol("_x__", j, "_") for j in 1:i]...)
                @test Dolang.normalize(ex) == want
            end
        end

        @testset "arithmetic" begin
            @test Dolang.normalize(:(a(1) + b + c(2) + d(-1))) == :(((_a__1_ .+ _b_) .+ _c__2_) .+ _d_m1_)
            @test Dolang.normalize(:(a(1) * b * c(2) * d(-1))) == :(((_a__1_ .* _b_) .* _c__2_) .* _d_m1_)
            @test Dolang.normalize(:(a(1) - b - c(2) - d(-1))) == :(((_a__1_ .- _b_) .- _c__2_) .- _d_m1_)
            @test Dolang.normalize(:(a(1) ^ b)) == :(_a__1_ .^ _b_)
        end

        @testset "throws errors when unsupported" begin
            @test_throws Dolang.NormalizeError Dolang.normalize("x+y || i <= 100")
        end
    end

    @testset "Expr(:(=), ...)" begin
        @testset "without targets" begin
            @test Dolang.normalize(:(x = y)) == :(_y_ .- _x_)
        end

        @testset "with targets" begin
            @test Dolang.normalize(:(x = log(y(-1))); targets=[:x]) == :(_x_ = log(_y_m1_))
            @test_throws Dolang.NormalizeError Dolang.normalize(:(x = y); targets=[:y])
        end
    end

    @testset "normalize(::Tuple{Symbol,Int})" begin
        @test Dolang.normalize((:x, 0)) == :_x_
        @test Dolang.normalize((:x, 1)) == :_x__1_
        @test Dolang.normalize((:x, -1)) == :_x_m1_
        @test Dolang.normalize((:x, -100)) == :_x_m100_
    end
end

end  # @testset "symbolic"
