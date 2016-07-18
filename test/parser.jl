@testset "Dolang.call_expr" begin
    @test Dolang.call_expr(:x, 0) == :x_
    @test Dolang.call_expr(:x, 1) == :x__1_
    @test Dolang.call_expr(:x, -1) == :x_m1_
    @test Dolang.call_expr(:x, -100) == :x_m100_

    @test Dolang.call_expr("x", 0) == :x_
    @test Dolang.call_expr("x", 1) == :x__1_
    @test Dolang.call_expr("x", -1) == :x_m1_
    @test Dolang.call_expr("x", -100) == :x_m100_

    @test Dolang.call_expr((:x, 0)) == :x_
    @test Dolang.call_expr((:x, 1)) == :x__1_
    @test Dolang.call_expr((:x, -1)) == :x_m1_
    @test Dolang.call_expr((:x, -100)) == :x_m100_
end

@testset "Dolang.eq_expr" begin
    ex = :(z = x + y(1))
    @test Dolang.eq_expr(ex) == :(x_ .+ y__1_ .- z_)
    @test Dolang.eq_expr(ex, [:z]) == :(z_ = x_ .+ y__1_)
end

@testset "Dolang._parse" begin
    @testset "numbers" begin
        for T in (Float16, Float32, Float64, Int8, Int16, Int32, Int64)
            x = rand(T)
            @test Dolang._parse(x) == x
        end
    end

    @testset "symbols" begin
        for i=1:10
            s = gensym()
            @test Dolang._parse(s) == Symbol(string(s), "_")
        end
    end

    @testset "x_(shift_Integer)" begin
        for i=1:10, T in (Int8, Int16, Int32, Int64)
            @test Dolang._parse(string("x(", T(i), ")")) == Symbol("x__$(i)_")
            @test Dolang._parse(string("x(", T(-i), ")")) == Symbol("x_m$(i)_")
        end

        # only add underscore to naems when shift is 0
        @test Dolang._parse("x(0)") == :x_
    end

    @testset "other function calls" begin
        @testset "one argument" begin
            @test Dolang._parse("sin(x)") == :(sin(x_))
            @test Dolang._parse("sin(x(-1))") == :(sin(x_m1_))
            @test Dolang._parse("foobar(x(2))") == :(foobar(x__2_))
        end

        @testset "two arguments" begin
            @test Dolang._parse("dot(x, y(1))") == :(dot(x_, y__1_))
            @test Dolang._parse("plot(x(-1), y)") == :(plot(x_m1_, y_))
            @test Dolang._parse("bingbong(x(2), y)") == :(bingbong(x__2_, y_))
        end

        @testset "more args" begin
            for i=3:10
                ex = Expr(:call, :my_func, [:(x($j)) for j in 1:i]...)
                want = Expr(:call, :my_func, [Symbol("x__", j, "_") for j in 1:i]...)
                @test Dolang._parse(ex) == want
            end
        end

        @testset "arithmetic" begin
            @test Dolang._parse(:(a(1) + b + c(2) + d(-1))) == :(((a__1_ .+ b_) .+ c__2_) .+ d_m1_)
            @test Dolang._parse(:(a(1) * b * c(2) * d(-1))) == :(((a__1_ .* b_) .* c__2_) .* d_m1_)
            @test Dolang._parse(:(a(1) - b - c(2) - d(-1))) == :(((a__1_ .- b_) .- c__2_) .- d_m1_)
            @test Dolang._parse(:(a(1) ^ b)) == :(a__1_ .^ b_)
        end

        @testset "throws errors when unsupported" begin
            @test_throws ErrorException Dolang._parse("x+y || i <= 100")
        end
    end

    @testset "Expr(:(=), ...)" begin
        @testset "without targets" begin
            @test Dolang._parse(:(x = y)) == :(y_ .- x_)
        end

        @testset "with targets" begin
            @test Dolang._parse(:(x = log(y(-1))); targets=[:x]) == :(x_ = log(y_m1_))
            @test_throws ErrorException Dolang._parse(:(x = y); targets=[:y])
        end
    end

    @testset "Tuple{Symbol,Int}" begin
        @test Dolang._parse((:x, 0)) == :x_
        @test Dolang._parse((:x, 1)) == :x__1_
        @test Dolang._parse((:x, -1)) == :x_m1_
        @test Dolang._parse((:x, -100)) == :x_m100_
    end
end
