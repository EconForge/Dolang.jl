@testset "symbolic" begin

@testset "Dolang.eq_expr" begin
    ex1 = :(z(0) = x + y(1))
    ex2 = :(z(0) == x + y(1))

    for ex in (ex1, ex2)
        @test Dolang.normalize(ex) == :(_x_ + _y__1_ - _z__0_)
        @test Dolang.normalize(ex, targets=[:_z__0_]) == :(_z__0_ = _x_ + _y__1_)
        @test Dolang.normalize(ex, targets=[(:z, 0)]) == :(_z__0_ = _x_ + _y__1_)
    end

end

@testset "Dolang.normalize" begin
    @testset "Dolang.normalize(::Union{Symbol,String}, Integer)" begin
        @test Dolang.normalize(:x, 0) == :_x__0_
        @test Dolang.normalize(:x, 1) == :_x__1_
        @test Dolang.normalize(:x, -1) == :_x_m1_
        @test Dolang.normalize(:x, -100) == :_x_m100_

        @test Dolang.normalize("x", 0) == :_x__0_
        @test Dolang.normalize("x", 1) == :_x__1_
        @test Dolang.normalize("x", -1) == :_x_m1_
        @test Dolang.normalize("x", -100) == :_x_m100_

        @test Dolang.normalize((:x, 0)) == :_x__0_
        @test Dolang.normalize((:x, 1)) == :_x__1_
        @test Dolang.normalize((:x, -1)) == :_x_m1_
        @test Dolang.normalize((:x, -100)) == :_x_m100_
    end

    @testset "numbers" begin
        for T in (Float16, Float32, Float64, Int8, Int16, Int32, Int64)
            x = rand(T)
            @test Dolang.normalize(x) == x
        end
    end

    @testset "symbols" begin
        for i=1:10
            s = gensym()
            want = Symbol("_", s, "_")
            @test Dolang.normalize(s) == want
            @test Dolang.normalize(want) == want
        end
    end

    @testset "x_(shift_Integer)" begin
        for i=1:10, T in (Int8, Int16, Int32, Int64)
            @test Dolang.normalize(string("x(", T(i), ")")) == Symbol("_x__$(i)_")
            @test Dolang.normalize(string("x(", T(-i), ")")) == Symbol("_x_m$(i)_")
        end
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
            @test Dolang.normalize(:(a(1) + b + c(2) + d(-1))) == :(_a__1_ + _b_ + _c__2_ + _d_m1_)
            @test Dolang.normalize(:(a(1) * b * c(2) * d(-1))) == :(_a__1_ * _b_ * _c__2_ * _d_m1_)
            @test Dolang.normalize(:(a(1) - b - c(2) - d(-1))) == :(((_a__1_ - _b_) - _c__2_) - _d_m1_)
            @test Dolang.normalize(:(a(1) ^ b)) == :(_a__1_ ^ _b_)
        end

        @testset "throws errors when unsupported" begin
            @test_throws Dolang.NormalizeError Dolang.normalize("x+y || i <= 100")
        end
    end

    @testset "Expr(:(=), ...)" begin
        @testset "without targets" begin
            @test Dolang.normalize(:(x = y)) == :(_y_ - _x_)
        end

        @testset "with targets" begin
            @test Dolang.normalize(:(x = log(y(-1))); targets=[:x]) == :(_x_ = log(_y_m1_))
            @test Dolang.normalize(:(x == log(y(-1))); targets=[:x]) == :(_x_ = log(_y_m1_))
            @test_throws Dolang.NormalizeError Dolang.normalize(:(x = y); targets=[:y])
        end
    end

    @testset "normalize(::Tuple{Symbol,Int})" begin
        @test Dolang.normalize((:x, 0)) == :_x__0_
        @test Dolang.normalize((:x, 1)) == :_x__1_
        @test Dolang.normalize((:x, -1)) == :_x_m1_
        @test Dolang.normalize((:x, -100)) == :_x_m100_
    end

    @testset "Custom normalizer" begin
        function agent_time_normalizer(ex::Expr)
            # matches var[inds](time)
            if (ex.head == :call &&  # function matches ending `()`
                length(ex.args) == 2 &&   # only one arg inside `()`
                isa(ex.args[1], Expr) &&  # part before `()` is an expr
                ex.args[1].head == :ref &&  # part before `()` is a `[]`
                isa(ex.args[1].args[2], Union{Symbol, Number}) &&  # part in `[]` is scalar
                isa(ex.args[2], Union{Symbol, Number})  # part in `()` is scalar
                )
                # have structure we want, just need to unpack it
                var = ex.args[1].args[1]
                ind = ex.args[1].args[2]
                shift = ex.args[2]
                return Nullable(
                    Symbol("_$(var)__$(ind)", shift >= 0 ? "__" : "_m", abs(shift), "_")
                    )
            else
                return Nullable{Symbol}()
            end
        end

        ex = :(a(1) + x[i](1) / b)
        want = :(_a__1_ + _x__i__1_ / _b_)
        @test_throws Dolang.NormalizeError Dolang.normalize(ex)
        try
            Dolang.normalize(ex)
        catch e
            @test isa(e, Dolang.NormalizeError)
            @test e.ex == :(x[i](1))
        end
        @test Dolang.normalize(ex, custom=agent_time_normalizer) == want

        # test with target
        ex = :(z(1) = a(1) + x[i](1) / b)
        want = :(_a__1_ + _x__i__1_ / _b_ - _z__1_)
        @test Dolang.normalize(ex, custom=agent_time_normalizer) == want
    end
end

@testset "Dolang.time_shift" begin
    defs = Dict(:a=>:(b(-1)/c))
    defs_sanitized = Dict(:a=>:(b(-1)/c(0)))
    funcs = [:foobar]
    for shift in [-1, 0, 1]
        have = Dolang.time_shift(:(a+b(1) + c), shift)
        @test have == :(a+b($(shift+1)) + c)

        have = Dolang.time_shift(:(a+b(1) + c(0)), shift)
        @test have == :(a+b($(shift+1)) + c($(shift)))


        # unknown function
        @test_throws Dolang.UnknownFunctionError Dolang.time_shift(:(a+b(1) + foobar(c)), shift)

        # with functions
        have = Dolang.time_shift(:(a+b(1) + foobar(c)), shift, functions=funcs)
        @test have == :(a+b($(shift+1)) + foobar(c))
    end
end

@testset "Dolang.steady_state" begin
    @test Dolang.steady_state(:(a+b(1) + c)) == :(a+b+c)

    # unknown function
    @test_throws Dolang.UnknownFunctionError Dolang.steady_state(:(a+b(1)+c+foobar(c)))

    # now let function be ok
    want = Dolang.steady_state(:(a+b(1) + foobar(c)), functions=[:foobar])
    @test want == :(a+b+foobar(c))
end

@testset "Dolang.list_symbols" begin
    ex = :(a+b(1)+c)
    out = Dolang.list_symbols(ex)
    @test haskey(out, :variables)
    @test out[:variables] == Set([(:b, 1)])
    @test haskey(out, :parameters)
    @test out[:parameters] == Set{Symbol}([:a, :c])
    @test out[:variables] == @inferred Dolang.list_variables(ex)
    @test out[:parameters] == @inferred Dolang.list_parameters(ex)

    ex = :(a+b(1)+c(0))
    out = Dolang.list_symbols(ex)
    @test haskey(out, :variables)
    @test out[:variables] == Set([(:b, 1), (:c, 0)])
    @test haskey(out, :parameters)
    @test out[:parameters] == Set{Symbol}([:a])
    @test out[:variables] == @inferred Dolang.list_variables(ex)
    @test out[:parameters] == @inferred Dolang.list_parameters(ex)

    # Unknown function
    @test_throws Dolang.UnknownFunctionError Dolang.list_symbols(:(a+b(1)+c+foobar(c)))

    # now let the function be ok
    ex = :(a+b(1)+c + foobar(c))
    out = Dolang.list_symbols(ex, functions=[:foobar])
    @test haskey(out, :variables)
    @test out[:variables] == Set([(:b, 1)])
    @test haskey(out, :parameters)
    @test out[:parameters] == Set{Symbol}([:a, :c])
    @test out[:variables] == @inferred Dolang.list_variables(ex, functions=[:foobar])
    @test out[:parameters] == @inferred Dolang.list_parameters(ex, functions=[:foobar])

end

@testset "arg_name, arg_time, arg_name_time, arg_names" begin
    for i in 1:5
        s = gensym()
        s_vec = [gensym() for xxxxxx in 1:3]
        s_dict = OrderedDict(:m => s_vec, :M => [s])
        @test Dolang.arg_name(s) == s
        @test Dolang.arg_time(s) == 0
        @test Dolang.arg_name_time(s) == (s, 0)
        @test Dolang.arg_names(s_vec) == s_vec
        @test Dolang.arg_names(s_dict) == vcat(s_vec, s)
        for t in 1:10
            @test Dolang.arg_name((s, t)) == s
            @test Dolang.arg_time((s, t)) == t
            @test Dolang.arg_name_time((s, t)) == (s, t)
            @test Dolang.arg_name(Expr(:call, s, t)) == s
            @test Dolang.arg_time(Expr(:call, s, t)) == t
            @test Dolang.arg_name_time(Expr(:call, s, t)) == (s, t)
        end
    end

    for f in (Dolang.arg_name, Dolang.arg_time, Dolang.arg_name_time, Dolang.arg_names)
        @test_throws MethodError f(1)        # Number
        @test_throws MethodError f([1])      # Array of number
    end

    @test Dolang.arg_name_time(:_x__1_) == (:x, 1)
    @test Dolang.arg_name_time(:_x_m100_) == (:x, -100)
    @test Dolang.arg_name_time(:_this_is_x_m100_) == (:this_is_x, -100)
end

end  # @testset "symbolic"
