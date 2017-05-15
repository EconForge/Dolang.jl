@testset "latex printer" begin

    tests = [
        ("a(1)", "a_{t+1}",),
        ("astar(1)", "a_{t+1}^{\\star}",),
        ("a_i_j__k(1)", "a_{i,j,t+1}^{k}",),
        ("a__k_i_j(1)", "a_{i,j,t+1}^{k}",),
        ("abar_i_jstar__k(1)", "\\overline{a}_{i,j^{\\star},t+1}^{k}"),
        ("a__p1_p2(1)", "a_{p2,t+1}^{p1}")
    ]

    for (have, want) in tests
        @test Dolang.latex(have) == want
    end

end  # testset
