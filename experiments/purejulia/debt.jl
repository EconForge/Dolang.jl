include("quickdef.jl")
include("misc.jl")

using Misc


symbols = Dict(
    :markov_states => [:x, :y],
    :states => [:b],
    :controls => [:db],
    :auxiliaries => [:c],
    :parameters => [:β, :σ, :rf, :rr, :bmin]
)


@quickdef symbols recipes[:transition] begin
    function transition()
        out = zeros(1)
        out[1] = db(-1)# b
        return out
    end
end

@quickdef symbols recipes[:arbitrage] begin
    function arbitrage()
        out = zeros(1)
        c = y + b*rf - db
        c_f = y(1) + b(1)*rf - db(1)
        out[1] = β*(c_f/c)^(-σ)*rf - 1
        return out
    end
end


@quickdef symbols recipes[:lower_bound] begin
    function lower_bound()
        out = zeros(1)
        out[1] = -1 # c
        return out
    end
end

@quickdef symbols recipes[:upper_bound] begin
    function upper_bound()
        out = zeros(1)
        out[1] = 0 # c
        return out
    end
end

@quickdef symbols recipes[:auxiliary] begin
    function auxiliary()
        out = zeros(1)
        out[1] = y + b*rf - db # c
        return out
    end
end

@quickdef symbols recipes[:felicity] begin
    function felicity()
        out = zeros(1)
        c = y + b*rf - db # c
        out[1] = c^(1-σ)/(1-σ)
        return out
    end
end

boundaries=(lower_bound, upper_bound)

pi = 0.1
mc = MarkovChain(
    [ [0.0 1.0]
      [0.0 0.95] ],
    [ [1-pi pi]
      [0.5  0.5] ]
)
#

calibration_d = Dict(
    :β =>  0.96,
    :rf =>  (1/0.96 - 0.001),
    :rr =>  (1/0.96 - 0.011),
    :σ=> 4.0,
    :pi =>  0.1,
    :bmin => -0.5,
    :y => 1.0,
    :c => 1.0,
    :b => 0.0,
    :db => 0.0,
    :r => 0.0,
    :dr => 0.0,
    :x => 0.0,
)



calib = get_calibration(symbols, calibration_d)

# test function

s = calib[:states]
x = calib[:controls]
p = calib[:parameters]
m = calib[:markov_states]

transition(m,s,x,m,p) - s
arbitrage(m,s,x,m,s,x,p)


approx = ApproximationSpace([-1.0], [0.0], [20])
grid = build_grid(approx)

s = grid
init_dr(i,s) = s
x = init_dr(1,s)


val = Core.eval_policy(symbols, calib, transition, felicity, 0.96, mc, approx, init_dr)

(controls_2, val_2) = solve_policy(symbols, calib, transition, felicity, boundaries, 0.96, mc, approx, init_dr, val)



# Make plots

# using Gadfly
# function plot_me(x,y)
#     l1 = layer(x=x,y=y,Geom.point,Geom.smooth)
#
#     plot(l1)
# end
# l1_1 = layer(x=grid[:,1], y=controls_2[1,:], Geom.line, Theme(default_color=color("blue")) )
# l1_2 = layer(x=grid[:,1], y=controls_2[2,:], Geom.line, Theme(default_color=color("red")) )
# l1_bis = layer(x=grid[:,1], y=grid[:,1], Geom.line, Theme(default_color=color("black")) )
# l2 = layer(x=grid[:,1], y=val_2[1,:], Geom.line)
# display(plot([l1_1, l1_2, l1_bis], Guide.title("Controls")))
# display(plot(l2, Guide.title("Value function")))
