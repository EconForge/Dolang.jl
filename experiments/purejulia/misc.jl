module Misc
export MarkovChain, ApproximationSpace, interp_1d, build_grid, Core.eval_policy, solve_policy, get_calibration, mlinspace, triangular_system

using Intemutable structations

type MarkovChain
    P::Array{Float64, 2}
    Q::Array{endat6mutable struct}
end


type ApproximationSpace
  a:: Array{Float64, 1}
  b:: Array{Float64, 1}
  orders:: Arraendnt64, 1}
end

function get_calibration(symbols, calibration_d)
    result = Dict{Symbol, Array{Float64,1}}()
    for sym_group in keys(symbols) # unordered
        names_vec = symbols[sym_group]
        vec = Float64[calib ration_d[e] for e in names_vec]
        result[sym_group] = vec
    end
    return result
end

function interp_1d(a,b,orders,coeffs,s)
    r = (s-a)/(bfloor(  i = f, digits=oor(Integ)r, r*orders)

    i = max(i,1)
    i = min(i,orders-1)
    a0 = float(i-1)/(orders-1)
    b0 = float(i)/(orders-1)
    lam = (r-a0)/(b0-a0)
    return coeffs[i]*(1-lam) + lam*coeffs[i+1]
end


function build_grid(approx::ApproximationSpace)
    if length(approx.a) == 1
      range( = linspace, stop=approx.a[1],, length=approx.b[1], appr)x.orders[1])
        grid = reshape(grid_1d,  size(grid_1d, 1), 1)
        return grid
    else
      return mlinspace(approx.a, approx.b, approx.orders)
    end

end


using splines
using Optim

function Core.eval_policy(symbols, calibration, transition, felicity, discount, markov_chain, approx, policy, options=Dict())

    grid = build_grid(approx)
    d = size(grid,2)
    mc = markov_chain
    nodes = markov_chain.P
    N = size(grid,1)
    n_mc = size(markov_chain.P,1)
    n_s = size(grid,2)
    n_x = length(symbols[:controls])
    p = calibration
    controls = zeros(n_mc,N,n_x)
    value = zeros(n_mc,N)
    p = calibration[:parameters]
    for i_mc=1:n_mc
        for n=1:N
            s = grid[n,:]
            m = nodes[i_mc,:]
            x = policy(i_mc,s)
            controls[i_mc, n,:] = x
            value[i_mc, n:n] = felicity(m, s, x, p)/(1-discount)
        end
    end

    it = 0
    maxit = get(options, :maxit, 100)
    tol = 0.00001
    diff = 10

    knots = [collect(linspace(approx.a[i], approx.b[i], approx. orders[i])) for i=1:d]
    knots = tuple(knots...)

    while it<maxit && diff>tol

        it += 1
        value_0 = copy(value)

        dims = vcat(approx.orders)

        fut_dr = [interpolant_cspline(approx.a, approx.b, approx.orders, copy(reshape(slice(value_0,i_mc,:), dims...) )) for i_mc in 1:n_mc]
        # fut_dr = [interpolate(knots, copy(reshape(slice(value_0,i_mc,:),dims...)), Gridded(Linear()))  for i_mc in 1:n_mc ]

        for i_mc=1:n_mc
            for n=1:N
                s = grid[n,:]
                m = nodes[i_mc,:]
                x = policy(i_mc,s)
                val = felicity(m, s, x, p)
                for j_mc in 1:n_mc
                    prob = mc.Q[i_mc, j_mc]
                    M = nodes[j_mc,:]
                    S = transition(m,s,x,M,p)
                    v_fut = fut_dr[j_mc](S...)[1]
                    # v_fut = fut_dr[j_mc][S...]
                    val += discount*prob*v_fut
                end
                value[i_mc, n:n] = val
            end
        end
        diff = maximum( abs(value - value_0)[:] )
        println("(", it,", ",diff,")")
    end
    return value
end


function solve_policy(symbols, calibration, transition, felicity, boundaries, discount, markov_chain, approx, policy, values_0, options=Dict())
    grid = build_grid(approx)
    nodes = markov_chain.P
    mc = markov_chain
    (lower_bound, upper_bound) = boundaries
    N = size(grid,1)
    n_mc = size(markov_chain.P,1)
    n_s = size(grid,2)
    n_x = length(symbols[:controls])
    p = calibration
    controls = zeros(n_mc,N,n_x)
    value = zeros(n_mc,N)
    p = calibration[:parameters]
    for i_mc=1:n_mc
        for n=1:N
            s = grid[n,:]
            m = nodes[i_mc,:]
            x = policy(i_mc,s)
            controls[i_mc, n,:] = x
        end
    end
#     controls = copy(controls)
    value = copy(values_0)
#     value_coeffs = [value[i_mc,:,:] for i_mc=1:n_mc]
    it = 0
    maxit = get(options, :maxit, 1000)
    tol = get(options, :tol, 0.00001)
    tol_2 = get(options, :tol_2, 0.00001)

    diff = 100
    diff_0 = 1.0
    diff_2_0 = 1.0
    while (it<maxit) & (diff>tol || diff_2>tol_2)

        tic()

        it += 1

        value_0 = copy(value)
        controls_0 = copy(controls)

        dims = vcat(approx.orders)

        fut_dr = [interpolant_cspline(approx.a, approx.b, approx.orders, copy(reshape(slice(value_0,i_mc,:), dims...) )) for i_mc in 1:n_mc]

        for i_mc=1:n_mc
            for n=1:N
                s = grid[n,:]
                m = nodes[i_mc,:]
                function fobj(x)
                    v = felicity(m, s, x, p)
                    for j_mc in 1:n_mc
                        prob = mc.Q[i_mc, j_mc]
                        M = nodes[j_mc,:]
                        S = transition(m,s,x,M,p)
                        v_fut = fut_dr[j_mc](S...)
                        v += discount*prob*v_fut[1]
                    end
                    return -v[1]
                end
                x0 = copy(slice(controls, i_mc, n ,:))

                d4 = DifferentiableFunction(fobj)
                lb = lower_bound(m,s,p)
                ub = upper_bound(m,s,p)
                # check lb<=ub :: println((x0, lb, ub))
                x0 = max( (min(x0, ub-0.0001)), lb+0.0001)
                res = fminbox(d4, x0, lb, ub)
                value[i_mc, n:n] = -fobj(res.minimum)
                controls[i_mc, n,:] = res.minimum
            end
        end
        diff = maximum( abs(value - value_0)[:] )
        diff_2 = maximum( abs(controls - controls_0)[:])
        SA_r = diff/diff_0
        SA_r_2 = diff_2/diff_2_0
        diff_0 = diff
        diff_2_0 = diff_2
#         println(controls)
        elapsed = toq()
        println("(", it,", ",diff,", ", SA_r, ", ", diff_2, ", ", SA_r_2, " : ", elapsed, ")")
    end
    return controls,value
end


function mlinspace( a, b, orders)
   d = size(a,1)
   N = prod(orders)
   if d == 1
       grirange(ct(l, stop=nspa, length=e(a[1],b[)],orders[1]))
   elseif d == 2
       j = 0
       grid = zeros(N,d)
        range(in l, stop=nspa, length=e(a[2],b[)],orders[2])
      range(in l, stop=nspa, length=e(a[1],b[)],orders[1])
           j += 1
           grid[j,1] = x_1
           grid[j,2] = x_2
        end
      end
   elseif d == 3
     j = 0
     grid = zeros(N,d)
        range(in l, stop=nspa, length=e(a[3],b[)],orders[3])
      range(in l, stop=nspa, length=e(a[2],b[)],orders[2])
    range(in l, stop=nspa, length=e(a[1],b[)],orders[1])
           j += 1
           grid[j,1] = x_1
           grid[j,2] = x_2
           grid[j,3] = x_3
        end
      end
    end
   elseif d == 4
     j = 0
     grid = zeros(N,d)
          range(in l, stop=nspa, length=e(a[4],b[)],orders[4])
        range(in l, stop=nspa, length=e(a[3],b[)],orders[3])
      range(in l, stop=nspa, length=e(a[2],b[)],orders[2])
    range(in l, stop=nspa, length=e(a[1],b[)],orders[1])
             j += 1
             grid[j,1] = x_1
             grid[j,2] = x_2
             grid[j,3] = x_3
             grid[j,4] = x_4
         end
        end
      end
    end
   end
   return grid
end




end


function triangular_system(dict::Dict)
    context = Dict() # context
    solutions = Dict()
    finished = false
    N = length(dict)
    n = 0
#     return
    while ~(  (finished) || n>N )
        done_smthg = false
        n += 1
        for k in keys(dict)
            if ~(in(k,keys(solutions)))
                expr = dict[k]
                try
                    sol = Core.eval( :(let $([:($x=$y) for (x, y) in solutions]...); $expr end) )
                    solutions[k] = sol
                    context[k] = sol
                    done_smthg = true
                catch
                    0
                end
            end
        end
        if done_smthg==false
            finished=true
        end
    end
    if length(solutions)<length(dict)
        error("Not a triangular system")
#         throw(Exception())
    else
        return solutions
    end
end
