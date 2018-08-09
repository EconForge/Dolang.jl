using Dolang

immutable RBC end
rbc_ff = let
    # construct args
    variables = [:y, :c, :k, :i, :l, :y_l, :z]
    exogenous = [:e]
    args = [(:y, 0), (:c, 0), (:c, 1), (:k, -1), (:k, 0), (:i, 0), (:l, 0),
            (:l, 1), (:y_l, 0), (:z, -1), (:z, 0), (:z, 1), (:e, 0)]

    # params
    params = [:beta, :psi, :delta, :alpha, :rho]

    # no definitions
    # no targets

    # equations
    eqs = [
        :((1/c) = beta*(1/c(+1))*(1+alpha*(k^(alpha-1))*(exp(z(+1))*l(+1))^(1-alpha)-delta)),
        :(psi*c/(1-l) = (1-alpha)*(k(-1)^alpha)*(exp(z)^(1-alpha))*(l^(-alpha))),
        :(c+i = y),
        :(y = (k(-1)^alpha)*(exp(z)*l)^(1-alpha)),
        :(i = k-(1-delta)*k(-1)),
        :(y_l = y/l),
        :(z = rho*z(-1)+e)
    ]

    map(Dolang._filter_lines!, eqs)

    Dolang.FunctionFactory(RBC, eqs, args, params, funname=:rbc_model)
end;

print_eval(ex) = (println(ex); eval(ex))
print_eval(make_method(rbc_ff))
print_eval(make_method(Der{1}, rbc_ff))
print_eval(make_method(Der{2}, rbc_ff, mutating=false))

# just some reasonable parameter values
alp = 0.33
bet = 0.99
gam = 0.003
mst = 1.011
rho = 0.7
psi = 0.787
del = 0.02
k = 6
P = 2.25
c = 0.45
e = 1
W = 4
R = 1.02
d = 0.85
n = 0.19
l = 0.86
y = 0.6
i = y-c
e = 0
z = e/(1-rho)
V = [y, c, c, k, k, i, l, l, y/l, z, z, z, e]
p = [bet, psi, del, alp, rho]
model = RBC()
levels = rbc_model(model, V, p)
jac = rbc_model(Der{1}, model, V, p)
hes = rbc_model(Der{2}, model, V, p)
