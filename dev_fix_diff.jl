using Dolo

model = Model("/home/pablo/.julia/dev/Dolo/examples/models/rbc.yaml")

model

fff = model.factories[:arbitrage]

fff

import OrderedCollections
import Dolang
using StaticArrays

ff2 = Dolang.FlatFunctionFactory(OrderedCollections.OrderedDict{Symbol,Union{Expr, Number, Symbol}}(:_out_1_ => :(_chi_ * _n__0_ ^ _eta_ * _c__0_ ^ _sigma_ - ((1 - _alpha_) * exp(_z__0_) * _k__0_ ^ _alpha_ * _n__0_ ^ (1 - _alpha_)) / _n__0_),:_out_2_ => :(1 - _beta_ * (_c__0_ / _c__1_) ^ _sigma_ * ((1 - _delta_) + _rk__1_))), OrderedCollections.OrderedDict(:m => [:_z__0_],:s => [:_k__0_],:x => [:_n__0_, :_i__0_],:M => [:_z__1_],:S => [:_k__1_],:X => [:_n__1_, :_i__1_],:p => [:_beta_, :_sigma_, :_eta_, :_chi_, :_delta_, :_alpha_, :_rho_, :_zbar_, :_sig_z_]), [:_out_1_, :_out_2_], OrderedCollections.OrderedDict{Symbol,Union{Expr, Number, Symbol}}(:_y__1_ => :(exp(_z__1_) * _k__1_ ^ _alpha_ * _n__1_ ^ (1 - _alpha_)),:_y__0_ => :(exp(_z__0_) * _k__0_ ^ _alpha_ * _n__0_ ^ (1 - _alpha_)),:_c__0_ => :(_y__0_ - _i__0_),:_rk__1_ => :((_alpha_ * _y__1_) / _k__1_),:_c__1_ => :(_y__1_ - _i__1_)), :arbitrage)

l = ([0.0], [9.354978290145986], [0.33, 0.23387445725364966], [0.99, 5.0, 1.0, 23.95785990938192, 0.025, 0.33, 0.8, 0.0, 0.016])
m, s, x, p = [SVector(e...) for e in l]

code = Dolang.gen_kernel(fff, [0,3])
arb = eval(code)


res, f_x = arb(m,s,x,m,s,x,p)

# true result
# array([[ 34.95244649, -13.27057815],
#         [-13.27057912,   6.56871655]]),
