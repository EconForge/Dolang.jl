from play_with_trees import read_equations

import copy

from pattern import ReplaceExpectation

from lib import LinearExpression
from dolo.compiler.codegen import to_source
import time
from play_with_trees import lines
from treesolve import *

import time


def eval_ast(expr, d):
    import ast

    # a = ast.Expr(value=expr)
    a = ast.Expression(body=expr)
    a =     ast.fix_missing_locations(a)
    cc = compile(a, '<string>', 'eval')
    res = eval(cc, d)
    return res

class LIVar:
    def __init__(self, name, etree):
        self.name = name
        self.etree = etree

    def __getitem__(self, x):
        import sympy
        # return sympy.Symbol(self.name+'_'+str.join('', map(str,x)))
        ind = self.etree.nodes.index(x)
        name = '{}_{}'.format(self.name,ind)
        return LinearExpression(name=name)

class Model:
    __jacobian__ = None

    def __init__(self, etree, lines, calibration):

        [conditions, equations, variables, complementarities] = read_equations(lines)
        self.conditions_ast = conditions
        self.equations_ast = equations
        self.variables_ast = variables
        self.complementarities = complementarities
        self.calibration = calibration
        self.etree = etree
        pass


    def compute_jacobian(self):

        model = self
        etree = model.etree

        conditions_fun = [(eval("lambda s: " + l) if l is not None else None) for l in model.conditions_ast]

        indexed_variables_str = set( [va.value.id for va in model.variables_ast] )
        indexed_variables = [LIVar(vs, etree) for vs in indexed_variables_str]

        context = {}
        for iv in indexed_variables:
            context[iv.name]  =  iv
        for c,v in calibration.items():
            context[c] = v

        # for s in etree.nodes:
        #     print(cond(s))
        import copy

        from pattern import ReplaceExpectation

        context['s'] = etree.nodes[0]
        context['etree'] = etree

        context['S'] = lambda x: [(etree.probas[s, x], x) for x in etree.children(s)]
        context['P'] = lambda x: etree.parent(x)
        context['H'] = lambda x: etree.history(x)
        context['Ex'] = lambda fun, ss: sum([p*fun(x) for p,x in ss])
        context['Sum'] = lambda fun, ss: sum([fun(x) for x in ss])



        tsf_equations = [ReplaceExpectation().visit( copy.copy(ee) ) for ee in model.equations_ast]
        # tsf_equations = [ReplaceSum().visit( copy.copy(ee) ) for ee in tsf_equations]

        full_equations = []
        full_variables = []
        full_complementarities = []
        for s in etree.nodes:
            context['s'] = s
            context['t'] = len(s)
            # eval_ast( model.conditions_ast
            for n,eq in enumerate(tsf_equations):
                if (conditions_fun[n] is None) or conditions_fun[n](s):
                    var = eval_ast( model.variables_ast[n], context )
                    full_variables.append(var.name)
                    # print("Converting {}".format(to_source(tsf_equations[n])))
                    eq_ast = tsf_equations[n]
                    eq = eval_ast(eq_ast, context)
                    full_equations.append(  eq )
                    full_complementarities.append( model.complementarities[n] )


        P = len(full_equations)
        import numpy
        res = numpy.zeros(P)
        jac = numpy.zeros((P,P))
        lb = numpy.zeros(P)

        for p in range(P):
            eq = full_equations[p]
            res[p] = eq.c[1]
            if full_complementarities[p]:
                lb[p] = 0
            else:
                lb[p] = -100000
            for k in eq.c.keys():
                if k != 1:
                    q = full_variables.index(k)
                    jac[p,q] = -eq.c[k]


        self.full_variables = full_variables
        self.full_equations = full_equations
        self.full_complementarities = full_complementarities

        return res, jac, lb


    def solve(self, verbose=False, jacs=None):

        model = self
        import numpy

        if jacs is None:
            constants,mat,lb = self.compute_jacobian()
        else:
            constants,mat,lb = jacs
        # print("Jacobian")
        # print(mat)
        # print("Residuals")
        # print(constants)

        mm = numpy.array(mat).astype(dtype=float)
        v = numpy.array(constants).astype(dtype=float).flatten()
        lb = numpy.array(lb).astype(dtype=float).flatten()

        lb[lb<-1000] = -numpy.inf
        x0 = v*0 + 0.1
        ub = numpy.zeros_like(lb)+numpy.inf

        def fun(x):
            res = v-numpy.dot(mm, x)
            return res
        #
        def dfun(x):
            return -mm


        from dolo.numeric.extern.lmmcp import lmmcp
        res = lmmcp(fun, dfun, x0, lb, ub, verbose=verbose, options={'preprocess': False})

        from collections import OrderedDict
        return OrderedDict(zip(map(str,model.full_variables), res))

calibration = dict(
    beta = 0.96,
    a = 1.5,
    c = 0.1,
    estar = -0.5,
    Rbar = 0.1,
    min_f = 0
)
t1 = time.time()



t0 = time.time()
N = 20
etree = DeterministicTree(N)



t1 = time.time()
model = Model(etree, lines, calibration)
t2 = time.time()

res, jac, lb = model.compute_jacobian()
t3 = time.time()
print(t1-t0,t2-t1, t3-t2)


model.solve(verbose=True,jacs=[res,jac,lb])

jac.shape
#






# etree = model.etree
#
# conditions_fun = [(eval("lambda s: " + l) if l is not None else None) for l in model.conditions_ast]
#
# indexed_variables_str = set( [va.value.id for va in model.variables_ast] )
# indexed_variables = [LIVar(vs, etree) for vs in indexed_variables_str]
#
# context = {}
# for iv in indexed_variables:
#     context[iv.name]  =  iv
# for c,v in calibration.items():
#     context[c] = v
#
# # for s in etree.nodes:
# #     print(cond(s))
#
# context['s'] = etree.nodes[0]
# context['etree'] = etree
#
# context['S'] = lambda x: [(etree.probas[s, x], x) for x in etree.children(s)]
# context['P'] = lambda x: etree.parent(x)
# context['H'] = lambda x: etree.history(x)
# context['Ex'] = lambda fun, ss: sum([p*fun(x) for p,x in ss])
# context['Sum'] = lambda fun, ss: sum([fun(x) for x in ss])
#
#
#
# tsf_equations = [ReplaceExpectation().visit( copy.copy(ee) ) for ee in model.equations_ast]
# # tsf_equations = [ReplaceSum().visit( copy.copy(ee) ) for ee in tsf_equations]
#
# full_equations = []
# full_variables = []
# for s in etree.nodes:
#     context['s'] = s
#     context['t'] = len(s)
#     # eval_ast( model.conditions_ast
#     for n,eq in enumerate(tsf_equations):
#         if (conditions_fun[n] is None) or conditions_fun[n](s):
#             var = eval_ast( model.variables_ast[n], context )
#             full_variables.append(var.name)
#             # print("Converting {}".format(to_source(tsf_equations[n])))
#             eq_ast = tsf_equations[n]
#             eq = eval_ast(eq_ast, context)
#             full_equations.append(  eq )
#             # print('{} :: {}'.format(var, eq.c.keys()))
#
# P = len(full_equations)
# import numpy
# res = numpy.zeros(P)
# jac = numpy.zeros((P,P))
# lb = numpy.zeros(P)
#
# for p in range(P):
#     eq = full_equations[p]
#     res[p] = eq.c[1]
#     for k in eq.c.keys():
#         if k != 1:
#             q = full_variables.index(k)
#             jac[p,q] = eq.c[k]
#
# t2 = time.time()
#
#
#
#
#
#
# print("Elapsed : {}".format(t2-t1))
#
#
#
# eq.c[1]
# all_variables = [v.name for v in all_variables]
# all_variables
#
# full_variables
# all_variables
#
# for e in model.equations_ast:
#     print( to_source( e ) )
#
# for i in range(len(model.complementarities)):
#     print(model.complementarities[i])
#
# ast.parse('0<=m').body[0].value
#
#
#
# print(ast.dump( ast.parse('E[ _expr | _x in _set]').body[0].value ) + "\n\nfsa")
#
#
#
#
# s = etree.nodes[-2]
# context['s'] = s
# context['s']
# eval_ast(eqq, context)
#
#
# ast.dump(ast.parse("a + 34", mode='eval'))
#
#
#
#
# print( eqs+'\n\n'+'ji' )
# eval( eqs, context)
#
# to_source( model.equations_ast[1] )
#
# eq1 = eval( to_source(model.equations_ast[0]), context )
#
#
#
# def test():
#     t = 1
#     h = lambda x: t + 1
#     return h
#
# t = 2
#
# test()(1)
