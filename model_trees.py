from play_with_trees import read_equations

import copy

from pattern import ReplaceExpectation

from lib import LinearExpression
from dolo.compiler.codegen import to_source
import time
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

from treesolve import get_ts
def determinstic_simul(model, sol):
    import numpy
    variables = set([v.value.id for v in model.variables_ast])
    series = [ get_ts(model.etree, sol, v) for v in variables ]
    import pandas
    df = pandas.DataFrame( numpy.array(series).T, columns=variables)
    return df


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
        for n,eq in enumerate(tsf_equations):
            for s in etree.nodes:
            # eval_ast( model.conditions_ast
                context['s'] = s
                context['t'] = len(s)-1
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


        self.full_variables = full_variables
        self.full_equations = full_equations
        self.full_complementarities = full_complementarities
        # return full_equations

        for p in range(P):
            eq = full_equations[p]
            res[p] = -eq.c[1]
            if full_complementarities[p]:
                lb[p] = 0
            else:
                lb[p] = -100000
            for k in eq.c.keys():
                if k != 1:
                    q = full_variables.index(k)
                    jac[p,q] = eq.c[k]


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
    beta = 0.8/(0.8+0.15),
    a = 0.8,
    c = 0.15,
    estar = 0.0,
    Rbar = 0.1,
    min_f = 0,
    kappa = 1.0,
    R = 0.00123
)
# t1 = time.time()
# #


if __name__ == '__main__':

    from play_with_trees import source
    lines = str.split(source, '\n')


    t0 = time.time()

    N = 20
    etree = DeterministicTree(N)
    for s in etree.nodes:
        etree.values[s] = 0.1

    t1 = time.time()
    model = Model(etree, lines, calibration)
    t2 = time.time()
    res, jac, lb = model.compute_jacobian()
    t3 = time.time()
    sol = model.solve(verbose=False,jacs=[res,jac,lb])
    t4 = time.time()
    sim = determinstic_simul(model, sol)
    t5 = time.time()
    print("Total : {}".format(t5-t0))
    print('- construct the tree: {}'.format(t1-t0))
    print('- construct the model: {}'.format(t2-t1))
    print('- evaluate jacobien: {}'.format(t3-t2))
    print('- find solution: {}'.format(t4-t3))
    print('- simulate: {}'.format(t5-t4))
