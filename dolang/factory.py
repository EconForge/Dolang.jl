from typing import List, Dict, Tuple
from dataclasses import dataclass
import ast
from ast import NodeTransformer, Call
from dolang.codegen import to_source
import sympy as symlib

@dataclass
class FlatFunctionFactory:
    preamble: Dict[str, str]
    content: Dict[str, str]
    arguments: Dict[str, List[str]]
    funname: str


def stack_arguments(ff:FlatFunctionFactory, varname:str='v'):
    import operator
    import functools
    args_k = [*ff.arguments.keys()]
    args_v = [*ff.arguments.values()]
    arguments = dict()
    arguments[varname] = functools.reduce(operator.concat, args_v[:-1])
    arguments[args_k[-1]] = args_v[-1]
    fff = FlatFunctionFactory(ff.preamble, ff.content, arguments, ff.funname)
    return fff


def substitute_preamble(ff:FlatFunctionFactory):
    import dolang
    import copy
    pr = copy.copy(ff.preamble)
    for k in pr.keys():
        pr[k] = dolang.parse_string(pr[k]).value
    st = SubsTransformer(pr)
    dd = copy.copy(ff.content)
    for k in dd.keys():
        eq = dolang.parse_string(dd[k]).value
        dd[k] = to_source(st.visit(eq))
    return FlatFunctionFactory({}, dd, ff.arguments, ff.funname)


def get_symbolic_derivatives(fff:FlatFunctionFactory, max_order=1):

    eqs = [symlib.sympify(eq) for eq in fff.content.values()]
    varname = [*fff.arguments.keys()][0]

    svars = [symlib.sympify(v) for v in fff.arguments[varname]]

    derivatives_sym = dict()
    derivatives_sym[0] = dict(((i,),symlib.sympify(eq)) for i,eq in enumerate(fff.content.values()))

    incidences = dict()
    incidence = dict()
    for i,eq in enumerate(eqs):
        ats = eq.atoms()
        l = []
        for (j,at) in enumerate(svars):
            if at in ats:
                l.append((j,at))
        incidence[(i,)] = l

    incidences[0] = incidence

    for order in range(1,max_order+1):
        deriv = dict()
        incs = dict()
        deriv__ = derivatives_sym[order-1]
        incs__ = incidences[order-1]
        for eq_d,eq in deriv__.items():
            syms = incs__[eq_d]
            n = eq_d[0]
            v = eq_d[1:]
            if len(v)==0:
                m = -1
            else:
                m = v[-1] # max index
            for k,s in syms:
                if k>=m:
                    deq = eq.diff(s)
                    ind = eq_d + (k,)
                    deriv[ind] = deq
                    # ats = deq.atoms()
                    incs[ind] = [e for e in syms if e[1] in deq.atoms()]
        derivatives_sym[order] = deriv
        incidences[order] = incs

    return derivatives_sym
    # return derivatives, incidences
