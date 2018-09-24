import sympy
import json
import symengine
import time
from contextlib import contextmanager

from dolang.triangular_solver import solve_triangular_system
from dolang.symbolic import parse_string
from dolang.symbolic import stringify, stringify_variable, stringify_parameter
from dolang.codegen import to_source


@contextmanager
def timeit(msg):
    t1 = time.time()
    yield
    t2 = time.time()
    print('{}: {:.4f} s'.format(msg, t2-t1))

#
# with timeit("Conversion mod -> json"):
#     from dolo.compiler.import_dynare import import_dynare
#     model = import_dynare("/home/pablo/Downloads/EA_QUEST3.mod", output_type='json')
#     txt = json.dumps(model)
#     model['calibration']
#     with open("EA_QUEST3.json",'w') as f:
#         json.dump(model,f)


with timeit('Load json'):
    with open("EA_QUEST3.json") as f:
        model = json.load(f)


model['symbols'].keys()
with timeit("Solve triangular system"):
    calibration = model['calibration']
    sol = solve_triangular_system(calibration)


with timeit("Parse equations"):
    equations = model['equations']

    compat = lambda s: s.replace("^", "**").replace('==','=').replace('=','==')

    equations = [compat(eq) for eq in equations]
    equations = ['{} - ({})'.format(*str.split(eq,'==')) for eq in equations]
    equations = [parse_string(e) for e in equations]


with timeit("stringify equations"):

    all_variables = [(v, 1) for v in model['symbols']['variables']] + \
                    [(v, 0) for v in model['symbols']['variables']]  + \
                    [(v, -1) for v in model['symbols']['variables']]  + \
                    [(v, 0) for v in model['symbols']['shocks']]
    all_vnames = [e[0] for e in all_variables]
    all_constants =  model['symbols']['parameters']

    # here comes the costly step
    equations_stringified = [stringify(e, variables=all_vnames) for e in equations]
    equations_stringified_strings = [to_source(e) for e in equations_stringified]
    variables_stringified_strings = [stringify_variable(e) for e in all_variables]


stringify_variable( all_variables[-3] )
equations_stringified_strings[-1]

with timeit("Sympify equations"):
    equations_stringified_sympy = [sympy.sympify(e) for e in equations_stringified_strings]


with timeit("Compute jacobian (sympy)"):
    jac = []
    for eq in equations_stringified_strings:
        line = []
        eqs = sympy.sympify(eq)
        atoms = [v for v in eqs.atoms() if str(v) in variables_stringified_strings]
        for v in atoms:
            line.append(eqs.diff(v))
        jac.append(line)


with timeit("Translate sympy - > symengine"):
    atoms = [[symengine.sympify(v) for v in eq.atoms() if str(v) in variables_stringified_strings] for eq in equations_stringified_sympy]
    eqs = [symengine.sympify(eq) for eq in equations_stringified_sympy]


with timeit("Compute Jacobian and Hessian (symengine)"):
    jac = []
    hes = []
    for i, eq in enumerate(eqs):
        line = []
        hesline = []
        # print(atoms)
        ats = atoms[i]
        for i, v in enumerate(ats):
            deq = eq.diff(v)
            line.append(deq)
            hesline2 = []
            for j, v in enumerate(ats):
                if i <= j:
                    hesline2.append(deq.diff(v))
            hesline.append(hesline2)
        jac.append(line)
        hes.append(hesline)






from dolo import *
model = yaml_import("/home/pablo/Programming/econforge/dolo/examples/models/rbc_dynare.yaml")

from dolo.algos.dynare.perturbations import solve_decision_rule

dr = solve_decision_rule(model)

dr.keys()
dr['g_a']
