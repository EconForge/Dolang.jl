import sympy
import json
import symengine
import time
from contextlib import contextmanager

from dolo.compiler.triangular_solver import solve_triangular_system
from dolo.compiler.function_compiler import StandardizeDatesSimple
from dolo.compiler.function_compiler_ast import to_expr
from dolo.compiler.function_compiler_ast import to_expr
from dolo.compiler.codegen import to_source


@contextmanager
def timeit(msg):
    t1 = time.time()
    yield
    t2 = time.time()
    print('{}: {:.4f} s'.format(msg, t2-t1))


# with timeit("Conversion mod -> json"):
    # from dolo.compiler.import_dynare import import_dynare
#     model = import_dynare("/home/pablo/Downloads/EA_QUEST3.mod", output_type='json')
#     txt = json.dumps(model)
#     model['calibration']
#     with open("EA_QUEST3.json",'w') as f:
#         json.dump(model,f)


with timeit('Load json'):
    with open("EA_QUEST3.json") as f:
        model = json.load(f)


with timeit("Solve triangular system"):
    calibration = model['calibration']
    sol = solve_triangular_system(calibration)


with timeit("Parse and normalize equations"):
    equations = model['equations']

    compat = lambda s: s.replace("^", "**").replace('==','=').replace('=','==')

    equations = [compat(eq) for eq in equations]
    equations = ['{} - ({})'.format(*str.split(eq,'==')) for eq in equations]
    equations = [to_expr(e) for e in equations]

    all_symbols = sum(model['symbols'].values(), [])
    sds = StandardizeDatesSimple(all_symbols)
    equations_normalized = [sds.visit(e) for e in equations]
    equations_normalized_strings = [to_source(e) for e in equations_normalized]
    symbols_normalized_strings = [to_source(sds.visit(to_expr(e))) for e in all_symbols]



with timeit("Sympify equations"):
    equations_normalized_sympy = [sympy.sympify(e) for e in equations_normalized_strings]


with timeit("Compute jacobian (sympy)"):
    jac = []
    for eq in equations_normalized_strings:
        line = []
        eqs = sympy.sympify(eq)
        atoms = [v for v in eqs.atoms() if str(v) in symbols_normalized_strings]
        for v in atoms:
            line.append(eqs.diff(v))
        jac.append(line)



with timeit("Translate sympy - > symengine"):
    atoms = [[symengine.sympify(v) for v in eq.atoms() if str(v) in symbols_normalized_strings] for eq in equations_normalized_sympy]
    eqs = [symengine.sympify(eq) for eq in equations_normalized_sympy]


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
