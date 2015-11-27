## Build the equations

from treesolve import *


class TreeModel:

    __jacobian__ = None
    __eval_jacobian__ = None

    def compute_symbolic_jacobian(self):

        import time
        t1 = time.time()
        if self.__jacobian__ is not None:
            return self.__jacobian__

        import sympy
        param_values = {sympy.Symbol(str(k)):v for k,v in self.param_values.items()}
        equations, variables = self.equations, self.variables

        import pickle
        with open('objects','wb') as f:
            pickle.dump( (param_values, equations, variables), f)

        import sympy
        zz = {v: 0 for v in variables}

        n = len(variables)

        import time
        t1 = time.time()

        mat = sympy.Matrix(n,n,lambda i,j: equations[i].expand().coeff(variables[j])).subs(param_values)
        constants = sympy.zeros(n,1)
        #
        # import numpy
        # constants = numpy.zeros(n, dtype=float)
        #
        # mat = numpy.zeros((n,n), dtype=float)
        for i in range(n):
            eq = equations[i].expand()
            syms = [a for a in eq.atoms() if a in variables]
            zz = {s:0 for s in syms}
            for s in syms:
                j = variables.index(s)
                mat[i,j] = eq.coeff(s) #.subs(param_values)
            constants[i] = -eq.subs(zz) #.subs(param_values)
        # t2 = time.time()
        # # print("Constructed the matrix in {} seconds".format(t2-t1))
        # #
        # # for eq in equations:
        # #     zz = {a: 0 for a in eq.atoms() if a in variables}
        # #     constants.append(-eq.subs(zz).subs(param_values))
        # # constants = sympy.Matrix(constants)
        # # # lb = constants*0
        # #
        # # t2 = time.time()
        # print("Jacobian computed in {} seconds".format(t2-t1))
        self.__symbolic_jacobian__ = [mat,constants]
        import sympy
        self.__eval_jacobian__ = [sympy.lambdify(self.syms, e) for e in [mat,constants]]

    def compute_jacobian(self):
        if self.__eval_jacobian__ is None:
            self.compute_symbolic_jacobian()

        v = [self.param_values[str(e)] for e in self.syms]
        return [fun(*v) for fun in self.__eval_jacobian__]

class ConstrainedModel(TreeModel):



    def __init__(self, etree, **param_values):

        import sympy

        self.etree = etree
        params = sympy.symbols('beta,a,c,estar,Rbar,R,min_f')

        isyms = [IVar(dummy, etree) for dummy in str.split( 'e,f,z,Gamma,Psi,Phi',',')]
        e, f, z, Gamma, psi, phi = isyms

        self.syms = params
        self.isyms = isyms
        self.param_values = param_values
        self.make_equations()

    def make_equations(self):

        import sympy
        etree = self.etree


        beta,a,c,estar, Rbar, R, min_f = self.syms
        e, f, z, Gamma, psi, phi = self.isyms
        p = etree.probas

        equations = []
        variables = []
        lower_bounds = []

        for s in etree.nodes:
            equations.append(
                - z[s] + etree.values[s]
            )
            variables.append(z[s])

        for s in etree.nodes:
            equations.append(
                - e[s] + a/(a+c)*Sum(lambda x: p[s,x]*e[x], etree.children(s)) + 1.0/(a+c)*(z[s]-f[s])
            )
            variables.append(e[s])

        for s in etree.nodes:
            t = len(s)-1

            if len(s)==1:
                equations.append(
                    - Gamma[s] + (e[s]-estar)
                )
            if len(s)>1:
                ss = etree.parent(s)
                equations.append(
                    - Gamma[s] + a/(a+c)*Gamma[ss] + beta**t*(e[s]-estar)
                )
            variables.append(Gamma[s])

        for s in etree.nodes:
            if len(s)==1:
                equations.append(
                    - Gamma[s] + Sum(lambda x: p[s,x]*Gamma[x], etree.children(s)) + psi[s] - phi[s]
                    # - f[s]
                )
            elif 1<len(s)<len(etree):
                equations.append(
                    - Gamma[s] + Sum(lambda x: p[s,x]*Gamma[x], etree.children(s)) + psi[s] - phi[s]
                    # - f[s]
                )
            else:
                equations.append(
                    - f[s]
                )
            variables.append(f[s])

        for s in etree.nodes:
            equations.append(
                -psi[s] + 1000*( Sum(lambda x: f[x], etree.history(s)) - Rbar )
            )
            variables.append(psi[s])

        for s in etree.nodes:
            equations.append(
                -phi[s] + (-(f[s]-min_f))*1000
            )
            variables.append(phi[s])

        N = len(etree.nodes)
        lower_bounds = [-10000]*(N*4)+[0]*(N*2)

        self.equations = equations
        self.variables = variables
        self.lower_bounds = lower_bounds


class VolumeModel(TreeModel):

    __jacobian__ = None

    def __init__(self, etree, **param_values):

        import sympy

        self.etree = etree
        params = sympy.symbols('beta,a,c,estar,Rbar,R,min_f,kappa')

        isyms = [IVar(dummy, etree) for dummy in str.split( 'e,f,z,Gamma,Psi,Phi',',')]
        e, f, z, Gamma, psi, phi = isyms

        self.syms = params
        self.isyms = isyms
        self.param_values = param_values
        self.make_equations()

    def make_equations(self):

        import sympy
        etree = self.etree


        beta,a,c,estar, Rbar, R, min_f,kappa = self.syms
        e, f, z, Gamma, psi, phi = self.isyms
        p = etree.probas

        equations = []
        variables = []
        lower_bounds = []

        for s in etree.nodes:
            equations.append(
                - z[s] + etree.values[s]
            )
            variables.append(z[s])

        for s in etree.nodes:
            equations.append(
                - e[s] + a/(a+c)*Sum(lambda x: p[s,x]*e[x], etree.children(s)) + 1.0/(a+c)*(z[s]-f[s])
            )
            variables.append(e[s])

        for s in etree.nodes:
            t = len(s)-1

            if len(s)==1:
                equations.append(
                    - Gamma[s] + (e[s]-estar)
                )
            if len(s)>1:
                ss = etree.parent(s)
                equations.append(
                    - Gamma[s] + a/(a+c)*Gamma[ss] + beta**t*(e[s]-estar)
                )
            variables.append(Gamma[s])

        for s in etree.nodes:
            if len(s)<len(etree):
                equations.append(
                    - f[s] + kappa*z[s] - psi[s]
                )
            else:
                equations.append(
                    - f[s]
                )
            variables.append(f[s])



        for s in etree.nodes:
            equations.append(
                -phi[s] + (-(f[s]-min_f))*1000
            )
            variables.append(phi[s])

        for s in etree.nodes:
            equations.append(
                -psi[s] + ( Sum(lambda x: f[x], etree.history(s)) - Rbar)*1000
            )
            variables.append(psi[s])

        N = len(etree.nodes)
        lower_bounds = [-10000]*(N*5)+[0]*(N)

        self.equations = equations
        self.variables = variables
        self.lower_bounds = lower_bounds


        #
class PegModel(TreeModel):

    __jacobian__ = None

    def __init__(self, etree, **param_values):

        import sympy

        self.etree = etree
        params = sympy.symbols('beta,a,c,estar,Rbar,R,min_f,kappa')

        isyms = [IVar(dummy, etree) for dummy in str.split( 'e,f,z,Gamma,Psi,Phi',',')]
        e, f, z, Gamma, psi, phi = isyms

        self.syms = params
        self.isyms = isyms
        self.param_values = param_values
        self.make_equations()

    def make_equations(self):

        import sympy
        etree = self.etree


        beta,a,c,estar, Rbar, R, min_f,kappa = self.syms
        e, f, z, Gamma, psi, phi = self.isyms
        p = etree.probas

        equations = []
        variables = []
        lower_bounds = []

        for s in etree.nodes:
            equations.append(
                - z[s] + etree.values[s]
            )
            variables.append(z[s])

        for s in etree.nodes:
            equations.append(
                - e[s] + a/(a+c)*Sum(lambda x: p[s,x]*e[x], etree.children(s)) + 1.0/(a+c)*(z[s]-f[s])
            )
            variables.append(e[s])

        for s in etree.nodes:
            t = len(s)-1

            if len(s)==1:
                equations.append(
                    - Gamma[s] + (e[s]-estar)
                )
            if len(s)>1:
                ss = etree.parent(s)
                equations.append(
                    - Gamma[s] + a/(a+c)*Gamma[ss] + beta**t*(e[s]-estar)
                )
            variables.append(Gamma[s])

        for s in etree.nodes:
            if len(s)<len(etree):
                # myopic peg
                equations.append(
                    - e[s] + z[s]/c*(1-kappa) + psi[s]
                )
            else:
                equations.append(
                    -f[s]
                )
            variables.append(f[s])



        for s in etree.nodes:
            equations.append(
                -phi[s] + (-(f[s]-min_f))*1000
            )
            variables.append(phi[s])

        for s in etree.nodes:
            equations.append(
                -psi[s] + ( Sum(lambda x: f[x], etree.history(s)) - Rbar)*1000
            )
            variables.append(psi[s])

        N = len(etree.nodes)
        lower_bounds = [-10000]*(N*5)+[0]*(N)

        self.equations = equations
        self.variables = variables
        self.lower_bounds = lower_bounds
