from treesolve import DeterministicTree



class LinearExpression:

    def __init__(self, c=None, name=None):
        if name is not None:
            c = {name: 1.0}
        if c is None:
            c = dict()
        if 1 not in c:
            c[1] = 0.0
        self.c = c
        self.name = name

    def __add__(self, d):
        kk = self.c.copy() # ?
        if isinstance(d, LinearExpression):
            for (k,v) in d.c.items():
                if k in kk:
                    kk[k] = kk[k] + v
                else:
                    kk[k] = v
        else:
            kk[1] = kk[1] + d
        return LinearExpression(c=kk)

    __radd__ = __add__


    def __sub__(self, d):
        kk = self.c.copy() # ?
        if isinstance(d, LinearExpression):
            for (k,v) in d.c.items():
                if k in kk:
                    kk[k] = kk[k] - v
                else:
                    kk[k] = -v
        else:
            kk[1] = kk[1] - d
        return LinearExpression(c=kk)

    def __rsub__(self, d):
        return self.__neg() + d

    def __neg__(self):
        kk = self.c.copy() # ?
        for k in kk.keys():
            kk[k] = -kk[k]
        return LinearExpression(c=kk)

    def __mul__(self, v):
        kk = self.c.copy() # ?
        for k in kk.keys():
            kk[k] *= v

        return LinearExpression(c=kk)

    __rmul__ = __mul__


    def __truediv__(self, v):
        print("v : {}".format(v))
        kk = self.c.copy() # ?
        for k in kk.keys():
            kk[k] /= v

        return LinearExpression(c=kk)

    def __repr__(self):
        return str(self.c)

N = 20
etree = DeterministicTree(N)



# now they are parameter
beta = 0.97
a = 1.1
c=0.1
estar=-0.1
Rbar=0.5
R=10
min_f = 0.0


class IVar:
    def __init__(self, name, etree):
        self.name = name
        self.etree = etree

    def __getitem__(self, x):
        import sympy
        # return sympy.Symbol(self.name+'_'+str.join('', map(str,x)))
        ind = self.etree.nodes.index(x)
        name = '{}_{}'.format(self.name,ind)
        return LinearExpression(name=name)

sym_names = str.split("e, f, z, Gamma, psi, phi",', ')
e, f, z, Gamma, psi, phi = [IVar(zz, etree) for zz in sym_names]


def Sum(f, S):
    return sum([f(zz) for zz in S], LinearExpression())

Sum(lambda x: e[x]*p[s,x], etree.children(s))


equations = []
variables = []
lower_bounds = []

import time
t1 = time.time()

for s in etree.nodes:
    equations.append(
        - z[s] + etree.values[s]
    )
    variables.append(z[s])

for s in etree.nodes:
    equations.append(
        - e[s] + a/(a+c)*Sum(lambda x: e[x]*p[s,x], etree.children(s)) + 1.0/(a+c)*(z[s]-f[s])
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

lower_bounds = [-10000]*(N*4)+[0]*(N*2)
t2 = time.time()



print("Proof of concept: {}".format(t2-t1))

equations[0]
equations[1]
equations[2]
equations[10]
equations[100]
equations[99]
v = variables[0]

varnames = [v.name for v in variables]

Neq = len(varnames)
t3 = time.time()
import numpy
res = numpy.zeros(Neq)
jac = numpy.zeros((Neq,Neq))
for i in range(Neq):
    eq = equations[i]
    res[i] = eq.c[1]
    for k in eq.c.keys():
        if k != 1:
            j = varnames.index(k)
            jac[i, j] = eq.c[k]
t4 = time.time()

print(t4-t3)
