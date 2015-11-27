class IVar:
    def __init__(self, name, etree):
        self.name = name
        self.etree = etree

    def __getitem__(self, x):
        import sympy
        # return sympy.Symbol(self.name+'_'+str.join('', map(str,x)))
        ind = self.etree.nodes.index(x)
        return sympy.Symbol('{}_{}'.format(self.name,ind))

def Sum(fun, ss):
    import sympy
    s = [sympy.sympify(fun(x)) for x in ss]
    return sum(s)


def contained(t1,t2):
    if len(t1)>len(t2):
        return False
    elif len(t1)==len(t2):
        return t1==t2
    else:
        return t1==t2[:len(t1)]

class EventTree:

    nodes = None # list of nodes
    probas = dict()
    values = dict()

    def children(self, s):
        # returns a list of all children of s
        if len(s)==len(self):
            return [s]
        else:
            return [e for e in self.nodes if len(s)==len(e)-1 and contained(s,e)]

    def parent(self, s):
        # returns parent of s
        return s[:-1]

    def history(self, s):
        return [s[:i] for i in range(1,len(s)+1)]

    def __len__(self):
        return max([len(e) for e in self.nodes])

    def graph(self, plot=False):
        import networkx as nx
        G = nx.DiGraph()
        for n1,n2 in self.probas:
            G.add_edge(n1,n2,{'p':self.probas[n1,n2]})
        if plot:
            nx.draw_spring(G)
        else:
            return G



class DeterministicTree(EventTree):
    def __init__(self, N):

        etree = self
        etree.nodes = [(0,)*t for t in range(1,N+1)]
        for s in etree.nodes:
            etree.values[s] = 0
            if len(s)>1:
                etree.probas[(etree.parent(s), s)] = 1
            if len(s)==len(etree):
                etree.probas[(s,s)] = 1


class BranchTree(EventTree):

    def __init__(self, N, T, p):
        '''
        p: [p_0, p_1]
        '''

        T = T+1
        etree = self
        nodes = [(0,)*t for t in range(1,T)]
        new_paths = []
        for i in range(2):
            nn = nodes[-1] + (i,)
            new_paths.append([nn+(0,)*t for t in range(0,N-T+1)])
        for i in range(len(new_paths[0])):
            for b in range(2):
                nodes.append(new_paths[b][i])
        etree.nodes = nodes

        for s in etree.nodes:
            etree.values[s] = 0
            if len(s)<len(etree):
                if len(etree.children(s))>1:
                    etree.probas[(s, etree.children(s)[0])] = p[0]
                    etree.probas[(s, etree.children(s)[1])] = p[1]
                else:
                    etree.probas[(s, etree.children(s)[0])] = 1
            if len(s)==len(etree):
                etree.probas[(s,s)] = 1


def get_model(etree, **parms):
    import sympy
    params = {sympy.Symbol(k,v): v for k,v in parms.items()}
    equations,variables,lower_bounds = make_equations(etree)
    mat,constants = compute_jacobian(equations, variables)



def count_variables(equations, params):
    var = set()
    for eq in equations:
        var.update([a for a in eq.atoms() if isinstance(a,sympy.Symbol) and a not in params])
    return len(var)

def solve_system(equations, variables, param_values):
    import sympy
    eqs = [eq.subs(param_values) for eq in equations]
    sol = sympy.solve(eqs, variables)
    return sol

def compute_jacobian(equations, variables):

    # So this assume the system is linear !

    import sympy
    zz = {v: 0 for v in variables}

    n = len(variables)

    mat = sympy.Matrix(n,n,lambda i,j: equations[i].expand().coeff(variables[j])).subs(param_values)
    # spmat = sympy.SparseMatrix(mat)
    #
    constants = []
    for eq in equations:
        zz = {a: 0 for a in eq.atoms() if a in variables}
        constants.append(-eq.subs(zz).subs(param_values))
    constants = sympy.Matrix(constants)
    lb = constants*0

    return [mat, constants]
#
#


### solve the complementarity problem




def solve_model(model, verbose=False):
    import numpy

    mat,constants = model.compute_jacobian()
    lb = model.lower_bounds

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
    return OrderedDict(zip(map(str,model.variables), res))

def get_ts(etree, sol, vname, ts_ind=0):
    import numpy
    terminal_states = [e for e in etree.nodes if len(e)==len(etree)]
    history = etree.history(terminal_states[ts_ind])
    his_inds = [etree.nodes.index(e) for e in history]
    vals = [sol["{}_{}".format(vname, h)] for h in his_inds]
    return numpy.array(vals).astype(dtype=float)



#
# evec = res[N:2*N]
# gvec = res[2*N:3*N]
# fvec = res[3*N:4*N]
# psvec = res[4*N:5*N]
# phvec = res[5*N:6*N]
