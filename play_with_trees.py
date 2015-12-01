
source = '''         z[s] + etree.values[s]   | z[s]
                     e[s] + a/(a+c)*E[ e[x]| x in S(s)] + 1.0/(a+c)*(z[s]-f[s]) | e[s]
len(s)==1:           Gamma[s] + (e[s]-estar)                                    | Gamma[s]
len(s)>1:            Gamma[s] + a/(a+c)*Gamma[P(s)] + beta**t*(e[s]-estar)      | Gamma[s]
len(s)==1:           Gamma[s] + E[ Gamma[x] | x in S(s) ] + psi[s] - phi[s]     | f[s]
1<len(s)<len(etree): Gamma[s] + E[ Gamma[x] | x in S(s) ] + psi[s] - phi[s]     | f[s]
len(s)==len(etree):  f[s]                                                       | f[s]
                     psi[s] + 1000*( Sum[f[x] | x in H(s)] - Rbar )             | psi[s]
                     phi[s] + (-(f[s]-min_f))*1000                              | 0 <= phi[s]
'''

from pattern import *

lines = str.split(source, '\n')
# print(lines)


def read_equations(lines):

    lines = [l.strip() for l in lines if len(l.strip())>=1]
    conditions = []
    equations = []
    variables = []
    complementarities = []

    for l in lines:
        if ':' in l:
            cond, rhs = str.split(l, ':')
        else:
            cond, rhs = None, l
        rhs = rhs.strip()
        # print("cond : " + str(cond))
        if cond:
            cond = cond.strip()
        # print(rhs)
        d = match("_x | _y", rhs)
        if not d:
            lb = True
            d = match("_x | 0 <= _y", rhs)
        else:
            lb = False


        eq = d["_x"]
        comp = d['_y']
        var = comp
        # print( "... {}".format( (var, lb))  )

        conditions.append(cond)
        equations.append(eq)
        variables.append(var)
        complementarities.append(lb)

    return [conditions, equations, variables, complementarities]
