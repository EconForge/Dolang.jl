class Symbol:

    def __init__(self, name=None, subscripts=None, date=None):
        if date is None:
            date = 0
        if subscripts is None:
            subscripts = []
        assert(isinstance(date,int))
        for s in subscripts:
            assert(isinstance(s,str))
            assert('__' not in s)
        assert(name.isidentifier())
        self.subscripts = tuple( subscripts )
        self.date = date
        self.name = name

    def __str__(self):
        # sep = 'ⵂ'
        # close_lhs = 'ⵂ'
        # close_rhs = 'ⵂ'
        sep = '__'
        close_lhs = '__'
        close_rhs = ''
        # close_lhs = '᚜'
        # close_rhs = '᚛'
        if self.date:
            argss = str.join(sep, [write_time_subscript(self.date)] + list(self.subscripts))
        else:
            argss =  str.join(sep, list(self.subscripts))
        s = self.name + close_lhs + argss + close_rhs
        return s

def write_time_subscript(s):
    # neg = '֊'
    neg = 'm'
    if isinstance(s, int):
        if s>=0:
            return str(s)
        elif s<0:
            return neg + str(-s)
    else:
        raise Exception("Don't know what to do with that")
#
# Module(
#     body=[
#         Expr(value=Subscript(value=Name(id='E', ctx=Load()), slice=Index(value=Compare(lef│E[ s[1] | s[1] , S(s) ] : OK
# t=BinOp(left=Subscript(value=Name(id='s', ctx=Load()), slice=Index(value=Num(n=1)), ctx=Load()), op=BitOr│E[ s[1] | s[1] ∈ S(s) ] : OK
# (), right=Subscript(value=Name(id='s', ctx=Load()), slice=Index(value=Num(n=1)), ctx=Load())), ops=[In()]│Any[("E( s[1] for s[1] in S(s) )","Failed"),("E[ s[1] for s[1] in S(s) ]","OK"),("E[ s[1] | s[1] in S(s)
# , comparators=[Call(func=Name(id='S', ctx=Load()), args=[Name(id='s', ctx=Load())], keywords=[], starargs│]","OK"),("E[ s[1] | s[1] | S(s) ]","OK"),("E[ s[1] | s[1] , S(s) ]","OK"),("E[ s[1] | s[1] ∈ S(s) ]","OK
# =None, kwargs=None)])), ctx=Load()))
#     ]
# )
# Module(body=[
#     Expr(value=Subscript(
#             value=Name(id='E', ctx=Load()),slice=Index(value=Compare(left=BinOp(left=Subscript(value=Name(id='s', ctx=Load()), slice=Index(value=Num(n=1)), ctx=Load()), op=BitOr(),
# right=Subscript(value=Name(id='s', ctx=Load()), slice=Index(value=Num(n=1)), ctx=Load())), ops=[In()]=None, kwargs=None)])),
# ctx=Load()))
#     ]
# )
#
#
# Subscript(
#     value=Name(id='E', ctx=Load()),
#     slice=Index(
#         value=Compare(
#                 left=BinOp(
#                     left=Subscript(
#                             value=Name(id='s', ctx=Load()),
#                             slice=Index(value=Num(n=1)),
#                             ctx=Load()
#                         ),
#                     op=BitOr(),
#                     right=Subscript(
#                         value=Name(id='s', ctx=Load()),
#                         slice=Index(value=Num(n=1)),
#                         ctx=Load()
#                         )
#                     ),
#                 ops=[In()],
#                 comparators=[Call(func=Name(id='S', ctx=Load()), args=[Name(id='s', ctx=Load())], keywords=[], starargs=None, kwargs=None)]
#             )
#     ),
#     ctx=Load()
# )
#
# from ast import *
# to_parse = Module(
#     body=[
#         Expr(value=
#             Subscript(
#                 value=Name(id='E', ctx=Load()),
#                 slice=Index(
#                     value=Compare(
#                         left=BinOp(
#                             left=Name(                           # expr
#                                 id='u',
#                                 ctx=Load()
#                             ),
#                             op=BitOr(),
#                             right=Name(id='u', ctx=Load())       # dummy_var
#                         ),
#                         ops=[In()],
#                         comparators=[
#                             Call(
#                                 func=Name(id='S', ctx=Load()),
#                                 args=[Name(id='s', ctx=Load())],
#                                 keywords=[],
#                                 starargs=None,
#                                 kwargs=None
#                             )
#                         ]
#                     )
#                 ),
#                 ctx=Load()
#                 )
#             )
#     ]
# )
#
# print(isinstance(In(),In))
import ast
from ast import *

s = (0,1)
def S(s):
    return ( (0.1, (0,1,0)),  (0.1, (0,1,1)) )
def P(s):
    return (0,)

eqs = 'a + b[s] + c[S(s)] + d[P(s)]'
eq = ast.parse(eqs).body[0].value

ast.dump(eq)
def tuple_to_ast(t):
    elts = list([Num(n=e) for e in t])
    return Tuple(elts=elts, ctx=Load())

class ReplaceNode(ast.NodeTransformer):

    def __init__(self, s, P,S, state_name='s'):
        from ast import Tuple, Load
        elts = list([Num(n=e) for e in s])
        self.s_expr = Tuple(elts=elts, ctx=Load())
        self.context = {
            'P': P,
            'S': S
        }
        self.P = P
        self.S = S
        self.state_name = state_name

    def visit_Call(self, node):
        # print(ast.dump(node))
        if node.func.id == 'P':
            assert node.args[0].id == self.state_name
            ss = self.P(s)
            return tuple_to_ast(ss)
        # elif node.func.id == 'S':

        else:
            return node

    def visit_Name(self, node):
        if node.id == self.state_name:
            return self.s_expr
        else:
            return node

s = (0,1)
rp = ReplaceNode(s, P, S)
eqn = rp.visit(eq)
eqn = eq
# eqn = eq
expr = Expression(eqn)


expr = ast.fix_missing_locations(expr)

print("HI")
print(ast.dump(expr))
cc = compile(expr,"<string>",'eval')

ast.dump(tuple_to_ast(s))
print("he")
print(ast.dump(expr))
# eval(cc)

#####


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
        kk = self.c.copy() # ?
        for k in kk.keys():
            kk[k] /= v

        return LinearExpression(c=kk)

    def __repr__(self):
        return str(self.c)



class LIVar:
    # indexed symbol which returns a linear expression

    def __init__(self, name, etree):
        self.name = name
        self.etree = etree

    def __getitem__(self, x):
        # import sympy
        # return sympy.Symbol(self.name+'_'+str.join('', map(str,x)))
        ind = self.etree.nodes.index(x)   # why use the index ?
        name = '{}_{}'.format(self.name,ind)
        return LinearExpression(name=name)

#
#
# a0 = LinearExpression({1: 0.2})
# a1 = LinearExpression({1: 0.2})
# a2 = LinearExpression({'a': 0.43})
# a3 = LinearExpression({1: 0.2,'a':0.5})
# a2
#
# a1
# a2
#
# a1 + a2 - a3
#
# a2
#
# a2/0.43
#
#
#
#
# ss = sum([a3 for i in range(1000)], a0)
# ss
# ss/200
# ########################
# # Play with expections #
# ########################
#
#
# import ast
# # expect_expr = to_parse.body[0].value
# def recognize_expectation(expr):
#     try:
#         assert("E"==expr.value.id)
#         assert( isinstance( expr.slice.value.ops[0],In) )
#         u = integrand = expr.slice.value.left.left
#         v = dummy_var = expr.slice.value.left.right
#         w = int_set = expr.slice.value.comparators[0]
#         # print(ast.dump(integrand))
#         # print(ast.dump(dummy_var))
#         # print(ast.dump(int_set))
#         return Call(func=Name(id='E', ctx=Load()), args=[u,v,w], keywords=[], starargs=None, kwargs=None)
#
#     except Exception as e:
#         raise(e)
#         return expr
#
# s = 's'
# u = 'u'
# expr = 'u**2'
#
# def S(h):
#     return 'S({})'.format(str(h))
#
# def E(u, s, v):
#     return "E[ {} | {} in {}]".format(u,s,v)
#
#
# cc = recognize_expectation(expect_expr)
# ast.fix_missing_locations(cc)
# com = compile( ast.Expression(cc), "<string>", 'eval' )
# eval(com)
#
#
#
#
#
# ast.dump(dest)
#
# Call(func=Name(id='E', ctx=Load()), args=[u,v,w], keywords=[], starargs=None, kwargs=None)
