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
        sep = 'ⵂ'
        close_lhs = 'ⵂ'
        close_rhs = 'ⵂ'
        # close_lhs = '᚜'
        # close_rhs = '᚛'
        if self.date:
            argss = str.join(sep, [write_time_subscript(self.date)] + list(self.subscripts))
        else:
            argss =  str.join(sep, list(self.subscripts))
        s = self.name + close_lhs + argss + close_rhs
        return s
#
# def parse_time_subscript(s):
#     if len(s)==0:
#         return s
#     if s[0] == 'm':   # must be m1
#         i = int(s[1:])
#         return -i
#     elif s[0] == 'p':
#         i = int(s[1:])
#         return i
#     else:
#         return s

def write_time_subscript(s):
    neg = '֊'
    neg = 'm'
    if isinstance(s, int):
        if s>=0:
            return str(s)
        elif s<0:
            return neg + str(-s)
    else:
        raise Exception("Don't know what to do with that")

# def write_time_subscript(s):
#     if isinstance(s, int):
#         if s>=0:
#             return 'p'+str(s)
#         elif s<0:
#             return 'm'+str(-s)
#     else:
#         raise Exception("Don't know what to do with that")

results = {
    'p1' : +1,
    'p10': +10,
    'm1': -1,
    'm10': -10
}
#
# def test_parse_subcript():

    #
    # for k in results:
    #     res = parse_time_subscript(k)
    #     expected = results[k]
    # #     print(res)
    # #     print(expected)
    # #     assert(res == expected)
    #
    #
    # for k in results:
    #     res = write_time_subscript(results[k])
    #     print(res)
    #     print(k)
    #     assert(res == k)


s = Symbol(name='symbol', subscripts=['i','j'])
ss = Symbol(name='β', subscripts=['i','j'], date=1)
ss = Symbol(name='β', subscripts=['i'], date=1)
tt = Symbol(name='β', subscripts=['i'])

ss = Symbol(name='β', date=1)
tt = Symbol(name='β')

print(tt, ss)

a = Symbol(name='β', subscripts=['i','j'], date=-1)
b = Symbol(name='β', subscripts=['i'], date=-1)
c = Symbol(name='β', subscripts=['i'])
d = Symbol(name='β', subscripts=['i','j'], date=1)
e = Symbol(name='β', subscripts=['i'], date=1)
import ast
eq =  '{} + {} + {} + {} + {}'.format(a,b,c,d,e)
print(eq)
ast.parse( eq)
