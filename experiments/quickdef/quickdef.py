 # -*- coding: utf-8 -*-

from collections import OrderedDict

def construct_assign_dict(target, dict_name):
    # construct_assign_dict('a','d')
    # -> a = d['a']
    from ast import Assign, Name, Store, Subscript, Load, Index, Str
    a = target
    d = dict_name
    expr = Assign(targets=[Name(id=a, ctx=Store())], value=Subscript(value=Name(id=d, ctx=Load()), slice=Index(value=Str(s=a)), ctx=Load()))
    return expr

def construct_assign_array(target, array_name, index):
    # construct_assign_array('a', 'v', i)
    # -> a = v[i]
    from ast import Assign, Name, Store, Subscript, Load, Index, Str, Num
    a = target
    d = array_name
    i = index
    expr = Assign(targets=[Name(id=a, ctx=Store())], value=Subscript(value=Name(id=array_name, ctx=Load()), slice=Index(value=Num(n=i)), ctx=Load()))
    return expr

from dolo.compiler.codegen import to_source

class quickdef(object):

    def __init__(self, dd, dict_name='params', sub_type='dict'):
        self.dictionary = dd
        self.sub_type = sub_type

    def __call__(self, f):
        import ast
        import inspect
        from ast import arg

        dd = self.dictionary
        sub_type = self.sub_type

        tree = ast.parse(inspect.getsource(f))
        fundef = tree.body[0]
        fundef.decorator_list = []
        funname = fundef.name
        arguments = tree.body[0].args
        i = len(arguments.args) - len(arguments.defaults)
        # print(i)
        arguments.args.insert( i, arg(arg='ddict', annotation=None))
        funbody = tree.body[0].body
        if sub_type == 'dict':
            new_lines = [construct_assign_dict(k,'ddict') for k in dd.keys()]
        elif sub_type == 'array':
            new_lines = [construct_assign_array(k,'ddict',i) for i,k in enumerate(dd.keys())]
        else:
            raise Exception("Unknown value for option 'sub_type'. Available values: 'dict' and 'array'")
        for l in new_lines:
            l.lineno = 0
            l.col_offset = 0
            # ast.fix_missing_locations(l)
        funbody[0:0] = new_lines
        ast.fix_missing_locations(tree)

        code = compile(tree, '<string>', 'exec')

        namespace = dict()
        exec(code, namespace)

        return namespace[funname]


class quickargs(object):

    def __init__(self, dd):
        self.dictionary = dd

    def __call__(self, f):
        import ast
        import inspect
        from ast import arg

        dd = self.dictionary

        tree = ast.parse(inspect.getsource(f))
        fundef = tree.body[0]
        fundef.decorator_list = []
        funname = fundef.name

        # add arguments
        arguments = tree.body[0].args
        # for symbol_group in dd.keys():
        #     i = len(arguments.args) - len(arguments.defaults)
        #     print("{}, {}, {}".format(len(arguments.args), len(arguments.defaults), i))
        #     arguments.args.insert( i, arg(arg=symbol_group, annotation=None))
        i = len(arguments.args) - len(arguments.defaults) - 1
        new_args = [arg(arg=symbol_group, annotation=None) for symbol_group in dd.keys()]
        arguments.args[i:i] = new_args

        # add declarations
        new_lines = []
        funbody = tree.body[0].body
        for symbol_group in dd.keys():
            symbol_group_arg_name = symbol_group
            new_lines.extend( [construct_assign_array(k,symbol_group_arg_name,i) for i,k in enumerate(dd[symbol_group]) ] )

        for l in new_lines:
            l.lineno = 0
            l.col_offset = 0

        funbody[0:0] = new_lines

        ast.fix_missing_locations(tree)
        code = compile(tree, '<string>', 'exec')

        namespace = dict()
        exec(code, namespace)

        return namespace[funname]


# basic usecase
calib = dict(a=1,b=2,c=3)

@quickdef(calib)
def fun(x):
    return x**2*a + x*b + c

assert(   fun(1.0, calib)  == 6.0)

# same but now the generated argument takes a vector instead of a dict

from collections import OrderedDict
calib = OrderedDict(a=1,b=2,c=3)

@quickdef(calib, sub_type='array')
def funvec(x):
    return x**2*a + x*b + c

calib_vec = [v for v in calib.values()]
assert(    funvec(1.0, calib_vec ) == 6.0   )
