import ast
Expression = ast.Expr

from dolang.language import functions as functions_dict
functions = list(functions_dict.keys())
from typing import Tuple, List

# from dolang.parser import parse_string

def stringify_variable(arg: Tuple[str, int]) -> str:
    s = arg[0]
    date = arg[1]
    if date == 0:
        return '{}__'.format(s)
    elif date <= 0:
        return '{}__m{}_'.format(s, str(-date))
    elif date >= 0:
        return '{}__{}_'.format(s, str(date))

def stringify_parameter(p: str) -> str:
    return '{}_'.format(p)

def stringify(arg) -> str:
    if isinstance(arg, str):
        return stringify_parameter(arg)
    elif isinstance(arg, tuple):
        if len(arg)==2 and isinstance(arg[0],str) and isinstance(arg[1],int):
            return stringify_variable(arg)
    raise Exception("Unknown canonical form: {}".format(arg))

###

def destringify_variable(s: str) -> Tuple[str, int]:
    raise Exception("Not implemented.")

def destringify_parameter(s: str) -> Tuple[str, int]:
    raise Exception("Not implemented.")

def destringify(s: str):
    raise Exception("Not implemented.")

###

def normalize(expr: Expression, variables: List[str])->Expression:
    import copy
    en = ExpressionNormalizer(variables=variables)
    return en.visit(copy.deepcopy(expr))

# shift timing in equations
# time_shift(:(a+b(1)+c),1,[:b,:c]) == :(a+b(2)+c(1))
def time_shift(expr: Expression, n, vars: List[str]) -> Expression:
    import copy
    eexpr = copy.deepcopy(expr)
    return TimeShiftTransformer(shift=n, variables=vars).visit(eexpr)

#
def steady_state(expr: Expression, vars: List[str]) -> Expression:
    import copy
    eexpr = copy.deepcopy(expr)
    return TimeShiftTransformer(shift='S', variables=vars).visit(eexpr)


# list variables
# list_variables(:(a+b(1)+c), [:b,:c,:d]) == [(:b,1),(:c,0)]
#


def list_variables(expr: Expression, funs: List[str]=None, vars: List[str]=None) -> List[Tuple[str,int]]:
    if funs is None: funs=[]
    if vars is None: vars=[]
    l = ListSymbols(known_functions=functions+funs, known_variables=vars)
    l.visit(expr)
    if l.problems:
        e = Exception('Symbolic error.')
        e.problems = l.problems
    return [v[0] for v in l.variables]

def list_symbols(expr: Expression, funs: List[str]=None, vars: List[str]=None) -> List[Tuple[str,int]]:
    if funs is None: funs=[]
    l = ListSymbols(known_functions=functions+funs, known_variables=vars)
    l.visit(expr)
    if l.problems:
        e = Exception('Symbolic error.')
        e.problems = l.problems
    head = lambda v: [i[0] for i in v]
    d = {
        'variables': head(l.variables),
        'constants': head(l.constants),
        'functions': head(l.functions)
    }
    return d




# substitute expression
# subs(:(a + b), :b, :(c+d(1)) == :(a+c+d(1))
#
# subs(expr: Expression, symbol: Symbol, sexpr: Expression) -> Expression



from dolang.language import functions as functions_dict



from ast import NodeTransformer, Name, UnaryOp, UAdd, USub, Load, Call

class ExpressionNormalizer(NodeTransformer):

    # replaces calls to variables by time subscripts

    def __init__(self, variables=None, functions=None):

        self.variables = variables if variables is not None else []
        if functions is None:
            self.functions = [e for e in functions_dict.keys()]
        else:
            self.functions = functions
        # self.variables = tvariables # ???

    def visit_Name(self, node):

        name = node.id
        # if name self.functions:
        #     return node
        if name in self.variables:
            return Name(id=stringify_variable((name,0)), ctx=Load())
        else:
            return Name(id=stringify_parameter(name), ctx=Load())

    def visit_Call(self, node):

        name = node.func.id
        args = node.args[0]

        if name in self.variables:
            try:
                date = eval_scalar(args)
            except:
                raise Exception("Unrecognized subscript.")
            newname = stringify_variable((name, date))
            if newname is not None:
                return Name(newname, Load())
        else:
            return Call(func=node.func, args=[self.visit(e) for e in node.args], keywords=[])


class TimeShiftTransformer(ast.NodeTransformer):

    def __init__(self, shift=0, variables=None):

        self.variables = variables if variables is not None else []
        self.shift = shift

    def visit_Name(self, node):
        name = node.id
        if name in self.variables:
            if self.shift==0 or self.shift=='S':
                return ast.parse(name).body[0].value
            else:
                return ast.parse('{}({})'.format(name,self.shift)).body[0].value
        else:
             return node

    def visit_Call(self, node):

        name = node.func.id
        args = node.args[0]

        if name in self.variables:
            if isinstance(args, UnaryOp):
                # we have s(+1)
                if (isinstance(args.op, UAdd)):
                    args = args.operand
                    date = args.n
                elif (isinstance(args.op, USub)):
                    args = args.operand
                    date = -args.n
                else:
                    raise Exception("Unrecognized subscript.")
            else:
                date = args.n
            if self.shift =='S':
                return ast.parse('{}'.format(name)).body[0].value
            else:
                new_date = date+self.shift
                if new_date != 0:
                    return ast.parse('{}({})'.format(name,new_date)).body[0].value
                else:
                    return ast.parse('{}'.format(name)).body[0].value
        else:

            # , keywords=node.keywords,  kwargs=node.kwargs)
            return Call(func=node.func, args=[self.visit(e) for e in node.args], keywords=[])


def eval_scalar(tree):
    try:
        if isinstance(tree, ast.Num):
            return tree.n
        elif isinstance(tree, ast.UnaryOp):
            if isinstance(tree.op, ast.USub):
                return -tree.operand.n
            elif isinstance(tree.op, ast.UAdd):
                return tree.operand.n
        else:
            raise Exception("Don't know how to do that.")
    except:
        raise Exception("Don't know how to do that.")

class ListSymbols(ast.NodeVisitor):

    def __init__(self, known_functions=[], known_variables=[]):
        self.known_functions = known_functions
        self.known_variables = known_variables
        self.functions = []
        self.variables = []
        self.constants = []
        self.problems = []

    def visit_Call(self, call):
        name = call.func.id
        colno = call.func.col_offset
        if name in self.known_functions:
            self.functions.append((name, colno))
            [self.visit(e) for e in call.args]
        else:
            try:
                assert(len(call.args)==1)
                n = int(eval_scalar(call.args[0]))
                self.variables.append(((name,n),colno))
            except:
                if name in self.known_variables:
                    self.problems.append([name,0,colno,'incorrect subscript'])
                else:
                    self.problems.append([name,0,colno,'unknown_function'])
                [self.visit(e) for e in call.args]

    def visit_Name(self, name):
        # colno = name.colno
        colno = name.col_offset
        n = 0
        name = name.id
        if name in self.known_variables:
            self.variables.append( ((name,0), colno))
        elif name in self.known_functions:
            self.problems.append([name, colno, 'function_not_called' ])
        else:
            self.constants.append( (name, colno) )
