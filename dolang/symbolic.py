from dolang.language import functions as functions_dict
from dolang.parser import parse_string
from dolang.codegen import to_source
from typing import Tuple, List, Dict, Set, TypeVar, Union
from ast import NodeTransformer, Name, UnaryOp, UAdd, USub, Load, Call
from .dataclasses import dataclass
import ast
from ast import Expr
from functools import wraps

Expression = ast.Expr

functions = list(functions_dict.keys())


# from dolang.parser import parse_string
T = TypeVar('T')


def dedup(l: List[T])->List[T]:
    return list(dict.fromkeys(l))


def stringify_variable(arg: Tuple[str, int]) -> str:
    s = arg[0]
    date = arg[1]
    if date == 0:
        return '{}__0_'.format(s)
    elif date <= 0:
        return '{}_m{}_'.format(s, str(-date))
    elif date >= 0:
        return '{}__{}_'.format(s, str(date))


def stringify_parameter(p: str) -> str:
    return '{}_'.format(p)


def stringify_symbol(arg) -> str:
    if isinstance(arg, str):
        return stringify_parameter(arg)
    elif isinstance(arg, tuple):
        if len(arg) == 2 and isinstance(arg[0], str) and isinstance(arg[1], int):
            return stringify_variable(arg)
    raise Exception("Unknown canonical form: {}".format(arg))


def destringify_variable(s: str) -> Tuple[str, int]:
    raise Exception("Not implemented.")


def destringify_parameter(s: str) -> Tuple[str, int]:
    raise Exception("Not implemented.")


def destringify(s: str):
    raise Exception("Not implemented.")

###
# The following function can take a string or an expression as the first argument, and return an argument of the same type.
###


def expression_or_string(f):
    @wraps(f)
    def wrapper(*args, **kwds):
        if not isinstance(args[0], str):
            return f(*args, **kwds)
        else:
            a = parse_string(args[0])
            nargs = tuple([a]) + args[1:]
            res = f(*nargs, **kwds)
            return to_source(res)
    return wrapper


@expression_or_string
def stringify(expr: Expression, variables: List[str] = [])->Expression:
    import copy
    en = ExpressionStringifier(variables=variables)
    return en.visit(copy.deepcopy(expr))

# shift timing in equations
# time_shift(:(a+b(1)+c),1,[:b,:c]) == :(a+b(2)+c(1))


@expression_or_string
def time_shift(expr: Expression, n) -> Expression:
    import copy
    eexpr = copy.deepcopy(expr)
    variables = [e[0] for e in list_variables(eexpr)]
    return TimeShiftTransformer(shift=n, variables=variables).visit(eexpr)


@expression_or_string
def steady_state(expr: Expression) -> Expression:
    import copy
    eexpr = copy.deepcopy(expr)
    variables = [e[0] for e in list_variables(eexpr)]
    return TimeShiftTransformer(shift='S', variables=variables).visit(eexpr)


@expression_or_string
def sanitize(expr: Expression, variables=None, functions=None):
    # special functions ?
    es = ExpressionSanitizer(variables=variables)
    return es.visit(expr)


def list_variables(expr: Expression, funs: List[str]=None) -> List[Tuple[str, int]]:

    if funs is None:
        funs = []
    ll = ListSymbols(known_functions=functions + funs)
    ll.visit(expr)
    if ll.problems:
        e = Exception('Symbolic error.')
        e.problems = ll.problems
        raise e
    return dedup([v[0] for v in ll.variables])


@dataclass
class SymbolList(dict):
    variables: Set[Tuple[str, int]]
    parameters: Set[str]
    functions: Set[str]


def list_symbols(expr: Expression, funs: List[str]=None) -> SymbolList:
    if funs is None:
        funs = []
    ll = ListSymbols(known_functions=functions + funs)
    ll.visit(expr)
    if ll.problems:
        e = Exception('Symbolic error.')
        e.problems = ll.problems
        print(e.problems)
        raise e

    def head(v): return [i[0] for i in v]
    return SymbolList(
            dedup(head(ll.variables)),
            dedup(head(ll.constants)),
            dedup(head(ll.functions))
        )


### Tree manipulation functions

def eval_scalar(tree: Expression)->Union[int, float]:
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
                assert(len(call.args) == 1)
                n = int(eval_scalar(call.args[0]))
                self.variables.append(((name, n), colno))
            except Exception as e:
                self.problems.append([name, 0, colno, 'incorrect subscript'])
                # [self.visit(e) for e in call.args]

    def visit_Name(self, name):
        # colno = name.colno
        colno = name.col_offset
        n = 0
        name = name.id
        if name in self.known_functions:
            self.problems.append([name, colno, 'function_not_called'])
        else:
            self.constants.append((name, colno))


class ExpressionStringifier(NodeTransformer):

    # replaces calls to variables by time subscripts

    def __init__(self, variables=None, functions=None, constants=None):

        self.variables = variables if variables is not None else []
        if functions is None:
            self.functions = [e for e in functions_dict.keys()]
        else:
            self.functions = functions
        if constants is None:
            self.constants = ['pi', 'e', 'inf']
        else:
            self.constants = constants
        # self.variables = tvariables # ???

    def visit_Name(self, node):

        name = node.id
        # if name self.functions:
        #     return node
        if name in self.constants:
            return node
        elif name in self.variables:
            return Name(id=stringify_variable((name, 0)), ctx=Load())
        else:
            return Name(id=stringify_parameter(name), ctx=Load())

    def visit_Call(self, node):

        name = node.func.id
        args = node.args[0]

        if name in self.variables or name not in self.functions:
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
            if self.shift == 'S':
                new_date = 0
            else:
                new_date = date + self.shift
            return ast.parse('{}({})'.format(name, new_date)).body[0].value
        else:

            # , keywords=node.keywords,  kwargs=node.kwargs)
            return Call(func=node.func, args=[self.visit(e) for e in node.args], keywords=[])


class ExpressionSanitizer(NodeTransformer):

    # replaces calls to variables by time subscripts
    def __init__(self, variables=None):
        self.variables = variables if variables is not None else []

    def visit_Name(self, node):
        name = node.id
        if name in self.variables:
            return ast.parse('{}(0)'.format(name)).body[0].value
        else:
            return node

    def visit_Call(self, node):
        name = node.func.id
        if name in self.variables:
            t = eval_scalar(node.args[0])
            return ast.parse('{}({})'.format(name, t)).body[0].value
        else:
            return Call(func=node.func, args=[self.visit(e) for e in node.args], keywords=[])


class NameSubstituter(ast.NodeTransformer):

    # substitutes a name by an expression
    def __init__(self, substitutions: Dict[str, Expr]):
        self.substitutions = substitutions

    def visit_Name(self, node):
        name = node.id
        if name in self.substitutions:
            return self.substitutions[name]
        else:
            return node
