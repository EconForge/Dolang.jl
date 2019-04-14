import ast
from ast import Assign, arg, FunctionDef, Module, Store, Subscript, Name, Load, Index, Num
from ast import arguments as ast_arguments

from dolang.factory import FlatFunctionFactory
from dolang.symbolic import parse_string
from dolang.codegen import to_source


def compile_factory(fff: FlatFunctionFactory):

    arguments = [*fff.arguments.keys()]
    funname = fff.funname

    unpacking = []
    for i, (arg_group_name, arg_group) in enumerate(fff.arguments.items()):
        for pos, sym in enumerate(arg_group):

            rhs = Subscript(value=Name(id=arg_group_name, ctx=Load()),
                            slice=Index(Num(pos)), ctx=Load())
            val = Assign(targets=[Name(id=sym, ctx=Store())], value=rhs)
            unpacking.append(val)

    body = []

    for (k, neq) in fff.preamble.items():
        val = parse_string(neq).value
        line = Assign(targets=[Name(id=k, ctx=Store())], value=val)
        body.append(line)

    for n, (k, neq) in enumerate(fff.content.items()):
        # should the result of parse_string always of type Expr ?
        val = parse_string(neq).value
        line = Assign(targets=[Name(id=k, ctx=Store())], value=val)
        body.append(line)
    #
    for n, (lhs, neq) in enumerate(fff.content.items()):
        line = Assign(targets=[Subscript(value=Name(id='out', ctx=Load()),
                                         slice=Index(Num(n)), ctx=Store())], value=Name(id=lhs, ctx=Load()))
        body.append(line)

    f = FunctionDef(name=funname,
                    args=ast_arguments(args=[arg(arg=a) for a in arguments] + [arg(arg='out')],
                                       vararg=None, kwarg=None, kwonlyargs=[], kw_defaults=[], defaults=[]),
                    body=unpacking + body, decorator_list=[])

    mod = Module(body=[f])
    mmod = ast.fix_missing_locations(mod)
    return mmod


def make_method_from_factory(fff: FlatFunctionFactory, vectorize=True, use_file=False, debug=False):

    mod = compile_factory(fff)

    if debug:
        print(to_source(mod))

    if vectorize:
        coredims = [len(v) for k, v in fff.arguments.items()]
        signature = str.join(',', ['(n_{})'.format(d) for d in coredims])
        n_out = len(fff.content)
        if n_out in coredims:
            signature += '->(n_{})'.format(n_out)
            # ftylist = float64[:](*([float64[:]] * len(coredims)))
            fty = "void(*[float64[:]]*{})".format(len(coredims) + 1)
        else:
            signature += ',(n_{})'.format(n_out)
            fty = "void(*[float64[:]]*{})".format(len(coredims) + 1)
    else:
        signature = None

    fun = eval_ast(mod)

    from numba import jit, guvectorize

    jitted = jit(fun, nopython=True)
    if vectorize:
        gufun = guvectorize(
            [fty], signature, target='parallel', nopython=True)(fun)
        return jitted, gufun
    else:
        return jitted


def eval_ast(mod):

    context = {}

    import numpy

    context['inf'] = numpy.inf

    context['exp'] = numpy.exp
    context['log'] = numpy.log
    context['sin'] = numpy.sin
    context['cos'] = numpy.cos
    context['tan'] = numpy.tan
    context['tanh'] = numpy.tanh
    context['atan'] = numpy.arctan
    context['atanh'] = numpy.arctanh

    context['maximum'] = numpy.maximum
    context['minimum'] = numpy.minimum
    context['max'] = numpy.maximum
    context['min'] = numpy.minimum
    context['abs'] = numpy.abs

    name = mod.body[0].name
    mod = ast.fix_missing_locations(mod)
    code = compile(mod, '<string>', 'exec')
    exec(code, context, context)
    fun = context[name]

    return fun
