from __future__ import division

def eval_ast(mod):

    import ast
    import numpy
    from numexpr import evaluate

    context = {}
    context['division'] = division # THAT seems strange !
    context['inf'] = numpy.inf
    context['evaluate'] = evaluate
    context['maximum'] = numpy.maximum
    context['minimum'] = numpy.minimum
    context['exp'] = numpy.exp
    context['log'] = numpy.log
    context['sin'] = numpy.sin
    context['cos'] = numpy.cos
    context['abs'] = numpy.abs

    name = mod.body[0].name
    mod = ast.fix_missing_locations(mod)
    code  = compile(mod, '<string>', 'exec')
    exec(code, context, context)
    fun = context[name]
    return fun
