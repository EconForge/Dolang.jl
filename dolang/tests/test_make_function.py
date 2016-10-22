def test_make_method_list_of_lists():
    from dolang.parser import parse_string
    from dolang.codegen import to_source
    from dolang.function_compiler import make_method
    #style 1: list of lists
    eqs = ['x + lam*y']
    arguments = [
        [('x',1)],
        [('x',0),('y',0)]
    ]
    constants = ['lam']
    fun = make_method(eqs, arguments, constants)
    print(to_source(fun))

def test_make_method_dictionary():
    from dolang.parser import parse_string
    from dolang.codegen import to_source
    from dolang.function_compiler import make_method
    # style 2: OrderedDict
    eqs = ['x + lam*y']
    from collections import OrderedDict
    arguments = OrderedDict()
    arguments['group_1'] = [('x',1)]
    arguments['group_2'] = [('x',0),('y',0)]
    constants = ['lam']
    fun2 = make_method(eqs, arguments, constants)
    print(to_source(fun2))

# def test_make_method_higher_order():
#     # style 3: List
#     arguments = [('x',1), ('x',0), ('y',0)]
#     constants = ['lam']
#     fun3 = make_method(eqs, arguments, constants)
#     print(to_source(fun3))

def test_make_method_definitions():
    from dolang.parser import parse_string
    from dolang.codegen import to_source
    from dolang.function_compiler import make_method
    # with definitions

    from collections import OrderedDict
    eqs = ['x + lam*y + z']
    arguments = OrderedDict()
    arguments['group_1'] = [('x',1)]
    arguments['group_2'] = [('x',0),('y',0)]
    definitions = {'z': 'x+y'}
    constants = ['lam']
    fun = make_method(eqs, arguments, constants, definitions=definitions)
    print(to_source(fun))

def test_make_method_elaborate_definitions():
    from dolang.parser import parse_string
    from dolang.codegen import to_source
    from dolang.function_compiler import make_method
    # with elaborate definitions

    from collections import OrderedDict
    eqs = ['x + lam*y + z + w(1)']
    arguments = OrderedDict()
    arguments['group_1'] = [('x',1)]
    arguments['group_2'] = [('x',0),('y',0)]
    definitions = {'z': 'x', 'w': 'z+x(-1)+y(-1)'}
    constants = ['lam']
    fun = make_method(eqs, arguments, constants, definitions=definitions)
    print(to_source(fun))
#
# from dolang.symbolic import list_variables, ListSymbols
# from dolang.parser import parse_string
# expr = parse_string('z + x(-(1)) + y(-(1))')
# list_variables( expr )
#
# ln = ListSymbols(known_variables=['z'])
# ln.visit(expr)
# print(ln.variables)
#
# list_variables(expr, vars=['z'])


if __name__ == '__main__':
    import nose
    nose.run(defaultTest=__name__)
